library(readxl)
library(tidyverse)
library(writexl)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(RColorBrewer)
library(rcssci)
library(mediation)
library(cutpointr)
library(jstable)
library(broom)
library(chest)
library(scitb)
library(forestploter)
library(grid)
library(tidyLPA)
library(mclust)
library(MplusAutomation)
library(haven)
library(questionr)
library(xlsx)
library(openxlsx)
library(readr)
library(lubridate)
library(mice)
library(missForest)
library(DMwR2)
library(simputation)
library(survival)
library(gtsummary)
library(flextable)
library(stats)
library(rstpm2)
library(survival)
library(referenceIntervals)
library(flexsurv)
library(lmtest)
library(mstate)
library(missForest)
library(missRanger)#随机森林插补数据
library(VIM)
set.seed(20250512)
#一、上传数据#########################################################################
Protein <- read_csv("~/AF/Protein/Protein_datafinal.csv")
Lifestyle_total1 <- read_csv("~/Lifestyle/Lifestyle_total1.csv")
AF_antioxidantsdata <- read_csv("~/AF/AF_antioxidantsdata.csv")

AFdata <- AF_antioxidantsdata %>% dplyr::select(ID, Age, Sex, TDI, Date_lost, Date_last,
                                                region, Date_attending, Pulse, CRP, 
                                                Glucose, Cystatin_C, T2DM, HTN, AF, 
                                                Dementia, Date_death, WC, Height, HDL, 
                                                TG, LDL, TC, Anti_DM_HTN1)
AFdata <- AFdata %>% rename(Glu = Glucose, date_death = Date_death)

Lifestyledata <- Lifestyle_total1 %>% dplyr::select(ID,
                                                    Smoking_status,
                                                    Drink_status)

a1 <- Protein %>% left_join(Lifestyledata, by = "ID") %>%
   left_join(AFdata, by = "ID")

# 二、清洗数据#####################################################
# 1. 参与者筛选####################################################
a1$AF <- as.Date(a1$AF, format = "%Y-%m-%d")
a1$Dementia <- as.Date(a1$Dementia, format = "%Y-%m-%d")
a1$Date_lost <- as.Date(a1$Date_lost, format = "%Y-%m-%d")
a1$Date_last <- as.Date(a1$Date_last, format = "%Y-%m-%d")
a1$Date_attending <- as.Date(a1$Date_attending, format = "%Y-%m-%d")
a1$date_death <- as.Date (a1$date_death, format = "%Y-%m-%d")

a1 <- a1 %>% filter(is.na(AF) | is.na(Date_attending) | AF > Date_attending) # 53013 - 1156 = 51857
a1 <- a1 %>% filter(is.na(Dementia) | is.na(Date_attending) | Dementia > Date_attending) # 51857 - 45 = 51812

a1 <- a1 %>%
   mutate(cutoff_date = case_when(
      region == "Scotland" ~ as.Date("2022-08-31"),
      region == "England" ~ as.Date("2022-12-31"),
      region == "Wales" ~ as.Date("2022-05-31"))) %>%
   filter(is.na(AF) | AF <= cutoff_date) %>%
   select(-cutoff_date) 

a1 <- a1 %>%
   mutate(cutoff_date = case_when(
      region == "Scotland" ~ as.Date("2022-08-31"),
      region == "England" ~ as.Date("2022-12-31"),
      region == "Wales" ~ as.Date("2022-05-31"))) %>%
   filter(is.na(Dementia) | Dementia <= cutoff_date) %>%
   select(-cutoff_date) # 51812 - 5  = 51807
# 2.随访时间#####################################################
# (1).AF
a1 <- a1 %>% mutate(AF_follow = case_when(
   !is.na(AF) ~ AF, 
   is.na(AF) & Date_lost > Date_attending & Date_lost > Date_last ~ Date_lost, 
   region == "Scotland" & (!is.na(date_death) & date_death < as.Date("2022-08-31")) ~ date_death,  
   region == "Scotland" ~ as.Date("2022-08-31"),  
   region == "England" & (!is.na(date_death) & date_death < as.Date("2022-12-31")) ~ date_death,
   region == "England" ~ as.Date("2022-12-31"), 
   region == "Wales" & (!is.na(date_death) & date_death < as.Date("2022-05-31")) ~ date_death,  
   region == "Wales" ~ as.Date("2022-05-31")))

a1 <- a1 %>% mutate(AF_time_days = as.numeric(AF_follow - Date_attending))
a1 <- a1 %>% mutate(AF_time_months = as.numeric(round(interval(Date_attending, AF_follow) / months(1), 5)))
# (2).Dementia
a1 <- a1 %>% mutate(Dementia_follow = case_when(
   !is.na(Dementia) ~ Dementia,
   is.na(Dementia) & Date_lost > Date_attending & Date_lost > Date_last ~ Date_lost,
   region == "Scotland" & (!is.na(date_death) & date_death < as.Date("2022-08-31")) ~ date_death,  
   region == "Scotland" ~ as.Date("2022-08-31"),  
   region == "England" & (!is.na(date_death) & date_death < as.Date("2022-12-31")) ~ date_death,
   region == "England" ~ as.Date("2022-12-31"), 
   region == "Wales" & (!is.na(date_death) & date_death < as.Date("2022-05-31")) ~ date_death,  
   region == "Wales" ~ as.Date("2022-05-31")))

a1 <- a1 %>% mutate(Dementia_time_days = as.numeric(Dementia_follow - Date_attending))
a1 <- a1 %>% mutate(Dementia_time_months = as.numeric(round(interval(Date_attending, Dementia_follow) / months(1), 5)))
# (3).death
a1 <- a1 %>% mutate(Death_follow = case_when(
   !is.na(date_death) ~ date_death,
   is.na(date_death) & Date_lost > Date_attending & Date_lost > Date_last ~ Date_lost,
   region == "Scotland" & (!is.na(date_death) & date_death < as.Date("2022-08-31")) ~ date_death,  
   region == "Scotland" ~ as.Date("2022-08-31"), 
   region == "England" & (!is.na(date_death) & date_death < as.Date("2022-12-31")) ~ date_death, 
   region == "England" ~ as.Date("2022-12-31"), 
   region == "Wales" & (!is.na(date_death) & date_death < as.Date("2022-05-31")) ~ date_death, 
   region == "Wales" ~ as.Date("2022-05-31")))

a1 <- a1 %>% mutate(Death_time_days = as.numeric(Death_follow - Date_attending))
a1 <- a1 %>% mutate(Death_time_months = as.numeric(round(interval(Date_attending, Death_follow) / months(1), 5)))
# (4).AF_Dementia
a1 <- a1 %>% mutate(AF_Dementia = case_when(
   AF_time_days > Dementia_time_days ~ AF_time_days, 
   AF_time_days < Dementia_time_days ~ Dementia_time_days, 
   TRUE ~ AF_time_days))

# 3.数据重编码#####################################################
a2 <- a1 %>%
   select(ID, 2:2924, Date_attending, region, Age, Sex, TDI, Pulse,
          CRP, Glu, Cystatin_C, T2DM, HTN, AF, Dementia, AF_Dementia,
          date_death, HDL, TG, LDL, TC, Anti_DM_HTN1, Smoking_status,
          Drink_status,AF_time_days, Dementia_time_days, Death_time_days,
          AF_time_months, Dementia_time_months, Death_time_months)

# (1).Age
a2 <- a2 %>% mutate(Age_group = case_when(
   Age >= 60~ "Yes",
   Age <  60 ~ "No"))

# (2).anti_HT_T2DM_HY
a2 %>% count(Anti_DM_HTN1)
# Anti_HTN
a2 <- a2 %>% mutate(Anti_HTN =case_when(
   Anti_DM_HTN1 == "Blood pressure medication" | Anti_DM_HTN1 == "Blood pressure medication|Insulin" |
      Anti_DM_HTN1 == "Cholesterol lowering medication|Blood pressure medication"|
      Anti_DM_HTN1 == "Cholesterol lowering medication|Blood pressure medication|Insulin"~ "Yes",
   TRUE ~ "No"))
# Anti_DM
a2 <- a2 %>% mutate(Anti_DM =case_when(
   Anti_DM_HTN1 == "Blood pressure medication|Insulin" | Anti_DM_HTN1 == "Insulin" |
      Anti_DM_HTN1 == "Cholesterol lowering medication|Insulin"|
      Anti_DM_HTN1 == "Cholesterol lowering medication|Blood pressure medication|Insulin"~ "Yes",
   TRUE ~ "No"))
# Anti_HY
a2 <- a2 %>% mutate(Anti_HY =case_when(
   Anti_DM_HTN1 == "Cholesterol lowering medication" | Anti_DM_HTN1 == "Cholesterol lowering medication|Blood pressure medication" |
      Anti_DM_HTN1 == "Cholesterol lowering medication|Blood pressure medication|Insulin"|
      Anti_DM_HTN1 == "Cholesterol lowering medication|Insulin" ~ "Yes",
   TRUE ~ "No"))

# (3).HTN
a2$HTN <- as.Date (a2$HTN, format = "%Y-%m-%d")
a2 <- a2 %>%
   mutate(HTN_status = case_when(
      !is.na(HTN) & HTN <= Date_attending ~ "Yes",
      !is.na(HTN) & HTN > Date_attending ~ "No",
      TRUE ~ "No"))

# (4).T2DM
a2$T2DM <- as.Date (a2$T2DM, format = "%Y-%m-%d")
a2 <- a2 %>%
   mutate(T2DM_status = case_when(
      !is.na(T2DM) & T2DM <= Date_attending ~ "Yes",
      !is.na(T2DM) & T2DM > Date_attending ~ "No",
      TRUE ~ "No"))
# (5).outcome
# AF
a2 <- a2 %>%
   mutate(
      AF_status_c = case_when(
         !is.na(AF) ~ "Yes",
         TRUE ~ "No"))
a2 <- a2 %>%
   mutate(AF_status = ifelse(AF_status_c == "Yes", 1, 0))

# Dementia
a2 <- a2 %>%
   mutate(
      Dementia_status_c = case_when(
         !is.na(Dementia) ~ "Yes",
         TRUE ~ "No"))
a2 <- a2 %>%
   mutate(Dementia_status = ifelse(Dementia_status_c == "Yes", 1, 0))

# Death
a2 <- a2 %>%
   mutate(
      Death_status_c = case_when(
         !is.na(date_death) ~ "Yes",
         TRUE ~ "No"))
a2 <- a2 %>%
   mutate(Death_status = ifelse(Death_status_c == "Yes", 1, 0))

# AF_Dementia
a2 <- a2 %>%
   mutate(
      AF_Dementia_status_c = case_when(
         !is.na(AF) & !is.na(Dementia) ~ "Yes",
         TRUE ~ "No"))
a2 <- a2 %>%
   mutate(AF_Dementia_status = ifelse(AF_Dementia_status_c == "Yes", 1, 0))


# (6)数据因子化
a3 <- a2 %>% dplyr::select(ID, 2:2924, Date_attending, region, Age, Age_group, Sex, TDI, Pulse,
                           CRP, Glu, Cystatin_C, T2DM_status, HTN_status, AF, Dementia, AF_Dementia,
                           date_death, HDL, TG, LDL, TC, Anti_HTN,Anti_DM,Anti_HY, Smoking_status,
                           Drink_status, AF_time_days, Dementia_time_days, Death_time_days,
                           AF_time_months, Dementia_time_months, Death_time_months,
                           AF_status_c, AF_status, Dementia_status_c , Dementia_status,
                           AF_Dementia_status_c, AF_Dementia_status, Death_status_c,
                           Death_status)

a3 <- a3 %>%
   mutate(
      Age_group = factor(Age_group, levels = c("Yes", "No")), 
      Sex = factor(Sex, levels = c("Female", "Male")), 
      T2DM_status = factor(T2DM_status, levels = c("Yes", "No")), 
      HTN_status = factor(HTN_status, levels = c("Yes", "No")),
      Anti_HTN = factor(Anti_HTN, levels = c("No", "Yes")),
      Anti_DM = factor(Anti_DM, levels = c("No", "Yes")),
      Anti_HY = factor(Anti_HY, levels = c("No", "Yes")),
      Smoking_status = factor(Smoking_status, levels = c("Smokers", "Non_smokers")),
      Drink_status = factor(Drink_status, levels = c("Non_Moderate", "Moderate")),
      AF_status_c = factor(AF_status_c, levels = c("Yes","No")),
      Dementia_status_c = factor(Dementia_status_c, levels = c("Yes","No")),
      Death_status_c = factor(Death_status_c, levels = c("Yes","No")),
      AF_Dementia_status_c = factor(AF_Dementia_status_c, levels = c("Yes","No")))

# 4.插补缺失值######################################################
a4 <- as.data.frame(a3)
a5 <- missRanger(
   data = a4,
   num.trees = 10,          # 树的数量（可根据需要减少到50提升速度）
   maxiter = 5,              # 最大迭代次数，默认5，可调整
   verbose = 1,              # 显示进度
   seed = 20250512,          # 保证可重复性
   num.threads = parallel::detectCores() - 1  # 自动使用多核心
)
write_csv(a5, "a5.csv")

a5  <- read_csv("~/Dementia_AF/a5.csv")
a5 <- a5 %>%
   mutate(
      Age_group = factor(Age_group, levels = c("Yes", "No")), 
      Sex = factor(Sex, levels = c("Female", "Male")), 
      T2DM_status = factor(T2DM_status, levels = c("Yes", "No")), 
      HTN_status = factor(HTN_status, levels = c("Yes", "No")),
      Anti_HTN = factor(Anti_HTN, levels = c("No", "Yes")),
      Anti_DM = factor(Anti_DM, levels = c("No", "Yes")),
      Anti_HY = factor(Anti_HY, levels = c("No", "Yes")),
      Smoking_status = factor(Smoking_status, levels = c("Smokers", "Non_smokers")),
      Drink_status = factor(Drink_status, levels = c("Non_Moderate", "Moderate")),
      AF_status_c = factor(AF_status_c, levels = c("Yes","No")),
      Dementia_status_c = factor(Dementia_status_c, levels = c("Yes","No")),
      Death_status_c = factor(Death_status_c, levels = c("Yes","No")),
      AF_Dementia_status_c = factor(AF_Dementia_status_c, levels = c("Yes","No")))

# 三、Table 1########################################################
library(scitb)
allVars <-c("Age", "Sex", "TDI", "Pulse", "CRP",
            "Glu","HDL","TG","LDL","TC","T2DM_status","HTN_status",
            "Dementia_status_c","Death_status_c","AF_Dementia_status_c")
fvars<-c("Sex", "HTN_status","T2DM_status","AF_status_c","Dementia_status_c","Death_status_c","AF_Dementia_status_c")
strata<-"AF_status_c"

Table1 <-scitb1(vars=allVars,fvars=fvars,strata=strata,
                data=a5,atotest=T,statistic=TRUE,Overall=TRUE,smd=TRUE)
Table1 <- as.data.frame(Table1)
write_xlsx(Table1, "Table1.xlsx")

#四、各个状态的人数汇总######################################
# 循环遍历每个变量，生成分组统计表
result <- a5 %>%
   summarise(
      AF_status_c_yes = sum(AF_status_c == "Yes"),
      AF_status_c_no = sum(AF_status_c == "No"),
      Dementia_status_c_yes = sum(Dementia_status_c == "Yes"),
      Dementia_status_c_no = sum(Dementia_status_c == "No"),
      Death_status_c_yes = sum(Death_status_c == "Yes"),
      Death_status_c_no = sum(Death_status_c == "No"),
      AF_Dementia_status_c_yes = sum(AF_Dementia_status_c == "Yes"),
      AF_Dementia_status_c_no = sum(AF_Dementia_status_c == "No"))

# 合并所有结果数据框
# 将结果输出到Excel文件
write_xlsx(result, "FinalTable.xlsx")

# 五、随访时间#######################################################
library(survival)
library(survminer)
# 中位随访时间: 166 months
AF_MT <- survfit(Surv(time = a5$AF_time_months, event = a5$AF_status == 0) ~ 1, data = a5)
AF_MT
# Call: survfit(formula = Surv(time = a5$AF_time_months, event = a5$AF_status == 
#                                0) ~ 1, data = a5)
# 
# n events median 0.95LCL 0.95UCL
# [1,] 51807  47969    166     166     166

AF_MT <- survfit(Surv(time = a5$Dementia_time_months, event = a5$Dementia_status == 0) ~ 1, data = a5)
AF_MT
# Call: survfit(formula = Surv(time = a5$Dementia_time_months, event = a5$AF_status == 
#                                0) ~ 1, data = a5)
# 
# n events median 0.95LCL 0.95UCL
# [1,] 51807  47969    166     166     166

AF_MT <- survfit(Surv(time = a5$Death_time_months, event = a5$Death_status == 0) ~ 1, data = a5)
AF_MT
# Call: survfit(formula = Surv(time = a5$Death_time_months, event = a5$Death_status == 
#                                0) ~ 1, data = a5)
# 
# n events median 0.95LCL 0.95UCL
# [1,] 51807  46436    167     167     167

sum(a5$AF_time_months) # 8064117
sum(a5$Dementia_time_months) # 8243700
sum(a5$Death_time_months) # 8291607

# 六、多状态模型########################################################
tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

msebmt_aamdc <- msprep(
   time = c(NA,"AF_time_days","Dementia_time_days","AF_Dementia","Death_time_days"),
   status = c(NA, "AF_status","Dementia_status","AF_Dementia_status","Death_status"), 
   data = a5, 
   trans = tmat, 
   keep = c("aamdc","Age","Sex","TDI","Pulse","CRP",
            "Glu","HDL","TG","LDL","TC","T2DM_status","HTN_status")) 

msebmt1_aamdc <- msebmt_aamdc %>%
   mutate(
      Tstart = case_when(
         Tstart == 0 & Tstop == 0 ~ 0,
         Tstart == Tstop ~ Tstop - 0.5,
         TRUE ~ Tstart
      )) %>%
   structure(trans = attr(msebmt_aamdc, "trans"), class = class(msebmt_aamdc))

# 转移概率估计
events(msebmt1_aamdc)
# $Frequencies
#              to
# from          Baseline    AF Dementia AF_Dementia Death no event total entering
#   Baseline           0  3763     1186           0  3662    43196          51807
#   AF                 0     0        0         210   916     2637           3763
#   Dementia           0     0        0          75   619      492           1186
#   AF_Dementia        0     0        0           0   174      111            285
#   Death              0     0        0           0     0     5371           5371
# 
# $Proportions
#              to
# from            Baseline         AF   Dementia AF_Dementia      Death   no event
#   Baseline    0.00000000 0.07263497 0.02289266  0.00000000 0.07068543 0.83378694
#   AF          0.00000000 0.00000000 0.00000000  0.05580654 0.24342280 0.70077066
#   Dementia    0.00000000 0.00000000 0.00000000  0.06323777 0.52192243 0.41483980
#   AF_Dementia 0.00000000 0.00000000 0.00000000  0.00000000 0.61052632 0.38947368
#   Death       0.00000000 0.00000000 0.00000000  0.00000000 0.00000000 1.00000000

covs <- c("aamdc","Age","Sex","TDI","Pulse","CRP",
          "Glu","HDL","TG","LDL","TC","T2DM_status","HTN_status")
msebmt2_aamdc <- expand.covs(msebmt1_aamdc, covs = covs, longnames = FALSE)

P10_aamdc <- coxph(Surv(Tstart, Tstop, status) ~ 
                      aamdc.1+aamdc.2+aamdc.3+aamdc.4+aamdc.5+aamdc.6+aamdc.7+aamdc.8+
                      Age.1+Age.2+Age.3+Age.4+Age.5+Age.6+Age.7+Age.8+
                      Sex.1+Sex.2+Sex.3+Sex.4+Sex.5+Sex.6+Sex.7+Sex.8+
                      TDI.1+TDI.2+TDI.3+TDI.4+TDI.5+TDI.6+TDI.7+TDI.8+
                      Pulse.1+Pulse.2+Pulse.3+Pulse.4+Pulse.5+Pulse.6+Pulse.7+Pulse.8+
                      CRP.1+CRP.2+CRP.3+CRP.4+CRP.5+CRP.6+CRP.7+CRP.8+
                      Glu.1+Glu.2+Glu.3+Glu.4+Glu.5+Glu.6+Glu.7+Glu.8+
                      HDL.1+HDL.2+HDL.3+HDL.4+HDL.5+HDL.6+HDL.7+HDL.8+
                      TG.1+TG.2+TG.3+TG.4+TG.5+TG.6+TG.7+TG.8+
                      LDL.1+LDL.2+LDL.3+LDL.4+LDL.5+LDL.6+LDL.7+LDL.8+
                      TC.1+TC.2+TC.3+TC.4+TC.5+TC.6+TC.7+TC.8+
                      T2DM_status.1+T2DM_status.2+T2DM_status.3+T2DM_status.4+
                      T2DM_status.5+T2DM_status.6+T2DM_status.7+T2DM_status.8+HTN_status.1+HTN_status.2+     
                      HTN_status.3+HTN_status.4+HTN_status.5+HTN_status.6+HTN_status.7+HTN_status.8+strata(trans),
                   data = msebmt2_aamdc, method="breslow")
P10_Results_aamdc <- tidy(P10_aamdc, exponentiate = TRUE, conf.int = TRUE)
write.csv(P10_Results_aamdc, file = "P10_Results_aamdc1111.csv", row.names = FALSE)


# 多状态模型循环################################################
tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

protein_vars <- names(a5)[2:2924]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL","TG","LDL","TC","T2DM_status","HTN_status")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a5, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(
         Tstart = case_when(
            Tstart == 0 & Tstop == 0 ~ 0,
            Tstart == Tstop ~ Tstop - 0.5,
            TRUE ~ Tstart
         )
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}

# 七、敏感性分析##############################################
## （一）、排除前两年发生时间的参与者############################
a6 <- a5 %>% filter(!(AF_status == 1 & AF_time_days <= 730))
a6 <- a6 %>% filter(!(Dementia_status == 1 & Dementia_time_days <= 730))
a6 <- a6 %>% filter(!(Death_status == 1 & Death_time_days <= 730)) # 51807 - 668 = 51139

# 发生病例详细数据
result <- a6 %>%
   summarise(
      AF_status_c_yes = sum(AF_status_c == "Yes"),
      AF_status_c_no = sum(AF_status_c == "No"),
      Dementia_status_c_yes = sum(Dementia_status_c == "Yes"),
      Dementia_status_c_no = sum(Dementia_status_c == "No"),
      Death_status_c_yes = sum(Death_status_c == "Yes"),
      Death_status_c_no = sum(Death_status_c == "No"),
      AF_Dementia_status_c_yes = sum(AF_Dementia_status_c == "Yes"),
      AF_Dementia_status_c_no = sum(AF_Dementia_status_c == "No"))

# 合并所有结果数据框
# 将结果输出到Excel文件
write_xlsx(result, "FinalTableEX2Y.xlsx")

#导入蛋白数据
Protein_initial <- read_csv("~/Dementia_AF/protein_initial/Protein_initial_remain1.csv")

tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

Protein_initial <- as.data.frame(Protein_initial)
rownames(Protein_initial) <- Protein_initial$term
protein_vars <- rownames(Protein_initial)[1:440]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL",
                "TG","LDL","TC","T2DM_status","HTN_status")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a6, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(
         Tstart = case_when(
            Tstart == 0 & Tstop == 0 ~ 0,
            Tstart == Tstop ~ Tstop - 0.5,
            TRUE ~ Tstart
         )
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}
## （二）、排除肿瘤患者##################################################
Date_cancer <- read_csv("~/AF/Data_cancer.csv")
Date_cancer <- Date_cancer %>% rename(ID = eid, cancer0 = p40005_i0, cancer1 = p40005_i1, cancer2 = p40005_i2, cancer3 = p40005_i3,
                                      cancer4 = p40005_i4, cancer5 = p40005_i5, cancer6 = p40005_i6, cancer7 = p40005_i7,
                                      cancer8 = p40005_i8, cancer9 = p40005_i9, cancer10 = p40005_i10, cancer11 = p40005_i11,
                                      cancer12 = p40005_i12, cancer13 = p40005_i13, cancer14 = p40005_i14, cancer15 = p40005_i15,
                                      cancer16 = p40005_i16, cancer17 = p40005_i17, cancer18 = p40005_i18, cancer19 = p40005_i19,
                                      cancer20 = p40005_i20, cancer21 = p40005_i21)
a7 <- left_join(a5, Date_cancer, by = "ID")
a7[grep("^cancer", names(a7))] <- lapply(a7[grep("^cancer", names(a7))], as.Date)
a7 <- a7 %>%
   mutate(cancer_status = case_when(
      rowSums(across(starts_with("cancer"), ~ !is.na(.) & . <= Date_attending), na.rm = TRUE) > 0 ~ "Yes",
      TRUE ~ "No"))
a7 <- a7 %>% filter(cancer_status == "No") # 51807 -  4840 = 46967

# 发生病例详细数据
result <- a7 %>%
   summarise(
      AF_status_c_yes = sum(AF_status_c == "Yes"),
      AF_status_c_no = sum(AF_status_c == "No"),
      Dementia_status_c_yes = sum(Dementia_status_c == "Yes"),
      Dementia_status_c_no = sum(Dementia_status_c == "No"),
      Death_status_c_yes = sum(Death_status_c == "Yes"),
      Death_status_c_no = sum(Death_status_c == "No"),
      AF_Dementia_status_c_yes = sum(AF_Dementia_status_c == "Yes"),
      AF_Dementia_status_c_no = sum(AF_Dementia_status_c == "No"))

# 合并所有结果数据框
# 将结果输出到Excel文件
write_xlsx(result, "FinalTableEXcan.xlsx")

#导入蛋白数据
Protein_initial <- read_csv("~/Dementia_AF/protein_initial/Protein_remain1.csv")

tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

Protein_initial <- as.data.frame(Protein_initial)
rownames(Protein_initial) <- Protein_initial$term
protein_vars <- rownames(Protein_initial)[1551:1766]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL","TG","LDL","TC","T2DM_status","HTN_status")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a7, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(
         Tstart = case_when(
            Tstart == 0 & Tstop == 0 ~ 0,
            Tstart == Tstop ~ Tstop - 0.5,
            TRUE ~ Tstart
         )
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}

## (三）、增加Anti_DM、Anti_HTN、Anti_HY###############################################
setwd("~/Dementia_AF/Protein_add")
a8 <- a5

Protein_initial <- read_csv("~/Dementia_AF/protein_initial/Protein_remain1.csv")

tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

Protein_initial <- as.data.frame(Protein_initial)
rownames(Protein_initial) <- Protein_initial$term
protein_vars <- rownames(Protein_initial)[1765:1766]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL",
                "TG","LDL","TC","T2DM_status","HTN_status",
                "Anti_HTN", "Anti_DM", "Anti_HY")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a8, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(
         Tstart = case_when(
            Tstart == 0 & Tstop == 0 ~ 0,
            Tstart == Tstop ~ Tstop - 0.5,
            TRUE ~ Tstart
         )
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}

## (四)、排除同一天进入多个状态的情况(多状态模型部分)####################################################################
a9 <- a5

Protein_initial <- read_csv("~/Dementia_AF/protein_initial/Protein_remain1.csv")

tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

Protein_initial <- as.data.frame(Protein_initial)
rownames(Protein_initial) <- Protein_initial$term
protein_vars <- rownames(Protein_initial)[1376:1766]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL",
                "TG","LDL","TC","T2DM_status","HTN_status")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a9, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(!(Tstart == 0 & Tstop == 0) & Tstart != Tstop
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}

#(六)、使用不同的时间间隔（0.5、1、3和5年）计算进入同一状态的进入日期(多状态模型部分)################################
#0.5年##############################################################################
a10 <- a5

Protein_initial <- read_csv("~/Dementia_AF/protein_initial/Protein_remain1.csv")

tmat <- transMat(x= list(
   c(2, 3, 5), # 初始状态可转至AF(2)、Dementia(3)、死亡(5) 
   c(4, 5),  # AF状态可转至AF_Dementia(4)、Death(5)
   c(4, 5),  # Dementia状态可转至AF_Dementia(4)、Death(5)
   c(5),     # AF_Dementia状态只能转至Death(5)
   c()),     # 死亡为吸收状态
   names = c("Baseline","AF","Dementia","AF_Dementia","Death"))

Protein_initial <- as.data.frame(Protein_initial)
rownames(Protein_initial) <- Protein_initial$term
protein_vars <- rownames(Protein_initial)[1585:1766]
covariates <- c("Age","Sex","TDI","Pulse","CRP","Glu","HDL",
                "TG","LDL","TC","T2DM_status","HTN_status")
for (protein in protein_vars) {
   cat("Processing:", protein, "\n")
   
   # 构建msprep数据
   msebmt_tmp <- msprep(
      time = c(NA, "AF_time_days", "Dementia_time_days", "AF_Dementia", "Death_time_days"),
      status = c(NA, "AF_status", "Dementia_status", "AF_Dementia_status", "Death_status"), 
      data = a10, 
      trans = tmat, 
      keep = c(protein, covariates)
   )
   
   # 调整Tstart时间
   msebmt_tmp <- msebmt_tmp %>%
      mutate(Tstart = case_when(
         Tstart == 0 & Tstop == 0 ~ 0,
         Tstart == Tstop ~ Tstop - 182,
         TRUE ~ Tstart)
      ) %>%
      structure(trans = attr(msebmt_tmp, "trans"), class = class(msebmt_tmp))
   
   # 展开协变量（包括蛋白变量）
   msebmt_expanded <- expand.covs(msebmt_tmp, covs = c(protein, covariates), longnames = FALSE)
   
   # 构建公式字符串
   transitions <- 1:8
   terms <- unlist(lapply(c(protein, covariates), function(var) paste0(var, ".", transitions)))
   formula_str <- paste("Surv(Tstart, Tstop, status) ~", paste(terms, collapse = "+"), "+ strata(trans)")
   model_formula <- as.formula(formula_str)
   
   # 拟合Cox模型
   model <- coxph(model_formula, data = msebmt_expanded, method = "breslow")
   
   # 提取并导出结果
   model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
   write.csv(model_results, file = paste0("P10_Results_", protein, ".csv"), row.names = FALSE)
}


dx-backup-folder -d/MI_AF/202506261529.tar.gz --exclude .Renviron