library(readxl)
library(tidyverse)
library(writexl)
library(dplyr)
library(chest)
library(scitb)
library(gtsummary)

a5  <- read_csv("~/Dementia_AF/a5.csv")
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
