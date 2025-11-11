library(tidyverse)
library(ggrepel)


df <- read_csv("Initial_Protein_Pathway_8.csv") %>%
  mutate(
    FDR = ifelse(FDR == 0, 1e-300, FDR),      
    HR = as.numeric(str_extract(HR_CI, "^[0-9.]+")),
    neglog10FDR = -log10(FDR)
  )


df_sig <- df %>% filter(FDR < 0.05)
max_HR <- df_sig %>% arrange(desc(HR)) %>% slice(1:3)
min_HR <- df_sig %>% arrange(HR) %>% slice(1:3)
to_label <- bind_rows(max_HR, min_HR) %>%
  mutate(Protein = toupper(Protein))    

df <- df %>%
  mutate(
    is_label = Protein %in% to_label$Protein,
    color = case_when(
      FDR >= 0.05 ~ "#b6bcb7",
      FDR < 0.05 & HR < 1.00 ~ "#4a739f",
      FDR < 0.05 & HR > 1.00 ~ "#9f3835"
    )
  )


hr_limits <- range(df$HR, na.rm = TRUE)
fdr_limits <- range(df$neglog10FDR, na.rm = TRUE)
buffer_hr <- diff(hr_limits) * 0.1
buffer_fdr <- diff(fdr_limits) * 0.1

p <- ggplot(df, aes(x = HR, y = neglog10FDR, color = color, size = is_label, alpha = is_label)) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  scale_size_manual(values = c("TRUE" = 2, "FALSE" = 1), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.95, "FALSE" = 0.6), guide = "none") +
  theme_classic(base_size = 15) +
  labs(
    x = "Hazard Ratio (HR)",
    y = expression(-log[10]~"FDR")
  ) +
  geom_text_repel(
    data = to_label,
    aes(x = HR, y = neglog10FDR, label = Protein),
    size = 10 / .pt,
    color = "black",
    box.padding = 0.8,
    point.padding = 0.8,
    segment.size = 0.7,
    segment.color = "grey40",
    min.segment.length = 0.1,
    max.overlaps = 50,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", linewidth = 0.7) +
  theme(
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  ) +
  xlim(hr_limits[1] - buffer_hr, hr_limits[2] + buffer_hr) +
  ylim(fdr_limits[1] - buffer_fdr, fdr_limits[2] + buffer_fdr)

print(p)
