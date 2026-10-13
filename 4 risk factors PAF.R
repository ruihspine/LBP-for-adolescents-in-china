# =========================================================
# PAF 堆叠柱状图（每年一根小立柱）
# =========================================================

library(tidyverse)
library(scales)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
risk <- read.csv("data4 risk factors.csv", header = TRUE)

df_risk <- risk %>%
  filter(sex_name == "Both",
         measure_name == "YLDs (Years Lived with Disability)",
         cause_name == "Low back pain",
         metric_name == "Rate") %>%
  select(location_name, year, rei_name, val)

df_total <- read.csv("data1.csv", header = TRUE) %>%
  filter(measure_name == "YLDs (Years Lived with Disability)",
         sex_name == "Both",
         cause_name == "Low back pain",
         metric_name == "Rate",
         age_id == 159) %>%
  select(location_name, year, total = val)

df_paf <- df_risk %>%
  left_join(df_total, by = c("location_name", "year")) %>%
  mutate(
    paf = val / total,
    rei_short = recode(rei_name,
                       "High body-mass index" = "High BMI",
                       "Occupational ergonomic factors" = "Occupational ergonomic"),
    rei_short = factor(rei_short,
                       levels = c("Occupational ergonomic", "High BMI")),
    location_name = factor(location_name, levels = c("China", "Global"))
  )

p <- ggplot(df_paf, aes(x = year, y = paf, fill = rei_short)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.15) +
  facet_wrap(~ location_name, ncol = 2) +
  scale_fill_manual(values = c("High BMI" = "#E64B35",
                               "Occupational ergonomic" = "#4DBBD5"),
                    name = "Risk factor") +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Attributable fraction of YLDs rate") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  )

print(p)

ggsave("PAF attributable fraction.pdf", p, width = 10, height = 5, dpi = 300)
ggsave("PAF attributable fraction.tiff", p, width = 10, height = 5,
       dpi = 600, compression = "lzw")