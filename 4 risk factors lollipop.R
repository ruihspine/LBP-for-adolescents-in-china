# =========================================================
# 归因 EAPC 棒棒糖图
# =========================================================

library(tidyverse)
library(scales)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
eapc <- read.csv("data5 risk factors eapc.csv", header = TRUE)

df_eapc <- eapc %>%
  filter(measure_name == "YLDs (Years Lived with Disability)",
         cause_name == "Low back pain") %>%
  mutate(
    rei_short = recode(rei_name,
                       "High body-mass index" = "High BMI",
                       "Occupational ergonomic factors" = "Occupational ergonomic"),
    group = paste(location_name, sex_name, sep = " · ")
  )

# 手工定义 y 轴顺序（从下往上）
df_eapc <- df_eapc %>%
  mutate(
    group = factor(group, levels = c(
      "Global · Both", "Global · Female", "Global · Male",
      "China · Both",  "China · Female",  "China · Male"
    )),
    rei_short = factor(rei_short,
                       levels = c("Occupational ergonomic", "High BMI"))
  )

p <- ggplot(df_eapc, aes(x = val, y = group,
                         color = rei_short,
                         shape = rei_short)) +
  # 竖虚线：0
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  # 从 0 到 val 的横线
  geom_segment(aes(x = 0, xend = val, yend = group),
               linewidth = 0.7) +
  # 圆点
  geom_point(size = 3.8) +
  # 数值
  geom_text(aes(label = sprintf("%.2f", val)),
            hjust = -0.3, vjust = 0.4,
            size = 3.3, color = "grey20",
            show.legend = FALSE) +
  # 分面：行 = 地区，列 = 无（只一行）
  scale_color_manual(values = c("High BMI" = "#E64B35",
                                "Occupational ergonomic" = "#4DBBD5"),
                     name = "Risk factor") +
  scale_shape_manual(values = c("High BMI" = 16,
                                "Occupational ergonomic" = 17),
                     name = "Risk factor") +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.25))) +
  labs(x = "EAPC (%)", y = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", color = "grey20"),
    axis.text.x = element_text(size = 10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.x = element_blank()
  )

print(p)

ggsave("EAPC lollipop risk factors.pdf", p, width = 9, height = 4.5, dpi = 300)
ggsave("EAPC lollipop risk factors.tiff", p, width = 9, height = 4.5,
       dpi = 600, compression = "lzw")