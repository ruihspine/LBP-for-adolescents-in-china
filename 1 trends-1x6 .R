# =========================================================
# 顶刊风格：Number（上）+ Rate（下）双面板趋势图
# Measures: Incidence / Prevalence / YLDs (Years Lived with Disability)
# Locations: Global, China
# =========================================================

library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
df <- read.csv("data1.csv", header = TRUE)

# ---------- 0. 确认 measure_name ----------
print(unique(df$measure_name))
# 预期输出应包含：
# "Incidence" "Prevalence" "YLDs (Years Lived with Disability)" 等

# 注意这里用完整名称
measures <- c(
  "Incidence",
  "Prevalence",
  "YLDs (Years Lived with Disability)"
)

# ---------- 1. 通用配色 ----------
pal <- pal_lancet()(2)
cols <- c("Global" = pal[1], "China" = pal[2])
ltys <- c("Global" = "solid", "China" = "dashed")

# 轴标签映射：key 必须与 measures 中名称完全一致
ylab_number_map <- c(
  "Incidence"                          = "Incident cases",
  "Prevalence"                         = "Prevalent cases",
  "YLDs (Years Lived with Disability)" = "YLDs cases"
)
ylab_rate_map <- c(
  "Incidence"                          = "Age-standardized incidence rate (per 100,000)",
  "Prevalence"                         = "Age-standardized prevalence rate (per 100,000)",
  "YLDs (Years Lived with Disability)" = "Age-standardized YLD rate (per 100,000)"
)

# 图标题映射：把长名字换成短名字，避免标题太长
title_map <- c(
  "Incidence"                          = "Incidence",
  "Prevalence"                         = "Prevalence",
  "YLDs (Years Lived with Disability)" = "YLDs"
)

# ---------- 2. 单 measure 双面板函数 ----------
plot_measure_2panel <- function(measure, data = df) {
  
  dfx <- data %>%
    filter(
      measure_name == measure,
      location_id %in% c(1, 6),
      cause_name == "Low back pain",
      metric_name %in% c("Number", "Rate"),
      sex_name == "Both",
      age_id == 159
    )
  
  if (nrow(dfx) == 0) {
    message("跳过（无数据）：", measure)
    return(NULL)
  }
  
  dfx$location_name <- factor(dfx$location_name, levels = c("Global", "China"))
  
  df_number <- dfx %>% filter(metric_name == "Number")
  df_rate   <- dfx %>% filter(metric_name == "Rate")
  
  ylab_number <- ifelse(measure %in% names(ylab_number_map),
                        ylab_number_map[[measure]], "Number")
  ylab_rate   <- ifelse(measure %in% names(ylab_rate_map),
                        ylab_rate_map[[measure]], "Age-standardized rate (per 100,000)")
  title_lab   <- ifelse(measure %in% names(title_map),
                        title_map[[measure]], measure)
  
  x_breaks <- c(seq(1990, 2020, by = 5), 2023)
  
  # ---- 上图：Number ----
  p_num <- ggplot(df_number, aes(x = year, y = val, fill = location_name)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "grey20",
      linewidth = 0.2,
      alpha = 0.75
    ) +
    scale_fill_manual(values = cols, name = "Location") +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      name = ylab_number,
      labels = label_number(scale = 1e-6, unit = "M", accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = NULL, title = title_lab) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor.y = element_blank()
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.75, color = "grey20")))
  
  # ---- 下图：Rate ----
  p_rate <- ggplot(
    df_rate,
    aes(x = year, y = val, color = location_name, linetype = location_name)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_color_manual(values = cols, name = "Location") +
    scale_linetype_manual(values = ltys, name = "Location") +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      name = ylab_rate,
      labels = label_number(accuracy = 1, big.mark = ","),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "Year") +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor.y = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"), size = 1.2)))
  
  p_num / p_rate +
    plot_layout(heights = c(1, 1), guides = "collect") &
    theme(legend.position = "right")
}

# ---------- 3. 批量生成 ----------
plots_2panel <- lapply(measures, plot_measure_2panel)
names(plots_2panel) <- measures
plots_2panel <- plots_2panel[!sapply(plots_2panel, is.null)]

# 纵向拼图，A/B/C 标签
combined_2panel <- wrap_plots(plots_2panel, ncol = 1) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 14)
    )
  )

print(combined_2panel)

# ---------- 4. 输出 ----------
ggsave("trend_measures_2panel.pdf", combined_2panel,
       width = 8.7, height = 22, dpi = 300)
ggsave("trend_measures_2panel.tiff", combined_2panel,
       width = 8.7, height = 22, dpi = 600, compression = "lzw")