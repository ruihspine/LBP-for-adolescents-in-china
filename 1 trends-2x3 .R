# =========================================================
# 顶刊主图布局：2行 × 3列
# 上排：Rate 折线图 (Prevalence, Incidence, YLDs)
# 下排：Number 柱状图 (Prevalence, Incidence, YLDs)
# 地区：Global, China
# =========================================================

library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
df <- read.csv("data1.csv", header = TRUE)

# ---------- 0. 统一 measure 名称 ----------
df <- df %>%
  mutate(measure_name = recode(measure_name,
                               "YLDs (Years Lived with Disability)" = "YLDs",
                               "Years lived with disability"        = "YLDs"
  ))

print(unique(df$measure_name))

# ---------- 1. 通用设置 ----------
pal  <- pal_lancet()(2)
cols <- c("Global" = pal[1], "China" = pal[2])
ltys <- c("Global" = "solid", "China" = "dashed")

ylab_number_map <- c(
  "Prevalence" = "Prevalent cases",
  "Incidence"  = "Incident cases",
  "YLDs"       = "YLDs cases"
)
ylab_rate_map <- c(
  "Prevalence" = "Age-standardized prevalence rate (per 100,000)",
  "Incidence"  = "Age-standardized incidence rate (per 100,000)",
  "YLDs"       = "Age-standardized YLD rate (per 100,000)"
)
title_map <- c(
  "Prevalence" = "Prevalence",
  "Incidence"  = "Incidence",
  "YLDs"       = "YLDs"
)

x_breaks <- c(seq(1990, 2020, by = 5), 2023)

# ---------- 2. 公共数据筛选函数 ----------
get_data <- function(measure, data = df) {
  data %>%
    filter(
      measure_name == measure,
      location_id %in% c(1, 6),
      cause_name == "Low back pain",
      metric_name %in% c("Number", "Rate"),
      sex_name == "Both",
      age_id == 159
    ) %>%
    mutate(location_name = factor(location_name, levels = c("Global", "China")))
}

# ---------- 3. Rate 折线图函数 ----------
plot_rate <- function(measure, data = df, show_x = FALSE, show_legend = FALSE) {
  dfx <- get_data(measure, data) %>% filter(metric_name == "Rate")
  if (nrow(dfx) == 0) return(NULL)
  
  ylab <- ylab_rate_map[[measure]]
  title_lab <- title_map[[measure]]
  
  p <- ggplot(dfx, aes(x = year, y = val,
                       color = location_name,
                       linetype = location_name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.6) +
    scale_color_manual(values = cols, name = "Location") +
    scale_linetype_manual(values = ltys, name = "Location") +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      name = ylab,
      labels = label_number(accuracy = 1, big.mark = ","),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(x = if (show_x) "Year" else NULL,
         title = title_lab) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = if (show_x) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(4, 6, 2, 4)
    )
  
  if (show_legend) {
    p <- p + guides(color = guide_legend(
      override.aes = list(linetype = c("solid", "dashed"), size = 1.2)
    ))
  }
  p
}

# ---------- 4. Number 柱状图函数 ----------
plot_number <- function(measure, data = df, show_x = TRUE, show_legend = FALSE) {
  dfx <- get_data(measure, data) %>% filter(metric_name == "Number")
  if (nrow(dfx) == 0) return(NULL)
  
  ylab <- ylab_number_map[[measure]]
  
  p <- ggplot(dfx, aes(x = year, y = val, fill = location_name)) +
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
      name = ylab,
      labels = label_number(scale = 1e-6, unit = "M", accuracy = 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(x = if (show_x) "Year" else NULL) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = if (show_x) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(4, 6, 2, 4)
    )
  
  if (show_legend) {
    p <- p + guides(fill = guide_legend(
      override.aes = list(alpha = 0.75, color = "grey20")
    ))
  }
  p
}

# ---------- 5. 生成 6 个面板 ----------
# 上排：Rate
p_rate_prev <- plot_rate("Prevalence", show_x = FALSE, show_legend = FALSE)
p_rate_inc  <- plot_rate("Incidence",  show_x = FALSE, show_legend = FALSE)
p_rate_yld  <- plot_rate("YLDs",       show_x = FALSE, show_legend = TRUE)

# 下排：Number
p_num_prev  <- plot_number("Prevalence", show_x = TRUE, show_legend = FALSE)
p_num_inc   <- plot_number("Incidence",  show_x = TRUE, show_legend = FALSE)
p_num_yld   <- plot_number("YLDs",       show_x = TRUE, show_legend = TRUE)

# ---------- 6. 组合 2×3 布局 ----------
top_row    <- p_rate_prev | p_rate_inc | p_rate_yld
bottom_row <- p_num_prev  | p_num_inc  | p_num_yld

final_plot <- top_row / bottom_row +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 14)
    )
  )

print(final_plot)

# ---------- 7. 输出 ----------
ggsave("Fig_main_2x3.pdf", final_plot,
       width = 12, height = 7.5, dpi = 300)
ggsave("Fig_main_2x3.tiff", final_plot,
       width = 12, height = 7.5, dpi = 600, compression = "lzw")