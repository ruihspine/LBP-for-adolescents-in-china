# =========================================================
# 预测图 + inset（上 Global 下 China）
# 性别图例修正：Both 正常显示，不再是 NA
# =========================================================

library(tidyverse)
library(forecast)
library(scales)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
risk <- read.csv("data4 risk factors.csv", header = TRUE)

# ---------- 1. 历史数据 ----------
df_hist <- risk %>%
  filter(
    measure_name == "YLDs (Years Lived with Disability)",
    cause_name == "Low back pain",
    metric_name == "Rate"
  ) %>%
  select(location_name, sex_name, rei_name, year, val) %>%
  arrange(location_name, sex_name, rei_name, year)

# ---------- 2. 预测函数 ----------
forecast_risk <- function(df, h = 17) {
  df <- df %>% arrange(year)
  ts_data <- ts(df$val, start = min(df$year), frequency = 1)
  fit <- auto.arima(ts_data)
  fc <- forecast(fit, h = h)
  data.frame(
    year  = (max(df$year) + 1):(max(df$year) + h),
    fit   = as.numeric(fc$mean),
    lower = as.numeric(fc$lower[, 2]),
    upper = as.numeric(fc$upper[, 2])
  )
}

# ---------- 3. 批量预测 ----------
preds <- df_hist %>%
  group_by(location_name, sex_name, rei_name) %>%
  group_modify(~ forecast_risk(.x, h = 17)) %>%
  ungroup()

# ---------- 4. 拼接历史 + 预测 ----------
df_hist2 <- df_hist %>% rename(fit = val) %>% mutate(lower = NA, upper = NA)
df_all <- bind_rows(df_hist2, preds) %>%
  mutate(
    location_name = factor(location_name, levels = c("Global", "China")),
    rei_name = factor(rei_name,
                      levels = c("High body-mass index",
                                 "Occupational ergonomic factors")),
    # ---- 修正：加上 Both ----
    sex_name = factor(sex_name, levels = c("Male", "Female", "Both"))
  )

# ---------- 5. 主图 ----------
p_main <- ggplot(df_all, aes(x = year, y = fit, color = sex_name, fill = sex_name)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(data = df_all %>% filter(!is.na(lower)),
              aes(ymin = lower, ymax = upper),
              alpha = 0.15, color = NA) +
  geom_vline(xintercept = 2023.5, linetype = "dotted",
             color = "grey50", linewidth = 0.4) +
  facet_grid(location_name ~ rei_name) +
  # ---- 修正：加上 Both 的颜色 ----
scale_color_manual(values = c("Male"   = "#E64B35",
                              "Female" = "#4DBBD5",
                              "Both"   = "#6B6B6B"),
                   name = "Sex") +
  scale_fill_manual(values = c("Male"   = "#E64B35",
                               "Female" = "#4DBBD5",
                               "Both"   = "#6B6B6B"),
                    name = "Sex") +
  scale_x_continuous(breaks = seq(1990, 2040, by = 10),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                     expand = expansion(mult = c(0.02, 0.10))) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Year", y = "Attributable YLDs rate (per 100,000)") +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# ---------- 6. inset：China × High BMI ----------
df_zoom <- df_all %>%
  filter(location_name == "China",
         rei_name == "High body-mass index",
         year >= 2010)

p_inset <- ggplot(df_zoom, aes(x = year, y = fit, color = sex_name, fill = sex_name)) +
  geom_line(data = df_zoom %>% filter(year <= 2023),
            linewidth = 0.85) +
  geom_line(data = df_zoom %>% filter(year >= 2023),
            linewidth = 0.85, linetype = "dashed") +
  geom_ribbon(data = df_zoom %>% filter(year >= 2023),
              aes(ymin = lower, ymax = upper),
              alpha = 0.22, color = NA) +
  geom_vline(xintercept = 2023, linetype = "dotted",
             color = "grey40", linewidth = 0.3) +
  geom_point(data = df_zoom %>% filter(year %in% c(2010, 2023)),
             size = 1.3) +
  scale_color_manual(values = c("Male"   = "#E64B35",
                                "Female" = "#4DBBD5",
                                "Both"   = "#6B6B6B")) +
  scale_fill_manual(values = c("Male"   = "#E64B35",
                               "Female" = "#4DBBD5",
                               "Both"   = "#6B6B6B")) +
  scale_x_continuous(breaks = c(2010, 2020, 2030, 2040)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "China · High BMI",
       x = NULL, y = NULL) +
  theme_bw(base_size = 7) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 7, hjust = 0.5),
    axis.text = element_text(size = 5.8, color = "grey20"),
    axis.ticks = element_line(linewidth = 0.25),
    axis.ticks.length = unit(1, "pt"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey40", linewidth = 0.35),
    plot.background = element_rect(fill = "white", color = "grey40", linewidth = 0.35),
    plot.margin = margin(1, 1, 1, 1)
  )

# ---------- 7. 拼接 ----------
final <- p_main +
  inset_element(
    p_inset,
    left   = 0.025, bottom = 0.14,
    right  = 0.24,  top    = 0.39,
    align_to = "panel"
  )

print(final)

ggsave("forecast attributable YLDs rate with inset.pdf", final,
       width = 11, height = 7.5, dpi = 300)
ggsave("forecast attributable YLDs rate with inset.tiff", final,
       width = 11, height = 7.5, dpi = 600, compression = "lzw")