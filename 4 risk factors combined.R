# =========================================================
# 归因分析合并图：PAF + EAPC + Forecast
# 每个小图图例都放右侧，纵向排列
# =========================================================

library(tidyverse)
library(scales)
library(forecast)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")

risk  <- read.csv("data4 risk factors.csv", header = TRUE)
eapc  <- read.csv("data5 risk factors eapc.csv", header = TRUE)


# =========================================================
# PART 1: PAF 堆叠柱状图
# =========================================================

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
    location_name = factor(location_name, levels = c("Global", "China"))
  )

p_paf <- ggplot(df_paf, aes(x = year, y = paf, fill = rei_short)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.15) +
  facet_wrap(~ location_name, ncol = 2) +
  scale_fill_manual(values = c("High BMI" = "#E64B35",
                               "Occupational ergonomic" = "#4DBBD5"),
                    name = "Risk factor") +
  scale_x_continuous(breaks = seq(1990, 2020, by = 10),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Attributable fraction of YLDs rate") +
  theme_classic(base_size = 12) +
  theme(
    # ---- 改：图例放右侧 + 纵向 ----
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  )


# =========================================================
# PART 2: EAPC 棒棒糖图
# =========================================================

df_eapc <- eapc %>%
  filter(measure_name == "YLDs (Years Lived with Disability)",
         cause_name == "Low back pain") %>%
  mutate(
    rei_short = recode(rei_name,
                       "High body-mass index" = "High BMI",
                       "Occupational ergonomic factors" = "Occupational ergonomic"),
    group = paste(location_name, sex_name, sep = " \u00B7 ")
  )

df_eapc <- df_eapc %>%
  mutate(
    group = factor(group, levels = c(
      "China \u00B7 Male",  "China \u00B7 Female",  "China \u00B7 Both",
      "Global \u00B7 Male", "Global \u00B7 Female", "Global \u00B7 Both"
    )),
    rei_short = factor(rei_short,
                       levels = c("Occupational ergonomic", "High BMI"))
  )

p_eapc <- ggplot(df_eapc, aes(x = val, y = group,
                              color = rei_short,
                              shape = rei_short)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = val, yend = group), linewidth = 0.7) +
  geom_point(size = 3.8) +
  geom_text(aes(label = sprintf("%.2f", val)),
            hjust = -0.3, vjust = 0.4,
            size = 3.3, color = "grey20",
            show.legend = FALSE) +
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
    # ---- 改：图例放右侧 + 纵向 ----
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", color = "grey20"),
    axis.text.x = element_text(size = 10),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.x = element_blank()
  )


# =========================================================
# PART 3: 预测图 + inset
# =========================================================

df_hist <- risk %>%
  filter(
    measure_name == "YLDs (Years Lived with Disability)",
    cause_name == "Low back pain",
    metric_name == "Rate"
  ) %>%
  select(location_name, sex_name, rei_name, year, val) %>%
  arrange(location_name, sex_name, rei_name, year)

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

preds <- df_hist %>%
  group_by(location_name, sex_name, rei_name) %>%
  group_modify(~ forecast_risk(.x, h = 17)) %>%
  ungroup()

df_hist2 <- df_hist %>% rename(fit = val) %>% mutate(lower = NA, upper = NA)
df_all <- bind_rows(df_hist2, preds) %>%
  mutate(
    location_name = factor(location_name, levels = c("Global", "China")),
    rei_name = factor(rei_name,
                      levels = c("High body-mass index",
                                 "Occupational ergonomic factors")),
    sex_name = factor(sex_name, levels = c("Male", "Female", "Both"))
  )

p_forecast_main <- ggplot(df_all, aes(x = year, y = fit,
                                      color = sex_name, fill = sex_name)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(data = df_all %>% filter(!is.na(lower)),
              aes(ymin = lower, ymax = upper),
              alpha = 0.15, color = NA) +
  geom_vline(xintercept = 2023.5, linetype = "dotted",
             color = "grey50", linewidth = 0.4) +
  facet_grid(location_name ~ rei_name) +
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
    # ---- 改：图例放右侧 + 纵向 ----
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

df_zoom <- df_all %>%
  filter(location_name == "China",
         rei_name == "High body-mass index",
         year >= 2010)

p_inset <- ggplot(df_zoom, aes(x = year, y = fit, color = sex_name, fill = sex_name)) +
  geom_line(data = df_zoom %>% filter(year <= 2023), linewidth = 0.85) +
  geom_line(data = df_zoom %>% filter(year >= 2023), linewidth = 0.85,
            linetype = "dashed") +
  geom_ribbon(data = df_zoom %>% filter(year >= 2023),
              aes(ymin = lower, ymax = upper),
              alpha = 0.22, color = NA) +
  geom_vline(xintercept = 2023, linetype = "dotted",
             color = "grey40", linewidth = 0.3) +
  geom_point(data = df_zoom %>% filter(year %in% c(2010, 2023)), size = 1.3) +
  scale_color_manual(values = c("Male"   = "#E64B35",
                                "Female" = "#4DBBD5",
                                "Both"   = "#6B6B6B")) +
  scale_fill_manual(values = c("Male"   = "#E64B35",
                               "Female" = "#4DBBD5",
                               "Both"   = "#6B6B6B")) +
  scale_x_continuous(breaks = c(2010, 2020, 2030, 2040)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "China \u00B7 High BMI", x = NULL, y = NULL) +
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

p_forecast <- p_forecast_main +
  inset_element(
    p_inset,
    left   = 0.025, bottom = 0.14,
    right  = 0.24,  top    = 0.39,
    align_to = "panel"
  )


# =========================================================
# PART 4: 合并三张图
# =========================================================

combined <- (p_paf | p_eapc) / p_forecast +
  plot_layout(heights = c(1, 1.3)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 16)
    )
  )

print(combined)

ggsave("combined figure risk factors.pdf", combined,
       width = 16, height = 13, dpi = 300)
ggsave("combined figure risk factors.tiff", combined,
       width = 16, height = 13, dpi = 600, compression = "lzw")