library(tidyverse)
library(scales)
library(ggsci)
rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
df <- read.csv("data1.csv", header = TRUE)

# 1. 筛选数据
dfx <- df %>%
  filter(
    measure_name == "Incidence",
    location_id %in% c(1, 6),
    cause_name == "Low back pain",
    metric_name %in% c("Number", "Rate"),
    sex_name == "Both",
    age_id == 159
  )

# 确保 Global / China 顺序，如果你的 location_name 不是这两个名字，请改这里
dfx$location_name <- factor(
  dfx$location_name,
  levels = c("Global", "China")
)

# 2. 拆分 Number 与 Rate
df_number <- dfx %>% filter(metric_name == "Number")
df_rate   <- dfx %>% filter(metric_name == "Rate")

# 3. 双轴缩放因子：把 Rate 缩放到 Number 的量级上
max_number <- max(df_number$val, na.rm = TRUE)
max_rate   <- max(df_rate$val, na.rm = TRUE)
scaling_factor <- max_number / max_rate

# 4. 颜色：使用 Lancet 配色，也可自己改
pal <- pal_lancet()(2)
cols <- c(
  "Global" = pal[1],
  "China"  = pal[2]
)

# 5. 为了让折线点与并排柱状图对齐，给 Rate 数据加一点 x 偏移
dodge_width <- 0.8
df_rate <- df_rate %>%
  mutate(
    x_plot = ifelse(
      location_name == "Global",
      year - dodge_width / 4,
      year + dodge_width / 4
    )
  )

# 6. 绘图
p <- ggplot() +
  # Number：柱状图，左轴
  geom_col(
    data = df_number,
    aes(x = year, y = val, fill = location_name),
    position = position_dodge(width = dodge_width),
    width = 0.7,
    color = "grey20",
    linewidth = 0.2,
    alpha = 0.75
  ) +
  # Rate：折线，右轴，实际画在左轴需要乘以 scaling_factor
  geom_line(
    data = df_rate,
    aes(
      x = x_plot,
      y = val * scaling_factor,
      color = location_name,
      linetype = location_name
    ),
    linewidth = 1
  ) +
  # Rate：点
  geom_point(
    data = df_rate,
    aes(
      x = x_plot,
      y = val * scaling_factor,
      color = location_name
    ),
    size = 1.8
  ) +
  # x 轴
  scale_x_continuous(
    breaks = c(seq(1990, 2020, by = 5), 2023),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # 左轴 Number，右轴 Rate
  scale_y_continuous(
    name = "Incident cases",
    # 如果数字太大，用 M；如果你想用 K，改成 scale = 1e-3, unit = "K"
    labels = label_number(scale = 1e-6, unit = "M", accuracy = 1),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(
      ~ . / scaling_factor,
      name = "Age-standardized incidence rate (per 100,000)",
      labels = label_number(accuracy = 1, big.mark = ",")
    )
  ) +
  scale_fill_manual(values = cols, name = "Location") +
  scale_color_manual(values = cols, name = "Location") +
  scale_linetype_manual(
    values = c("Global" = "solid", "China" = "dashed"),
    name = "Location"
  ) +
  labs(x = "Year") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(alpha = 0.75, color = "grey20")
    ),
    color = guide_legend(
      override.aes = list(linetype = c("solid", "dashed"), size = 1.2)
    )
  )

p

# 7. 输出
ggsave("trend_global_china_dual_axis.pdf", plot = p, width = 8.7, height = 6.5, dpi = 300)
ggsave("trend_global_china_dual_axis.tiff", plot = p, width = 8.7, height = 6.5,
       dpi = 600, compression = "lzw")