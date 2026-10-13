# ============================================================
# EAPC calculation from yearly age-standardized rates
# Data: LBP, AYAs 10-24, China & Global, 1990-2023
# ============================================================

library(tidyverse)
library(broom)
rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
# ---------- 1. 读取数据 ----------
df <- read_csv("data  xiugai  eapc JISUAN.csv")

# 查看列名和结构
glimpse(df)

# ---------- 2. 数据清洗 ----------
df_clean <- df %>%
  filter(
    age_name == "10-24 years",
    metric_name == "Rate",
    sex_name == "Both",                 # 如果要分性别，去掉这行并保留 sex_name
    location_name %in% c("China", "Global"),
    measure_name %in% c(
      "Incidence",
      "Prevalence",
      "YLDs (Years Lived with Disability)"
    )
  ) %>%
  select(location_name, measure_name, year, val) %>%
  distinct() %>%                        # 去重
  arrange(location_name, measure_name, year)

# 检查年份是否完整
df_clean %>%
  group_by(location_name, measure_name) %>%
  summarise(
    n_years = n(),
    min_year = min(year),
    max_year = max(year),
    .groups = "drop"
  ) %>%
  print()

# ---------- 3. 定义 EAPC 函数 ----------
calc_eapc <- function(data) {
  # data 需包含 year 和 val
  fit <- lm(log(val) ~ year, data = data)
  
  beta <- coef(fit)["year"]
  ci   <- confint(fit, "year", level = 0.95)
  
  eapc     <- 100 * (exp(beta) - 1)
  eapc_lci <- 100 * (exp(ci[1]) - 1)
  eapc_uci <- 100 * (exp(ci[2]) - 1)
  
  # 总百分比变化
  val_1990 <- data$val[data$year == 1990]
  val_2023 <- data$val[data$year == 2023]
  total_change <- (val_2023 - val_1990) / val_1990 * 100
  
  tibble(
    eapc         = round(eapc, 3),
    eapc_lci     = round(eapc_lci, 3),
    eapc_uci     = round(eapc_uci, 3),
    p_value      = round(summary(fit)$coefficients["year", "Pr(>|t|)"], 4),
    total_change = round(total_change, 2)
  )
}

# ---------- 4. 分组计算 EAPC ----------
eapc_results <- df_clean %>%
  group_by(location_name, measure_name) %>%
  group_modify(~ calc_eapc(.x)) %>%
  ungroup()

# ---------- 5. 输出结果 ----------
print(eapc_results)

# 整理成表格
final_table <- eapc_results %>%
  mutate(
    eapc_95CI = paste0(
      sprintf("%.2f", eapc), " (",
      sprintf("%.2f", eapc_lci), " to ",
      sprintf("%.2f", eapc_uci), ")"
    )
  ) %>%
  select(
    Location = location_name,
    Measure = measure_name,
    EAPC = eapc,
    `95% CI` = eapc_95CI,
    `P value` = p_value,
    `Total change (%)` = total_change
  )

print(final_table)

# 保存
write_csv(final_table, "EAPC_results_LBP_AYA.csv")