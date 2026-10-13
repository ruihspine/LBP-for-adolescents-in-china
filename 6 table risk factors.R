# ============================================================
# GBD 10-24 岁人群腰痛归因风险因素整理脚本（Number + Rate + EAPC）
# 输入：1.csv（含 Number/Rate），2.csv（EAPC）
# ============================================================

rm(list = ls())
setwd("D:/R/LBP for adolescents")

# ---------- 1. 读取数据 ----------
risk      <- read.csv("1.csv", stringsAsFactors = FALSE, check.names = FALSE)
risk_eapc <- read.csv("2.csv", stringsAsFactors = FALSE, check.names = FALSE)

locations <- c("Global", "China")
sexes     <- c("Male", "Female", "Both")

# ---------- 2. 是否把负值截断为 0 ----------
# FALSE = 保留原值；TRUE = 把小于 0 的数和区间截断为 0
truncate_neg <- FALSE

# ---------- 3. 提取 Number / Rate ----------
extract_nr <- function(rei, metric, year) {
  sub <- risk[
    risk$rei_name == rei &
      risk$metric_name == metric &
      risk$year == year &
      risk$location_name %in% locations &
      risk$sex_name %in% sexes &
      risk$age_name == "10-24 years" &
      risk$cause_name == "Low back pain",
  ]
  sub <- sub[, c("location_name", "sex_name", "val", "upper", "lower")]
  prefix <- paste0(year, "_", tolower(metric))
  names(sub)[3:5] <- paste0(prefix, c("", "_upper", "_lower"))
  sub
}

# ---------- 4. 提取 EAPC ----------
extract_eapc <- function(rei) {
  sub <- risk_eapc[
    risk_eapc$rei_name == rei &
      risk_eapc$metric_name == "Rate" &
      risk_eapc$year_start == 1990 &
      risk_eapc$year_end == 2023 &
      risk_eapc$location_name %in% locations &
      risk_eapc$sex_name %in% sexes &
      risk_eapc$age_name == "10-24 years" &
      risk_eapc$cause_name == "Low back pain",
  ]
  sub <- sub[, c("location_name", "sex_name", "val", "upper", "lower")]
  names(sub)[3:5] <- c("eapc", "eapc_upper", "eapc_lower")
  sub
}

# ---------- 5. 格式化 ----------
fmt_int <- function(v, lo, hi) {
  if (truncate_neg) {
    v  <- pmax(v,  0)
    lo <- pmax(lo, 0)
    hi <- pmax(hi, 0)
  }
  sprintf("%.0f (%.0f to %.0f)", v, lo, hi)
}
fmt_pct <- function(v, lo, hi) {
  sprintf("%.1f%% (%.0f to %.0f)", v * 100, lo * 100, hi * 100)
}

# ---------- 6. 构建单张表 ----------
build_risk_table <- function(rei) {
  n1990 <- extract_nr(rei, "Number", 1990)
  r1990 <- extract_nr(rei, "Rate",   1990)
  n2023 <- extract_nr(rei, "Number", 2023)
  r2023 <- extract_nr(rei, "Rate",   2023)
  eapc  <- extract_eapc(rei)
  
  df <- n1990
  df <- merge(df, r1990, by = c("location_name","sex_name"), all = TRUE)
  df <- merge(df, n2023, by = c("location_name","sex_name"), all = TRUE)
  df <- merge(df, r2023, by = c("location_name","sex_name"), all = TRUE)
  df <- merge(df, eapc,  by = c("location_name","sex_name"), all = TRUE)
  
  df$location_name <- factor(df$location_name, levels = locations)
  df$sex_name      <- factor(df$sex_name,      levels = sexes)
  df <- df[order(df$location_name, df$sex_name), ]
  df$location_name <- as.character(df$location_name)
  df$sex_name      <- as.character(df$sex_name)
  
  out <- data.frame(
    地区          = df$location_name,
    性别          = df$sex_name,
    `1990 Number` = fmt_int(df$`1990_number`, df$`1990_number_lower`, df$`1990_number_upper`),
    `1990 Rate`   = fmt_int(df$`1990_rate`,   df$`1990_rate_lower`,   df$`1990_rate_upper`),
    `2023 Number` = fmt_int(df$`2023_number`, df$`2023_number_lower`, df$`2023_number_upper`),
    `2023 Rate`   = fmt_int(df$`2023_rate`,   df$`2023_rate_lower`,   df$`2023_rate_upper`),
    `EAPC (%)`    = fmt_pct(df$eapc, df$eapc_lower, df$eapc_upper),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  
  out$地区 <- ifelse(out$地区 == "Global", "全球",
                   ifelse(out$地区 == "China",  "中国", out$地区))
  out$性别 <- ifelse(out$性别 == "Male",   "男性",
                   ifelse(out$性别 == "Female", "女性",
                          ifelse(out$性别 == "Both",   "合计", out$性别)))
  out
}

# ---------- 7. 生成两张表 ----------
tab_bmi  <- build_risk_table("High body-mass index")
tab_ergo <- build_risk_table("Occupational ergonomic factors")

# ---------- 8. 导出 ----------
write.csv(tab_bmi,  "表4_高BMI.csv",
          row.names = FALSE, fileEncoding = "GB18030")
write.csv(tab_ergo, "表5_职业工效学因素.csv",
          row.names = FALSE, fileEncoding = "GB18030")

cat("导出完成，文件保存在：", getwd(), "\n\n")
cat("=== 表4 高 BMI ===\n"); print(tab_bmi)
cat("\n=== 表5 职业工效学因素 ===\n"); print(tab_ergo)