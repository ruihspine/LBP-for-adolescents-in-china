# ============================================================
# 从已导出的5个CSV生成正文表1、表2（修正编码）
# ============================================================

rm(list = ls())
setwd("D:/R/LBP for adolescents")

# ---------- 1. 读取已导出的5个CSV（关键：加 fileEncoding） ----------
prev <- read.csv("表1_Prevalence.csv",
                 stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "GB18030")
inc  <- read.csv("表2_Incidence.csv",
                 stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "GB18030")
yld  <- read.csv("表3_YLDs.csv",
                 stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "GB18030")
bmi  <- read.csv("表4_高BMI.csv",
                 stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "GB18030")
ergo <- read.csv("表5_职业工效学因素.csv",
                 stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "GB18030")

# ---------- 2. 提取主值函数 ----------
main_val <- function(x) as.numeric(sub(" .*", "", x))
main_pct <- function(x) as.numeric(sub("%.*", "", x))

# ---------- 3. 正文表1 ----------
tab1 <- data.frame(
  地区 = prev$地区,
  性别 = prev$性别,
  `Prevalence 1990 Rate` = main_val(prev$`1990 Prevalence rate`),
  `Prevalence 2023 Rate` = main_val(prev$`2023 Prevalence rate`),
  `Prevalence EAPC (%)`  = main_pct(prev$`EAPC of Prevalence rate (%)`),
  `Incidence 1990 Rate`  = main_val(inc$`1990 Incidence rate`),
  `Incidence 2023 Rate`  = main_val(inc$`2023 Incidence rate`),
  `Incidence EAPC (%)`   = main_pct(inc$`EAPC of Incidence rate (%)`),
  `YLDs 1990 Rate`       = main_val(yld$`1990 YLDs rate`),
  `YLDs 2023 Rate`       = main_val(yld$`2023 YLDs rate`),
  `YLDs EAPC (%)`        = main_pct(yld$`EAPC of YLDs rate (%)`),
  check.names = FALSE, stringsAsFactors = FALSE
)

# ---------- 4. 正文表2 ----------
tab2 <- data.frame(
  地区 = bmi$地区,
  性别 = bmi$性别,
  `High BMI 1990 Rate`  = main_val(bmi$`1990 Rate`),
  `High BMI 2023 Rate`  = main_val(bmi$`2023 Rate`),
  `High BMI EAPC (%)`   = main_pct(bmi$`EAPC (%)`),
  `Ergonomic 1990 Rate` = main_val(ergo$`1990 Rate`),
  `Ergonomic 2023 Rate` = main_val(ergo$`2023 Rate`),
  `Ergonomic EAPC (%)`  = main_pct(ergo$`EAPC (%)`),
  check.names = FALSE, stringsAsFactors = FALSE
)

# ---------- 5. 导出 ----------
write.csv(tab1, "正文表1.csv", row.names = FALSE, fileEncoding = "GB18030")
write.csv(tab2, "正文表2.csv", row.names = FALSE, fileEncoding = "GB18030")

cat("导出完成：正文表1.csv、正文表2.csv\n\n")
cat("=== 正文表1 ===\n"); print(tab1, row.names = FALSE)
cat("\n=== 正文表2 ===\n"); print(tab2, row.names = FALSE)