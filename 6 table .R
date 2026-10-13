# ============================================================
# GBD 10-24 岁人群腰痛负担整理脚本（带 95% UI，导出 CSV）
# ============================================================

rm(list = ls())
setwd("D:/R/LBP for adolescents")

# ---------- 1. 读取数据 ----------
eapc <- read.csv("data 1 china  global eapc.csv",
                 stringsAsFactors = FALSE, check.names = FALSE)
nr   <- read.csv("data 1 china  global number rate.csv",
                 stringsAsFactors = FALSE, check.names = FALSE)

# ---------- 2. 统一 measure 名称 ----------
measure_map <- c(
  "YLDs (Years Lived with Disability)" = "YLDs",
  "Prevalence" = "Prevalence",
  "Incidence"  = "Incidence"
)
eapc$measure <- measure_map[as.character(eapc$measure_name)]
nr$measure   <- measure_map[as.character(nr$measure_name)]

measures  <- c("Prevalence", "Incidence", "YLDs")
locations <- c("Global", "China")
sexes     <- c("Male", "Female", "Both")

# ---------- 3. 提取 Number / Rate（含 upper、lower） ----------
extract_nr <- function(measure, metric, year) {
  sub <- nr[
    nr$measure == measure &
      nr$metric_name == metric &
      nr$year == year &
      nr$location_name %in% locations &
      nr$sex_name %in% sexes &
      nr$age_name == "10-24 years" &
      nr$cause_name == "Low back pain",
  ]
  sub <- sub[, c("location_name", "sex_name", "val", "upper", "lower")]
  prefix <- paste(year, measure, tolower(metric))
  names(sub)[3:5] <- paste0(prefix, c("", "_upper", "_lower"))
  sub
}

# ---------- 4. 提取 EAPC（仅 Rate，含 upper、lower） ----------
extract_eapc <- function(measure) {
  sub <- eapc[
    eapc$measure == measure &
      eapc$metric_name == "Rate" &
      eapc$year_start == 1990 &
      eapc$year_end == 2023 &
      eapc$location_name %in% locations &
      eapc$sex_name %in% sexes &
      eapc$age_name == "10-24 years" &
      eapc$cause_name == "Low back pain",
  ]
  sub <- sub[, c("location_name", "sex_name", "val", "upper", "lower")]
  prefix <- paste0("EAPC_", measure)
  names(sub)[3:5] <- paste0(prefix, c("", "_upper", "_lower"))
  sub
}

# ---------- 5. 合并 ----------
final <- NULL
for (m in measures) {
  for (year in c(1990, 2023)) {
    for (metric in c("Number", "Rate")) {
      sub <- extract_nr(m, metric, year)
      final <- if (is.null(final)) sub else
        merge(final, sub, by = c("location_name", "sex_name"), all = TRUE)
    }
  }
}
for (m in measures) {
  final <- merge(final, extract_eapc(m),
                 by = c("location_name", "sex_name"), all = TRUE)
}

# ---------- 6. 排序 ----------
final$location_name <- factor(final$location_name, levels = locations)
final$sex_name      <- factor(final$sex_name,      levels = sexes)
final <- final[order(final$location_name, final$sex_name), ]
final$location_name <- as.character(final$location_name)
final$sex_name      <- as.character(final$sex_name)

# ---------- 7. 格式化函数 ----------
# Number / Rate：整数（下限 to 上限）
fmt_int <- function(v, lo, hi) {
  sprintf("%.0f (%.0f to %.0f)", v, lo, hi)
}

# EAPC：×100 后主值 1 位小数，置信区间取整，形如 -6.6% (-8 to -5)
fmt_pct <- function(v, lo, hi) {
  sprintf("%.1f%% (%.0f to %.0f)", v * 100, lo * 100, hi * 100)
}

# ---------- 8. 构建三张表 ----------
build_table <- function(df, measure) {
  out <- data.frame(
    地区 = df$location_name,
    性别 = df$sex_name,
    stringsAsFactors = FALSE
  )
  
  n1990 <- paste0("1990 ", measure, " number")
  r1990 <- paste0("1990 ", measure, " rate")
  n2023 <- paste0("2023 ", measure, " number")
  r2023 <- paste0("2023 ", measure, " rate")
  eapc_col <- paste0("EAPC_", measure)
  
  out[[n1990]] <- fmt_int(df[[n1990]],
                          df[[paste0(n1990, "_lower")]],
                          df[[paste0(n1990, "_upper")]])
  
  out[[r1990]] <- fmt_int(df[[r1990]],
                          df[[paste0(r1990, "_lower")]],
                          df[[paste0(r1990, "_upper")]])
  
  out[[n2023]] <- fmt_int(df[[n2023]],
                          df[[paste0(n2023, "_lower")]],
                          df[[paste0(n2023, "_upper")]])
  
  out[[r2023]] <- fmt_int(df[[r2023]],
                          df[[paste0(r2023, "_lower")]],
                          df[[paste0(r2023, "_upper")]])
  
  out[[paste0("EAPC of ", measure, " rate (%)")]] <-
    fmt_pct(df[[eapc_col]],
            df[[paste0(eapc_col, "_lower")]],
            df[[paste0(eapc_col, "_upper")]])
  
  # 中文转换
  out$地区 <- ifelse(out$地区 == "Global", "全球",
                   ifelse(out$地区 == "China",  "中国", out$地区))
  out$性别 <- ifelse(out$性别 == "Male",   "男性",
                   ifelse(out$性别 == "Female", "女性",
                          ifelse(out$性别 == "Both",   "合计", out$性别)))
  out
}

prev <- build_table(final, "Prevalence")
inc  <- build_table(final, "Incidence")
yld  <- build_table(final, "YLDs")

# ---------- 9. 导出三个 CSV ----------
write.csv(prev, "表1_Prevalence.csv", row.names = FALSE, fileEncoding = "GB18030")
write.csv(inc,  "表2_Incidence.csv",  row.names = FALSE, fileEncoding = "GB18030")
write.csv(yld,  "表3_YLDs.csv",       row.names = FALSE, fileEncoding = "GB18030")

# ---------- 10. 查看结果 ----------
cat("导出完成，文件保存在：", getwd(), "\n\n")
print(prev)
print(inc)
print(yld)