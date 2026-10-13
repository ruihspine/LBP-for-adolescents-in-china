# =========================================================
# 204 国家/地区 SDI 相关性分析：Low back pain
# 图1：Incidence rate vs SDI
# 图2：YLDs rate vs SDI
# 数据源：data1.csv + SDI 2023.csv + namex
# =========================================================

library(tidyverse)
library(patchwork)
library(ggrepel)
library(scales)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
load("./data/GBD.Rdata")

df <- read.csv("data1.csv", header = TRUE)
SDI2023 <- read.csv("SDI 2023.csv", header = TRUE)

cat("\n==== df 列名 ====\n"); print(colnames(df))
cat("\n==== SDI2023 列名 ====\n"); print(colnames(SDI2023))
cat("\n==== namex 列名 ====\n"); print(colnames(namex))
cat("namex 行数：", nrow(namex), "\n")

# ---------- 通用 SDI 相关性绘图函数 ----------
plot_sdi_204 <- function(measure, y_lab, out_name, data = df) {
  
  # 1. 筛选 2023 年数据（只保留 204 个国家）
  dfx <- data %>%
    filter(
      measure_name == measure,
      sex_name == "Both",
      cause_name == "Low back pain",
      metric_name == "Rate",
      age_id == 159,
      year == 2023,
      location_id %in% namex$location_id
    ) %>%
    select(location_id, val, year)
  
  cat("\n==== ", measure, " ====\n")
  print(dim(dfx))
  if (nrow(dfx) == 0) {
    message("没有数据：", measure)
    return(NULL)
  }
  
  # 2. 与 SDI 2023 合并：sdi 来自 SDI2023，地区名来自 namex
  SDI2019 <- SDI2023 %>%
    filter(year == 2023) %>%
    select(location_id, sdi) %>%
    left_join(., dfx, by = "location_id") %>%
    left_join(., namex, by = "location_id") %>%
    filter(!is.na(val))
  
  cat("合并后行数：", nrow(SDI2019), "\n")
  cat("有 location 的行数：", sum(!is.na(SDI2019$location)), "\n")
  
  # 3. Spearman 相关（提前算，供图上标注用）
  spearman_cor <- cor.test(SDI2019$sdi, SDI2019$val, method = "spearman")
  r <- spearman_cor$estimate
  p_val <- spearman_cor$p.value
  cat("Spearman's R:", round(r, 4), "\n")
  cat("p-value:", format(p_val, scientific = TRUE), "\n")
  
  # 4. 图上统计标注文字
  label_text <- if (p_val < 0.001) {
    sprintf("Spearman's \u03C1 = %.3f\np < 0.001", r)
  } else {
    sprintf("Spearman's \u03C1 = %.3f\np = %.3f", r, p_val)
  }
  
  # 5. 绘图
  p1 <- ggplot(SDI2019, aes(x = sdi, y = val)) +
    geom_point(aes(col = location), size = 0.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#708090") +
    geom_text_repel(aes(label = location, col = location),
                    size = 3.2, max.overlaps = 60, segment.size = 0.2) +
    annotate("text",
             x = 0.22,
             y = max(SDI2019$val, na.rm = TRUE) * 0.99,
             label = label_text,
             hjust = 0, vjust = 1,
             size = 4, fontface = "italic",
             color = "black") +
    labs(x = "SDI", y = y_lab) +
    scale_x_continuous(name = "SDI", limits = c(0.2, 1.0),
                       breaks = seq(0, 1, 0.1)) +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
  
  print(p1)
  
  # 6. 输出
  ggsave(paste0(out_name, ".pdf"), plot = p1, width = 10, height = 6, dpi = 300)
  ggsave(paste0(out_name, ".tiff"), plot = p1, width = 10, height = 6,
         dpi = 600, compression = "lzw")
  
  return(list(plot = p1, r = r, p = p_val, data = SDI2019))
}

# ---------- 图1：Incidence rate vs SDI ----------
res_inc <- plot_sdi_204(
  measure  = "Incidence",
  y_lab    = "Age-standardized incidence rate of low back pain per 100,000 population",
  out_name = "204 SDI incidence rate low back pain"
)

# ---------- 图2：YLDs rate vs SDI ----------
res_yld <- plot_sdi_204(
  measure  = "YLDs (Years Lived with Disability)",
  y_lab    = "Age-standardized YLDs rate of low back pain per 100,000 population",
  out_name = "204 SDI YLDs rate low back pain"
)

# ---------- 汇总结果 ----------
cat("\n========================================\n")
cat("Summary\n")
cat("========================================\n")
if (!is.null(res_inc)) {
  cat(sprintf("Incidence rate : r = %.4f, p = %s\n",
              res_inc$r, format(res_inc$p, scientific = TRUE)))
}
if (!is.null(res_yld)) {
  cat(sprintf("YLDs rate      : r = %.4f, p = %s\n",
              res_yld$r, format(res_yld$p, scientific = TRUE)))
}