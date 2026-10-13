# =========================================================
# SDI 相关性分析：Low back pain
# 图1：Incidence rate vs SDI
# 图2：YLDs rate vs SDI
# 数据源：data1.csv
# =========================================================

library(tidyverse)
library(patchwork)
library(ggsci)
library(scales)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
load("./data/GBD.Rdata")

df <- read.csv("data1.csv", header = TRUE)
SDI2019 <- read.csv("SDI 2023.csv", header = TRUE)

cat("\n==== df 列名 ====\n"); print(colnames(df))
cat("\n==== SDI2019 列名 ====\n"); print(colnames(SDI2019))

# ---------- 21 个地区顺序 ----------
location_order <- c(
  "Global", "High-income Asia Pacific", "High-income North America", "Western Europe",
  "Australasia", "Andean Latin America", "Tropical Latin America", "Central Latin America",
  "Southern Latin America", "Caribbean", "Central Europe", "Eastern Europe", "Central Asia",
  "North Africa and Middle East", "South Asia", "Southeast Asia", "East Asia", "Oceania",
  "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Central Sub-Saharan Africa",
  "Southern Sub-Saharan Africa"
)

# ---------- 配色 ----------
colors <- c(pal_npg("nrc", alpha = 0.7)(9),
            pal_aaas("default", alpha = 0.7)(9),
            pal_nejm("default", alpha = 0.7)(8),
            pal_jama("default", alpha = 0.7)(7))

# ---------- 通用 SDI 相关性绘图函数 ----------
plot_sdi <- function(measure, y_lab, out_name, data = df) {
  
  # 1. 筛选
  dfx <- data %>%
    filter(
      measure_name == measure,
      location_id %in% c(1, name_21),
      sex_name == "Both",
      cause_name == "Low back pain",
      metric_name == "Rate",
      age_id == 159
      # year == 2023        # 如需只看 2023 年，取消注释
    ) %>%
    select(location_id, val, year)
  
  cat("\n==== ", measure, " ====\n")
  print(dim(dfx))
  if (nrow(dfx) == 0) {
    message("没有数据：", measure)
    return(NULL)
  }
  
  # 2. 与 SDI 合并
  df11 <- left_join(dfx, SDI2019)
  
  df11_filtered <- df11 %>%
    filter(!is.na(sdi) & !is.na(val)) %>%
    mutate(location_name = factor(location_name, levels = location_order))
  
  cat("合并后行数：", nrow(df11_filtered), "\n")
  
  # 3. 绘图
  p <- ggplot(df11_filtered, aes(x = sdi, y = val,
                                 color = location_name,
                                 shape = location_name)) +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE, aes(group = 1), color = "#92A8D1") +
    scale_shape_manual(values = 1:22, breaks = location_order, labels = location_order) +
    scale_color_manual(values = colors, breaks = location_order, labels = location_order) +
    labs(x = "Socio-demographic Index",
         shape = "", color = "",
         y = y_lab) +
    theme_bw() +
    theme(legend.key.size = unit(0.03, "line"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.title = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 8)) +
    guides(shape = guide_legend(nrow = 22))
  
  print(p)
  
  # 4. Spearman 相关
  spearman_cor <- cor.test(df11_filtered$sdi, df11_filtered$val, method = "spearman")
  r <- spearman_cor$estimate
  p_val <- spearman_cor$p.value
  cat("Spearman's R:", round(r, 4), "\n")
  cat("p-value:", format(p_val, scientific = TRUE), "\n")
  
  # 5. 输出
  ggsave(paste0(out_name, ".pdf"), plot = p, width = 10, height = 6, dpi = 300)
  ggsave(paste0(out_name, ".tiff"), plot = p, width = 10, height = 6,
         dpi = 600, compression = "lzw")
  
  return(list(plot = p, r = r, p = p_val, data = df11_filtered))
}

# ---------- 图1：Incidence rate vs SDI ----------
res_inc <- plot_sdi(
  measure  = "Incidence",
  y_lab    = "Age-standardized incidence rate of low back pain (per 100,000)",
  out_name = "SDI vs incidence rate"
)

# ---------- 图2：YLDs rate vs SDI ----------
res_yld <- plot_sdi(
  measure  = "YLDs (Years Lived with Disability)",
  y_lab    = "Age-standardized YLDs rate of low back pain (per 100,000)",
  out_name = "SDI vs YLDs rate"
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