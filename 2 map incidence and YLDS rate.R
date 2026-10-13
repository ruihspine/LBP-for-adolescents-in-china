# =========================================================
# 世界地图：Low back pain
# 图1：Age-standardized Incidence rate, 2023
# 图2：Age-standardized YLDs rate, 2023
# 自动识别 YLDs 的 measure_name 写法
# =========================================================

library(tidyverse)
library(readxl)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(mapdata)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")
df <- read.csv("data1.csv", header = TRUE)
load("./data/GBD.Rdata")

colnames(df)

# ---------- 0. 自动识别 measure 名称 ----------
all_measures <- unique(df$measure_name)
print(all_measures)

inc_measure <- all_measures[grepl("^Incidence$", all_measures, ignore.case = TRUE)][1]
yld_measure <- all_measures[grepl("YLD", all_measures, ignore.case = TRUE)][1]

cat("Incidence measure:", inc_measure, "\n")
cat("YLDs measure:", yld_measure, "\n")

if (is.na(inc_measure)) stop("数据里找不到 Incidence")
if (is.na(yld_measure)) stop("数据里找不到 YLDs")

# ---------- 通用绘图函数 ----------
make_map <- function(measure, title_text, out_name, data = df) {
  
  # 1. 筛选数据
  dfx <- data %>%
    filter(
      measure_name == measure,
      cause_name == "Low back pain",
      metric_name == "Rate",
      sex_name == "Both",
      year == 2023,
      age_id == 159
    )
  
  cat("\n==== ", measure, " ====\n")
  print(dim(dfx))
  if (nrow(dfx) == 0) {
    message("没有数据：", measure)
    return(NULL)
  }
  
  # 2. 合并数据
  dfplot <- dfx %>%
    filter(location_id %in% namex$location_id) %>%
    select(location_id, val) %>%
    left_join(., namex, by = "location_id")
  
  # 3. 颜色方案
  color1 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
  colorx1 <- color1
  
  # 4. 构建绘图数据
  ASR <- dfplot %>%
    select(location_id, location, val) %>%
    mutate(val = val / 1)
  
  df_asr <- left_join(df_world, ASR, by = "location_id")
  
  # 5. 计算分位数断点
  xmin <- min(na.omit(df_asr$val))
  
  breaks <- quantile(ASR$val, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  breaks <- breaks[-1]
  
  breaks_3dec <- round(breaks, 3)
  xmin_3dec <- floor(xmin * 1000) / 1000
  
  formatted_breaks <- vector("character", length(breaks_3dec))
  formatted_breaks[1] <- paste0(format(xmin_3dec, nsmall = 3), " - ",
                                format(breaks_3dec[1], nsmall = 3))
  for (i in 2:length(breaks_3dec)) {
    formatted_breaks[i] <- paste0(format(breaks_3dec[i - 1], nsmall = 3), " - ",
                                  format(breaks_3dec[i], nsmall = 3))
  }
  print(formatted_breaks)
  
  # 6. 处理唯一断点
  unb <- unique(breaks)
  if (unb[1] == xmin) {
    unb <- unb[-1]
  }
  
  # 7. 调整标签数量与实际分组数匹配
  xlen <- length(na.omit(unique(cut(df_asr$val, breaks = c(xmin, unb)))))
  xstar <- length(formatted_breaks) - xlen + 1
  formatted_breaks <- formatted_breaks[xstar:length(formatted_breaks)]
  
  # 8. 添加分组变量
  df_asr <- df_asr %>%
    mutate(asr = replace_na(val, -99)) %>%
    mutate(asr_cut = cut(asr,
                         breaks = c(xmin, unb),
                         labels = formatted_breaks)) %>%
    filter(!is.na(asr_cut))
  
  # 9. 绘图
  p <- ggplot(df_asr %>% na.omit()) +
    geom_sf(aes(geometry = geometry, fill = asr_cut), size = 0.1) +
    scale_fill_manual(
      values = rev(colorx1),
      guide = guide_legend(reverse = TRUE)
    ) +
    guides(
      fill = guide_legend(
        label.position = "bottom",
        nrow = 1,
        title.position = "top",
        title = title_text
      )
    )
  
  p1 <- p +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = 'top',
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.6, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.05, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, 0, -5, 0),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  print(p1)
  
  # 10. 输出
  ggsave(paste0(out_name, ".pdf"), plot = p1, width = 8, height = 6, dpi = 600)
  ggsave(paste0(out_name, ".tiff"), plot = p1, width = 8, height = 6,
         dpi = 600, compression = "lzw")
  
  return(p1)
}

# ---------- 图1：Incidence rate ----------
p_inc <- make_map(
  measure    = inc_measure,
  title_text = "Age-standardized incidence rate of low back pain, 2023",
  out_name   = "map incidence rate 2023"
)

# ---------- 图2：YLDs rate ----------
p_yld <- make_map(
  measure    = yld_measure,
  title_text = "Age-standardized YLDs rate of low back pain, 2023",
  out_name   = "map YLDs rate 2023"
)

# ---------- 图3：拼成一张大图（A / B 面板） ----------
if (!is.null(p_inc) && !is.null(p_yld)) {
  combined <- p_inc / p_yld +
    plot_annotation(
      tag_levels = "A",
      theme = theme(plot.tag = element_text(face = "bold", size = 14))
    )
  
  ggsave("map incidence and YLDs 2023.pdf", combined,
         width = 8, height = 12, dpi = 600)
  ggsave("map incidence and YLDs 2023.tiff", combined,
         width = 8, height = 12, dpi = 600, compression = "lzw")
}











# =========================================================
# 诊断：两张图的分组数、颜色数、逐国数值核对
# =========================================================

diagnose_map <- function(measure, data = df) {
  
  dfx <- data %>%
    filter(
      measure_name == measure,
      cause_name == "Low back pain",
      metric_name == "Rate",
      sex_name == "Both",
      year == 2023,
      age_id == 159
    )
  
  dfplot <- dfx %>%
    filter(location_id %in% namex$location_id) %>%
    select(location_id, val) %>%
    left_join(., namex, by = "location_id")
  
  ASR <- dfplot %>% select(location_id, location, val)
  df_asr <- left_join(df_world, ASR, by = "location_id")
  
  xmin <- min(na.omit(df_asr$val))
  breaks <- quantile(ASR$val, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  breaks <- breaks[-1]
  unb <- unique(breaks)
  if (unb[1] == xmin) unb <- unb[-1]
  
  # 实际分组数
  grp <- cut(df_asr$val, breaks = c(xmin, unb))
  n_grp <- length(na.omit(unique(grp)))
  
  cat("\n==== ", measure, " ====\n")
  cat("最小值:", round(xmin, 3), "  最大值:", round(max(df_asr$val, na.rm=TRUE), 3), "\n")
  cat("断点数:", length(c(xmin, unb)), "  实际分组数:", n_grp, "\n")
  cat("会用到 rev(colorx1) 的前", n_grp, "个颜色：\n")
  print(rev(color1)[1:n_grp])
  cat("→ 是否用到深红 #67001f ?", "#67001f" %in% rev(color1)[1:n_grp], "\n")
  
  # 逐国数值 + 分组
  out <- df_asr %>%
    st_drop_geometry() %>%
    select(location_id, location, val, asr_cut) %>%
    arrange(desc(val)) %>%
    filter(!is.na(val))
  
  print(head(out, 15))
  print(tail(out, 15))
  
  invisible(out)
}

diag_inc <- diagnose_map(inc_measure)
diag_yld <- diagnose_map(yld_measure)