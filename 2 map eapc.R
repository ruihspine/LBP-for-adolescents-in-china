# =========================================================
# EAPC 世界地图：Low back pain
# 图1：Incidence EAPC
# 图2：YLDs EAPC
# 分箱：负值 5 档 + 正值 5 档，0 强制在中间
# 配色：与原代码完全一致（5 冷 + 5 暖）
# =========================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")

# ---------- 0. 先 load 地图，再读 EAPC ----------
load("./data/GBD.Rdata")
df_eapc <- read.csv("data6 eapc.csv", header = TRUE)

# ---------- 1. 固定 measure 名 ----------
inc_measure <- "Incidence"
yld_measure <- "YLDs (Years Lived with Disability)"

# ---------- 2. 通用 EAPC 地图函数 ----------
make_eapc_map <- function(measure, title_text, out_name, data = df_eapc) {
  
  # 2.1 筛选
  dfx <- data %>%
    filter(measure_name == measure) %>%
    select(location_id, val)
  
  cat("\n==== ", measure, " ====\n")
  cat("行数：", nrow(dfx), "\n")
  if (nrow(dfx) == 0) {
    message("没有数据：", measure)
    return(NULL)
  }
  
  # 2.2 与 namex / 地图 sf 合并
  dfplot <- dfx %>%
    filter(location_id %in% namex$location_id) %>%
    left_join(., namex, by = "location_id")
  
  df_asr <- left_join(df_world, dfplot %>% select(location_id, val),
                      by = "location_id")
  
  vals <- df_asr$val[!is.na(df_asr$val)]
  cat("匹配到地图的国家数：", length(vals), "\n")
  cat("min:", round(min(vals), 4),
      "  max:", round(max(vals), 4),
      "  median:", round(median(vals), 4), "\n")
  cat("负值数:", sum(vals < 0), "  正值数:", sum(vals > 0), "\n")
  
  # ---------- 2.3 负值 5 档 + 正值 5 档，0 强制在中间 ----------
  neg_vals <- vals[vals < 0]
  pos_vals <- vals[vals >= 0]
  
  neg_brks <- as.numeric(quantile(neg_vals, probs = seq(0, 1, length.out = 6)))
  neg_brks[length(neg_brks)] <- 0
  
  pos_brks <- as.numeric(quantile(pos_vals, probs = seq(0, 1, length.out = 6)))
  pos_brks[1] <- 0
  
  brks <- sort(unique(round(c(neg_brks, pos_brks), 4)))
  
  if (length(brks) != 11) {
    cat("断点数为", length(brks), "，重采样为 11 个\n")
    brks <- seq(min(vals), max(vals), length.out = 11)
    brks <- sort(unique(round(brks, 4)))
  }
  
  cat("分箱断点（", length(brks), "个）：\n", sep = ""); print(brks)
  cat("实际档数：", length(brks) - 1, "\n")
  
  # ---------- 2.4 配色：与原代码完全一致（5 冷 + 5 暖） ----------
  color1 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
  color_pal <- rev(color1)
  
  # ---------- 2.5 分箱 + 图例标签 ----------
  df_asr <- df_asr %>%
    mutate(asr_cut = cut(val, breaks = brks, include.lowest = TRUE))
  
  lvls <- levels(df_asr$asr_cut)
  lvls_num <- lapply(strsplit(gsub("\\(|\\[|\\]|\\)", "", lvls), ","), as.numeric)
  new_labels <- sapply(lvls_num, function(x) {
    paste0(format(round(x[1], 3), nsmall = 3), " – ",
           format(round(x[2], 3), nsmall = 3))
  })
  cat("每档国家数：\n"); print(table(df_asr$asr_cut))
  
  # ---------- 2.6 绘图 ----------
  p <- ggplot(df_asr) +
    geom_sf(aes(geometry = geometry, fill = asr_cut), size = 0.1) +
    scale_fill_manual(
      values = color_pal,
      labels = new_labels,
      na.value = "grey80",
      na.translate = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    guides(
      fill = guide_legend(
        label.position = "bottom",
        nrow = 2,
        byrow = TRUE,
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
  
  # ---------- 2.7 输出 ----------
  ggsave(paste0(out_name, ".pdf"), plot = p1, width = 8, height = 6, dpi = 600)
  ggsave(paste0(out_name, ".tiff"), plot = p1, width = 8, height = 6,
         dpi = 600, compression = "lzw")
  
  return(p1)
}

# ---------- 3. 生成两张图 ----------
p_inc <- make_eapc_map(
  measure    = inc_measure,
  title_text = "Estimated annual percentage change in incidence rate, 1990–2023",
  out_name   = "map EAPC incidence"
)

p_yld <- make_eapc_map(
  measure    = yld_measure,
  title_text = "Estimated annual percentage change in YLDs rate, 1990–2023",
  out_name   = "map EAPC YLDs"
)

# ---------- 4. 拼成 A / B 大图 ----------
if (!is.null(p_inc) && !is.null(p_yld)) {
  combined <- p_inc / p_yld +
    plot_annotation(
      tag_levels = "A",
      theme = theme(plot.tag = element_text(face = "bold", size = 14))
    )
  
  ggsave("map EAPC incidence and YLDs.pdf", combined,
         width = 8, height = 12, dpi = 600)
  ggsave("map EAPC incidence and YLDs.tiff", combined,
         width = 8, height = 12, dpi = 600, compression = "lzw")
}