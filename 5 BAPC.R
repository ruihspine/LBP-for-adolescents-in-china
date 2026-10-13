# =========================================================
# BAPC 预测：China + Global 一次跑完
# 经典图：plotBAPC() 直接画在 pdf/tiff 设备上（不赋值）
# 敏感性图：ggplot，注释动态定位在曲线上方，不挡线
# =========================================================

library(data.table)
library(tidyverse)
library(BAPC)
library(INLA)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\LBP for adolescents")

age_keep <- c("10-14", "15-19", "20-24")
var_name <- c("location_name", "sex_name", "year", "age_name", "val")

INLA::inla.setOption(num.threads = "1:1")
INLA::inla.setOption(inla.mode = "classic")


# =========================================================
# 核心函数：跑一个地区的完整流程
# =========================================================

run_bapc_location <- function(location_target,
                              stand_file,
                              output_prefix) {
  
  cat("\n\n########################################\n")
  cat("# 开始跑：", location_target, "\n")
  cat("########################################\n")
  
  # ---------- 1. 历史人口 ----------
  pop_hist <- read.csv("pop for 10-24.csv") %>%
    select(all_of(var_name)) %>%
    filter(location_name == location_target,
           sex_name == "Both",
           age_name %in% c("10-14 years", "15-19 years", "20-24 years")) %>%
    mutate(age_name = gsub(" years", "", age_name))
  
  cat("历史人口行数：", nrow(pop_hist), "（应为 102）\n")
  
  # ---------- 2. 预测人口 ----------
  pop_pred <- fread("IHME_POP_2017_2100_Both.csv") %>%
    filter(location_name == location_target,
           sex == "Both",
           year_id %in% 2024:2040,
           age_group_name %in% c("10 to 14", "15 to 19", "20 to 24")) %>%
    mutate(age_name = gsub(" to ", "-", age_group_name),
           year = year_id,
           sex_name = "Both") %>%
    select(location_name, sex_name, year, age_name, val)
  
  cat("预测人口行数：", nrow(pop_pred), "（应为 51）\n")
  
  # ---------- 3. 人口矩阵 ----------
  GBD <- bind_rows(pop_hist, pop_pred) %>%
    mutate(age_name = factor(age_name, levels = age_keep))
  setDT(GBD)
  GBD_n <- dcast(GBD, year ~ age_name, value.var = "val") %>% as.data.frame()
  rownames(GBD_n) <- GBD_n$year
  GBD_n <- GBD_n[, -1][, age_keep]
  GBD_n[] <- lapply(GBD_n, function(x) round(as.numeric(x), 0))
  rownames(GBD_n) <- 1990:2040
  colnames(GBD_n) <- age_keep
  
  # ---------- 4. 发病数据 ----------
  IBD_in <- read.csv("data3 age.csv") %>%
    filter(location_name == location_target,
           sex_name == "Both",
           age_name %in% c("10-14 years", "15-19 years", "20-24 years"),
           measure_name == "Incidence",
           metric_name == "Number") %>%
    select(age_name, year, val) %>%
    mutate(age_name = gsub(" years", "", age_name))
  
  IBD_n <- reshape2::dcast(IBD_in, year ~ age_name, value.var = "val")
  rownames(IBD_n) <- IBD_n$year
  IBD_n <- IBD_n[, -1][, age_keep]
  IBD_n[] <- lapply(IBD_n, function(x) round(as.numeric(x), 0))
  rownames(IBD_n) <- 1990:2023
  colnames(IBD_n) <- age_keep
  
  # ---------- 5. 补齐 2024-2040 ----------
  IBD_pro <- matrix(NA, nrow = 17, ncol = 3) %>% as.data.frame()
  rownames(IBD_pro) <- 2024:2040
  colnames(IBD_pro) <- age_keep
  IBD_pro_n <- rbind(IBD_n, IBD_pro)
  IBD_pro_n[] <- lapply(IBD_pro_n, function(x) round(as.numeric(x), 0))
  rownames(IBD_pro_n) <- 1990:2040
  colnames(IBD_pro_n) <- age_keep
  
  # ---------- 6. 三套权重 ----------
  age_stand <- read.csv(stand_file)
  age_stand$age_name <- gsub(" years", "", age_stand$age_name)
  
  wstand_main <- age_stand %>%
    filter(age_name %in% age_keep) %>%
    arrange(match(age_name, age_keep)) %>%
    pull(std_population)
  wstand_main <- wstand_main / sum(wstand_main)
  
  who_raw <- c("10-14" = 8.60, "15-19" = 8.47, "20-24" = 8.22)
  wstand_who <- who_raw[age_keep] / sum(who_raw[age_keep])
  
  wstand_equal <- c(1/3, 1/3, 1/3)
  names(wstand_equal) <- age_keep
  
  cat("主权重（", stand_file, "）：", round(wstand_main, 4), "\n")
  
  # ---------- 7. 跑三次 BAPC ----------
  IBD_input <- APCList(IBD_pro_n, GBD_n, gf = 5)
  
  run_one <- function(weight, label) {
    cat("  跑权重：", label, "\n")
    result <- BAPC(IBD_input,
                   predict = list(npredict = 17, retro = FALSE),
                   secondDiff = FALSE,
                   stdweight = weight,
                   verbose = FALSE)
    ar <- data.frame(result@agestd.rate)
    ar$year  <- as.numeric(rownames(ar))
    ar$rate  <- ar$mean * 1e5
    ar$lower <- (ar$mean - 1.96 * ar$sd) * 1e5
    ar$upper <- (ar$mean + 1.96 * ar$sd) * 1e5
    ar$scenario <- label
    list(result = result,
         data = ar %>% select(year, rate, lower, upper, scenario))
  }
  
  res_main  <- run_one(wstand_main,  paste0(location_target, " standard"))
  res_who   <- run_one(wstand_who,   "WHO standard")
  res_equal <- run_one(wstand_equal, "Equal weight")
  
  # ---------- 8. 合并结果 ----------
  all_res <- bind_rows(res_main$data, res_who$data, res_equal$data)
  all_res$scenario <- factor(all_res$scenario,
                             levels = c(paste0(location_target, " standard"),
                                        "WHO standard",
                                        "Equal weight"))
  all_res$location <- location_target
  
  end_2040 <- all_res %>% filter(year == 2040) %>% pull(rate)
  max_rel_diff <- (max(end_2040) - min(end_2040)) / mean(end_2040) * 100
  cat("\n三套权重 2040 年最大相对差异：", round(max_rel_diff, 2), "%\n")
  
  # ---------- 9. 图 1：经典 BAPC（不赋值，直接画） ----------
  cat("\n>>> 显示：", output_prefix, "经典 BAPC 图\n")
  plotBAPC(res_main$result,
           scale = 10^5,
           type = "ageStdRate",
           showdata = TRUE)
  Sys.sleep(2)
  
  pdf(paste0(output_prefix, " BAPC classic.pdf"),
      width = 8, height = 5.5)
  plotBAPC(res_main$result,
           scale = 10^5,
           type = "ageStdRate",
           showdata = TRUE)
  dev.off()
  
  tiff(paste0(output_prefix, " BAPC classic.tiff"),
       width = 8, height = 5.5, units = "in", res = 600,
       compression = "lzw")
  plotBAPC(res_main$result,
           scale = 10^5,
           type = "ageStdRate",
           showdata = TRUE)
  dev.off()
  
  cat("  已保存：", output_prefix, " BAPC classic.pdf / .tiff\n", sep = "")
  
  # ---------- 10. 图 2：三线敏感性（ggplot） ----------
  # 动态计算注释 y 位置：放在历史段上方空白区，不挡线
  hist_rate  <- all_res$rate[all_res$year <= 2023]
  y_top      <- max(hist_rate) + 0.55 * diff(range(hist_rate))
  
  p_sens <- ggplot(all_res, aes(x = year, y = rate,
                                color = scenario,
                                linetype = scenario)) +
    geom_vline(xintercept = 2023.5, linetype = "dotted",
               color = "grey50", linewidth = 0.4) +
    geom_ribbon(data = all_res %>%
                  filter(scenario == paste0(location_target, " standard")),
                aes(ymin = lower, ymax = upper, fill = scenario),
                alpha = 0.15, color = NA, show.legend = FALSE) +
    geom_line(linewidth = 1) +
    annotate("text",
             x     = 1992,
             y     = y_top,                                   # ← 动态，不挡线
             label = sprintf("Max difference at 2040: %.2f%%", max_rel_diff),
             size = 3.5, hjust = 0, color = "grey30", fontface = "italic") +
    scale_color_manual(values = setNames(
      c("#E64B35", "#4DBBD5", "#00A087"),
      c(paste0(location_target, " standard"), "WHO standard", "Equal weight")),
      name = "Standard population") +
    scale_fill_manual(values = setNames(
      "#E64B35", paste0(location_target, " standard"))) +
    scale_linetype_manual(values = setNames(
      c("solid", "dashed", "dotted"),
      c(paste0(location_target, " standard"), "WHO standard", "Equal weight")),
      name = "Standard population") +
    scale_x_continuous(breaks = seq(1990, 2040, by = 10)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +   # 上方留更多空间
    labs(x = "Year",
         y = "Age-standardized incidence rate (per 100,000)",
         title = paste0(location_target, " · Sensitivity analysis")) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor.y = element_blank()
    )
  
  cat("\n>>> 显示：", output_prefix, "敏感性分析图\n")
  print(p_sens)
  Sys.sleep(2)
  
  ggsave(paste0(output_prefix, " BAPC sensitivity.pdf"),
         p_sens, width = 8, height = 5.5, dpi = 300)
  ggsave(paste0(output_prefix, " BAPC sensitivity.tiff"),
         p_sens, width = 8, height = 5.5, dpi = 600, compression = "lzw")
  
  cat("  已保存：", output_prefix, " BAPC sensitivity.pdf / .tiff\n", sep = "")
  
  # ---------- 11. 输出 CSV ----------
  main_csv <- data.frame(res_main$result@agestd.rate)
  write.csv(main_csv,
            file = paste0(output_prefix, " Both ageStdRate.csv"),
            row.names = TRUE)
  
  cat("  已保存：", output_prefix, " Both ageStdRate.csv\n", sep = "")
  
  # ---------- 12. 返回 ----------
  return(list(
    bapc_object = res_main$result,
    sensitivity = p_sens,
    data_main   = res_main$data,
    data_all    = all_res,
    max_diff    = max_rel_diff
  ))
}


# =========================================================
# 跑 China 和 Global
# =========================================================

res_china <- run_bapc_location(
  location_target = "China",
  stand_file      = "age_stand China female 15-49.csv",
  output_prefix   = "China"
)

res_global <- run_bapc_location(
  location_target = "Global",
  stand_file      = "age_stand Global female 15-49.csv",
  output_prefix   = "Global"
)


# =========================================================
# 对比图：China vs Global（主分析）
# =========================================================

compare_df <- bind_rows(
  res_china$data_main  %>% mutate(location = "China"),
  res_global$data_main %>% mutate(location = "Global")
)

p_compare <- ggplot(compare_df, aes(x = year, y = rate,
                                    color = location,
                                    fill = location)) +
  geom_vline(xintercept = 2023.5, linetype = "dotted",
             color = "grey50", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("China" = "#E64B35",
                                "Global" = "#4DBBD5"),
                     name = "Location") +
  scale_fill_manual(values = c("China" = "#E64B35",
                               "Global" = "#4DBBD5"),
                    name = "Location") +
  scale_x_continuous(breaks = seq(1990, 2040, by = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Year",
       y = "Age-standardized incidence rate (per 100,000)") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  )

cat("\n>>> 显示：China vs Global 对比图\n")
print(p_compare)
Sys.sleep(2)

ggsave("China vs Global comparison.pdf", p_compare,
       width = 8, height = 5.5, dpi = 300)
ggsave("China vs Global comparison.tiff", p_compare,
       width = 8, height = 5.5, dpi = 600, compression = "lzw")


# =========================================================
# 两张敏感性分析拼成一张 2-panel 大图
# =========================================================

p_china_sens  <- res_china$sensitivity  + labs(title = NULL)
p_global_sens <- res_global$sensitivity + labs(title = NULL)

p_sens_combined <- p_china_sens / p_global_sens +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 16))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

cat("\n>>> 显示：China + Global 敏感性分析合并图\n")
print(p_sens_combined)
Sys.sleep(2)

ggsave("Sensitivity analysis China and Global combined.pdf",
       p_sens_combined, width = 10, height = 11, dpi = 300)
ggsave("Sensitivity analysis China and Global combined.tiff",
       p_sens_combined, width = 10, height = 11, dpi = 600, compression = "lzw")


# =========================================================
# 汇总表
# =========================================================

summary_table <- bind_rows(
  res_china$data_main  %>% mutate(location = "China"),
  res_global$data_main %>% mutate(location = "Global")
) %>%
  filter(year %in% c(1990, 2000, 2010, 2023, 2030, 2040)) %>%
  select(location, year, rate, lower, upper) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  pivot_wider(id_cols = year, names_from = location,
              values_from = c(rate, lower, upper))

cat("\n============================================\n")
cat("  China vs Global 关键年份对比（per 100,000）\n")
cat("============================================\n")
print(summary_table)

cat("\n============================================\n")
cat("  敏感性分析最大相对差异\n")
cat("============================================\n")
cat("  China  :", round(res_china$max_diff, 2),  "%\n")
cat("  Global :", round(res_global$max_diff, 2), "%\n")
cat("============================================\n")

cat("\n所有文件已保存到：", getwd(), "\n")
cat("\n生成的图（共 6 张）：\n")
cat("  1. China BAPC classic.pdf / .tiff\n")
cat("  2. China BAPC sensitivity.pdf / .tiff\n")
cat("  3. Global BAPC classic.pdf / .tiff\n")
cat("  4. Global BAPC sensitivity.pdf / .tiff\n")
cat("  5. China vs Global comparison.pdf / .tiff\n")
cat("  6. Sensitivity analysis China and Global combined.pdf / .tiff\n")
cat("\n生成的 CSV：\n")
cat("  - China Both ageStdRate.csv\n")
cat("  - Global Both ageStdRate.csv\n")