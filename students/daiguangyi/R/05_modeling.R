
# ==================== 第1部分：加载R包 ====================

library(dplyr)
library(ggplot2)


# ==================== 第2部分：数据读取与清洗 ====================

# 读取数据
df_2017 <- read.csv("D:/R-data/2017-2020合并数据.csv", 
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE)
df_2021 <- read.csv("D:/R-data/2021-2023合并数据.csv", 
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE)

# 标记时期并合并
df_2017$period <- "2017-2020"
df_2021$period <- "2021-2023"
df_all <- bind_rows(df_2017, df_2021)

# 数据清洗
df_clean <- df_all %>%
  filter(RIDAGEYR >= 18) %>%
  filter(!is.na(LBXHA) & LBXHA %in% c(1, 2)) %>%
  filter(!is.na(RIAGENDR) & RIAGENDR != "") %>%
  filter(!is.na(DMDEDUC2) & DMDEDUC2 %in% 1:5) %>%
  filter(!is.na(RIDRETH3) & RIDRETH3 != "")

# 变量编码
df_clean <- df_clean %>%
  mutate(
    age_group = cut(RIDAGEYR, 
                    breaks = c(18, 45, 65, Inf), 
                    labels = c("18-44", "45-64", "65+"), 
                    right = FALSE),
    RIDRETH3 = case_when(
      RIDRETH3 == 6 ~ 5,
      RIDRETH3 == 7 ~ 6,
      TRUE ~ RIDRETH3
    ),
    period = factor(period, levels = c("2017-2020", "2021-2023")),
    RIAGENDR = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    DMDEDUC2 = factor(DMDEDUC2, levels = 1:5, 
                      labels = c("Primary", "Middle", "High", "College", "University")),
    RIDRETH3 = factor(RIDRETH3, levels = 1:6,
                      labels = c("Mexican", "Other Hispanic", "White", "Black", "Asian", "Other")),
    LBXHA = factor(LBXHA, levels = c(1, 2), labels = c("Positive", "Negative"))
  )

# 创建二分类因变量（1=阳性, 0=阴性）
df_clean$y <- ifelse(df_clean$LBXHA == "Positive", 1, 0)


# ==================== 第3部分：卡方检验 ====================

cat("========================================\n")
cat("         卡方检验：两周期抗体阳性率比较\n")
cat("========================================\n\n")

# 四格表
tab <- table(df_clean$period, df_clean$LBXHA)
cat("四格表：\n")
print(tab)
cat("\n")

# 卡方检验
chi_result <- chisq.test(tab)
cat("卡方检验结果：\n")
cat("  χ² =", round(chi_result$statistic, 2), "\n")
cat("  df =", chi_result$parameter, "\n")
cat("  P =", format(chi_result$p.value, scientific = TRUE, digits = 3), "\n")

cat("\n========================================\n\n")


# ==================== 第4部分：Logistic回归 ====================

cat("========================================\n")
cat("       多因素Logistic回归分析\n")
cat("========================================\n\n")

# 拟合模型
logit_model <- glm(
  y ~ period + RIAGENDR + age_group + RIDRETH3 + DMDEDUC2,
  family = binomial(link = "logit"),
  data = df_clean,
  control = glm.control(maxit = 100, epsilon = 1e-6)
)

# 模型摘要
cat("模型摘要：\n")
print(summary(logit_model))
cat("\n")

# 计算OR及95%CI
exp_coef <- exp(coef(logit_model))
exp_ci <- exp(confint.default(logit_model, level = 0.95))

reg_result <- cbind(
  回归系数 = coef(logit_model),
  标准误 = summary(logit_model)$coefficients[, 2],
  OR = exp_coef,
  `95%CI下限` = exp_ci[, 1],
  `95%CI上限` = exp_ci[, 2],
  P值 = summary(logit_model)$coefficients[, 4]
)

cat("OR及95%置信区间：\n")
print(round(reg_result, 4))

cat("\n========================================\n\n")


# ==================== 第5部分：森林图 ====================

cat("========================================\n")
cat("           生成森林图\n")
cat("========================================\n\n")

# 准备森林图数据
forest_data <- data.frame(
  variable = c(
    "2021-2023 vs 2017-2020",
    "女性 vs 男性",
    "45-64岁 vs 18-44岁",
    "65+岁 vs 18-44岁",
    "其他西班牙裔 vs 墨西哥裔",
    "白人 vs 墨西哥裔",
    "黑人 vs 墨西哥裔",
    "亚裔 vs 墨西哥裔",
    "其他种族 vs 墨西哥裔",
    "初中 vs 小学",
    "高中 vs 小学",
    "大学 vs 小学",
    "本科及以上 vs 小学"
  ),
  OR = c(0.6774, 0.9851, 0.8962, 0.9540, 1.0060, 1.0102, 
         0.9795, 1.0978, 1.0934, 1.0986, 1.0581, 1.0650, 1.1130),
  CI_lower = c(0.6266, 0.9145, 0.8203, 0.8675, 0.8514, 0.8776, 
               0.8438, 0.9201, 0.8932, 0.9112, 0.8926, 0.9009, 0.9381),
  CI_upper = c(0.7323, 1.0611, 0.9790, 1.0491, 1.1888, 1.1629, 
               1.1371, 1.3099, 1.3384, 1.3245, 1.2543, 1.2590, 1.3205),
  P值 = c(0.0000, 0.6919, 0.0151, 0.3312, 0.9436, 0.8876, 
         0.7858, 0.3001, 0.3870, 0.3243, 0.5151, 0.4606, 0.2198)
)

# 标记显著性
forest_data$significance <- ifelse(forest_data$P值 < 0.05, 
                                   "Significant", "Not Significant")

# 按OR值排序
forest_data <- forest_data[order(forest_data$OR), ]
forest_data$variable <- factor(forest_data$variable, 
                               levels = forest_data$variable)

# 绘制森林图
forest_plot <- ggplot(forest_data, aes(x = OR, y = variable)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                 height = 0.2, color = "gray50", size = 0.8) +
  geom_point(aes(color = significance), size = 4) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  scale_x_continuous(
    trans = "log10",
    breaks = c(0.5, 0.7, 1, 1.3, 1.6),
    labels = c("0.5", "0.7", "1", "1.3", "1.6"),
    limits = c(0.4, 1.8)
  ) +
  labs(
    x = "Odds Ratio (95% CI)",
    y = "",
    title = "甲肝抗体阳性影响因素的多因素Logistic回归",
    color = "统计显著性"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c("Not Significant" = "#2E86C1", "Significant" = "#E74C3C"),
    labels = c("Not Significant" = "无统计学意义 (P ≥ 0.05)", 
               "Significant" = "有统计学意义 (P < 0.05)")
  )

# 保存森林图
ggsave("森林图_Logistic回归.png", forest_plot, width = 11, height = 8, dpi = 300)

cat("✅ 森林图已保存: 森林图_Logistic回归.png\n")
cat("\n========================================\n")


# ==================== 第6部分：输出结果汇总 ====================

cat("\n")
cat("========================================\n")
cat("         统计分析结果汇总\n")
cat("========================================\n\n")

cat("【1】卡方检验\n")
cat("    两周期抗体阳性率存在显著差异（χ² =", round(chi_result$statistic, 2), 
    ", P < 0.001）\n\n")

cat("【2】多因素Logistic回归\n")
cat("    独立保护因素：\n")
cat("    - 2021-2023周期（OR = 0.677, 95%CI: 0.627-0.732, P < 0.001）\n")
cat("    - 45-64岁年龄组（OR = 0.896, 95%CI: 0.820-0.979, P = 0.015）\n\n")

cat("【3】森林图\n")
cat("    已生成森林图：森林图_Logistic回归.png\n")

cat("\n========================================\n")
cat("========== 基础统计分析完成 ==========\n")
  