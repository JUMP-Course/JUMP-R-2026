# ============================================
# 1. ELD评分直方图 + 密度曲线
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

p1 <- ggplot(cleaned_data, aes(x = ELD_total_score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "steelblue", alpha = 0.6, color = "white") +
  geom_density(color = "darkred", linewidth = 1.2) +
  geom_vline(aes(xintercept = median(ELD_total_score, na.rm = TRUE)),
             linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = median(cleaned_data$ELD_total_score, na.rm = TRUE) + 2, 
           y = 0.12, label = paste("Median =", round(median(cleaned_data$ELD_total_score, na.rm = TRUE), 1)),
           size = 4, color = "black") +
  labs(
    title = "Distribution of EAT-Lancet Diet Score",
    x = "ELD Score",
    y = "Density"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p1)

# ============================================
# 2. 心衰患病率与ELD评分关系（分组散点图）
# ============================================

# 按ELD评分分组计算患病率
eld_groups <- cleaned_data %>%
  mutate(ELD_group = cut(ELD_total_score, 
                         breaks = seq(0, 40, by = 5),
                         include.lowest = TRUE)) %>%
  group_by(ELD_group) %>%
  summarise(
    n = n(),
    hf_count = sum(HF, na.rm = TRUE),
    prevalence = hf_count / n * 100,
    se = sqrt(prevalence * (100 - prevalence) / n)
  ) %>%
  filter(n >= 10)

p2 <- ggplot(eld_groups, aes(x = as.numeric(gsub("\\(|\\]|,.*", "", ELD_group)) + 2.5, 
                             y = prevalence)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = prevalence - se, ymax = prevalence + se), 
                width = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "red", alpha = 0.2) +
  geom_hline(yintercept = mean(cleaned_data$HF, na.rm = TRUE) * 100, 
             linetype = "dashed", color = "gray50") +
  annotate("text", x = 35, y = mean(cleaned_data$HF, na.rm = TRUE) * 100 + 0.5,
           label = "Overall Prevalence", size = 3, color = "gray50") +
  labs(
    title = "Heart Failure Prevalence by ELD Score",
    x = "ELD Score",
    y = "Prevalence of Heart Failure (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p2)

# ============================================
# 3. 连续变量的基线特征图（按ELD四分位数分组）
# ============================================

# 准备数据
baseline_cont <- cleaned_data %>%
  select(ELD_quartile, Age, BMI, INDFMPIR) %>%
  pivot_longer(cols = -ELD_quartile, names_to = "Variable", values_to = "Value")

p3 <- ggplot(baseline_cont, aes(x = ELD_quartile, y = Value, fill = ELD_quartile)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "Baseline Characteristics by ELD Score Quartiles",
    x = "ELD Score Quartile",
    y = "Value"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11)
  )

print(p3)
