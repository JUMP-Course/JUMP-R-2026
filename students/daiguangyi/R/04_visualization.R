
# ==================== 第1部分：加载R包 ====================

library(dplyr)
library(ggplot2)
library(scales)


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

# 创建二分类因变量
df_clean$y <- ifelse(df_clean$LBXHA == "Positive", 1, 0)


# ==================== 第3部分：图1 抗体阴阳性人数分布 ====================

fig1 <- ggplot(df_clean, aes(x = period, fill = LBXHA)) +
  geom_bar(position = "stack", width = 0.6) +
  labs(title = "图1 不同时期甲肝抗体阳性与阴性人数分布",
       x = "调查时期", y = "人数", fill = "抗体结果") +
  scale_fill_manual(values = c("Positive" = "#E64B35", "Negative" = "#4DBBD5")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("图1_抗体人数分布.png", fig1, width = 7, height = 5, dpi = 300)


# ==================== 第4部分：图2 年龄构成对比 ====================

fig2 <- ggplot(df_clean, aes(x = age_group, fill = period)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "图2 不同时期年龄构成对比",
       x = "年龄组", y = "构成比", fill = "调查时期") +
  scale_fill_manual(values = c("2017-2020" = "#3C5488", "2021-2023" = "#00A087")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("图2_年龄构成对比.png", fig2, width = 7, height = 5, dpi = 300)


# ==================== 第5部分：图3 标准化阳性率对比 ====================

# 计算标准化率
df_temp <- df_clean %>%
  mutate(period = as.character(period),
         age_group = as.character(age_group))

age_rates <- df_temp %>%
  group_by(period, age_group) %>%
  summarise(n = n(), pos = sum(LBXHA == "Positive"), rate = pos / n, .groups = "drop")

std_pop <- df_temp %>%
  group_by(age_group) %>%
  summarise(std_n = n(), .groups = "drop")

age_std <- age_rates %>%
  left_join(std_pop, by = "age_group") %>%
  group_by(period) %>%
  summarise(adjusted_rate = sum(rate * std_n) / sum(std_n) * 100, .groups = "drop")

raw_rates <- df_temp %>%
  group_by(period) %>%
  summarise(raw_rate = round(mean(LBXHA == "Positive") * 100, 2), .groups = "drop")

final_rate <- left_join(raw_rates, age_std, by = "period")

# 绘图
fig3 <- ggplot(final_rate, aes(x = period, y = adjusted_rate, fill = period)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(adjusted_rate, 1), "%")), 
            vjust = -0.8, size = 5, fontface = "bold") +
  labs(title = "图3 两周期年龄标准化甲肝抗体阳性率对比",
       x = "调查时期", y = "年龄标准化阳性率 (%)") +
  scale_fill_manual(values = c("2017-2020" = "#3C5488", "2021-2023" = "#00A087")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank())

ggsave("图3_标准化率对比.png", fig3, width = 5, height = 5, dpi = 300)


# ==================== 第6部分：图4 抗体阴阳性构成比 ====================

fig4 <- ggplot(df_clean, aes(x = period, fill = LBXHA)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "图4 不同时期甲肝抗体阳性与阴性构成比",
       x = "调查时期", y = "构成比", fill = "抗体结果") +
  scale_fill_manual(values = c("Positive" = "#E64B35", "Negative" = "#4DBBD5")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("图4_抗体构成比.png", fig4, width = 7, height = 5, dpi = 300)


# ==================== 第7部分：图5 不同性别阳性率 ====================

gender_rate <- df_clean %>%
  group_by(RIAGENDR) %>%
  summarise(rate = round(mean(LBXHA == "Positive") * 100, 1), .groups = "drop")

fig5 <- ggplot(gender_rate, aes(x = RIAGENDR, y = rate, fill = RIAGENDR)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(rate, "%")), vjust = -0.5, size = 4) +
  labs(title = "图5 不同性别甲肝抗体阳性率",
       x = "性别", y = "阳性率 (%)") +
  scale_fill_manual(values = c("Male" = "#3C5488", "Female" = "#00A087")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank())

ggsave("图5_性别阳性率.png", fig5, width = 5, height = 5, dpi = 300)


# ==================== 第8部分：图6 不同种族阳性率 ====================

race_rate <- df_clean %>%
  group_by(RIDRETH3) %>%
  summarise(rate = round(mean(LBXHA == "Positive") * 100, 1), .groups = "drop") %>%
  arrange(desc(rate))

fig6 <- ggplot(race_rate, aes(x = reorder(RIDRETH3, rate), y = rate, fill = RIDRETH3)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(rate, "%")), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "图6 不同种族甲肝抗体阳性率",
       x = "种族", y = "阳性率 (%)") +
  scale_fill_manual(values = c("#E64B35", "#4DBBD5", "#3C5488", "#00A087", "#F39C12", "#8491B4")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_blank())

ggsave("图6_种族阳性率.png", fig6, width = 7, height = 5, dpi = 300)


# ==================== 第9部分：图7 不同教育水平阳性率 ====================

edu_rate <- df_clean %>%
  group_by(DMDEDUC2) %>%
  summarise(rate = round(mean(LBXHA == "Positive") * 100, 1), .groups = "drop")

fig7 <- ggplot(edu_rate, aes(x = DMDEDUC2, y = rate, fill = DMDEDUC2)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(rate, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "图7 不同教育水平甲肝抗体阳性率",
       x = "教育程度", y = "阳性率 (%)") +
  scale_fill_manual(values = c("#3C5488", "#4DBBD5", "#00A087", "#F39C12", "#E64B35")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("图7_教育水平阳性率.png", fig7, width = 7, height = 5, dpi = 300)


# ==================== 第10部分：图8 分年龄组阳性率 ====================

age_period_rate <- df_clean %>%
  group_by(period, age_group) %>%
  summarise(rate = round(mean(LBXHA == "Positive") * 100, 1), .groups = "drop")

fig8 <- ggplot(age_period_rate, aes(x = age_group, y = rate, fill = period)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = paste0(rate, "%")), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +
  labs(title = "图8 各年龄组分时期抗体阳性率对比",
       x = "年龄组", y = "阳性率 (%)", fill = "调查时期") +
  scale_fill_manual(values = c("2017-2020" = "#3C5488", "2021-2023" = "#00A087")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

ggsave("图8_分年龄组阳性率.png", fig8, width = 7, height = 5, dpi = 300)


# ==================== 第11部分：图9 年龄组阳性率趋势 ====================

fig9 <- ggplot(age_period_rate, aes(x = age_group, y = rate, color = period, group = period)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "图9 各年龄组甲肝抗体阳性率变化趋势",
       x = "年龄组", y = "阳性率 (%)", color = "调查时期") +
  scale_color_manual(values = c("2017-2020" = "#3C5488", "2021-2023" = "#00A087")) +
  ylim(0, 70) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

ggsave("图9_年龄组阳性率趋势.png", fig9, width = 7, height = 5, dpi = 300)


# ==================== 第12部分：图10 年龄与阳性率散点趋势 ====================

# 按年龄计算阳性率
age_plot <- df_clean %>%
  group_by(RIDAGEYR) %>%
  summarise(rate = mean(LBXHA == "Positive") * 100, n = n(), .groups = "drop") %>%
  filter(n >= 10)  # 过滤样本量过少的年龄

fig10 <- ggplot(age_plot, aes(x = RIDAGEYR, y = rate)) +
  geom_smooth(method = "loess", se = TRUE, color = "#2E86C1", fill = "#2E86C1", alpha = 0.2) +
  geom_point(size = 1, alpha = 0.5, color = "gray30") +
  labs(title = "图10 年龄与甲肝抗体阳性率的关系",
       x = "年龄（岁）", y = "阳性率（%）") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("图10_年龄阳性率趋势.png", fig10, width = 7, height = 5, dpi = 300)


# ==================== 第13部分：完成 ====================

cat("\n========== 可视化图表生成完成 ==========\n")
cat("生成图片清单：\n")
cat("  图1_抗体人数分布.png\n")
cat("  图2_年龄构成对比.png\n")
cat("  图3_标准化率对比.png\n")
cat("  图4_抗体构成比.png\n")
cat("  图5_性别阳性率.png\n")
cat("  图6_种族阳性率.png\n")
cat("  图7_教育水平阳性率.png\n")
cat("  图8_分年龄组阳性率.png\n")
cat("  图9_年龄组阳性率趋势.png\n")
cat("  图10_年龄阳性率趋势.png\n")