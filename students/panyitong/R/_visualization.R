install.packages("brglm2", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(brglm2)
library(dplyr)
library(ggplot2)
library(survey)

clean_data <- readRDS("clean_data.rds")
clean_data$smoking <- relevel(factor(clean_data$smoking), ref = "Never")
levels(factor(clean_data$smoking))

# 设置 survey 设计对象（用于加权）
design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = clean_data,
  nest = TRUE)

# 图1：不同吸烟状态的高血压患病率（加权）

# 计算加权患病率
htn_by_smoking <- svyby(~hypertension, ~smoking, design, svymean, na.rm = TRUE)
htn_by_smoking <- as.data.frame(htn_by_smoking)
colnames(htn_by_smoking) <- c("smoking", "hypertension_rate", "SE")

# 绘图
p1 <- ggplot(htn_by_smoking, aes(x = smoking, y = hypertension_rate * 100, fill = smoking)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = (hypertension_rate - 1.96 * SE) * 100,
                    ymax = (hypertension_rate + 1.96 * SE) * 100),
                width = 0.2) +
  labs(title = "不同吸烟状态的高血压患病率",
       x = "吸烟状态",
       y = "高血压患病率 (%)",
       caption = "误差线表示 95% 置信区间；加权分析") +
  scale_fill_manual(values = c("Never" = "#2E86AB", "Former" = "#A23B72", "Current" = "#F18F01")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# 保存图1
ggsave("C:/Users/lenovo/Documents/GitHub/JUMP-R-2026/students/panyitong/figures/figure1_htn_by_smoking.png", p1, width = 8, height = 6, dpi = 300)

# 显示图片

print(p1)


# 图2：加权Logistic回归森林图

# 带惩罚拟合，CI全部收敛为有限正数

fit_model <- svyglm(
  hypertension ~ smoking + age_group + gender + race + bmi_group + education + pir_group,
  design = design,
  family = binomial(link = "logit"),
  method = "brglmFit")

# 用 summary 提取结果
summ <- summary(fit_model)

# 提取系数、标准误、p 值
coef_table <- as.data.frame(summ$coefficients)

# 计算 OR
OR <- exp(coef_table$Estimate)

# 手动计算 95% CI（基于标准误）
LCL <- exp(coef_table$Estimate - 1.96 * coef_table$`Std. Error`)
UCL <- exp(coef_table$Estimate + 1.96 * coef_table$`Std. Error`)

# 合并成数据框
or_data <- data.frame(
  variable = rownames(coef_table),
  OR = OR,
  LCI = LCL,
  UCI = UCL)

# 去掉截距
or_data <- or_data[or_data$variable != "(Intercept)", ]

# 查看结果
print(or_data)

# 清理变量名
or_data$variable <- gsub("smoking", "", or_data$variable)
or_data$variable <- gsub("age_group", "", or_data$variable)
or_data$variable <- gsub("gender", "", or_data$variable)
or_data$variable <- gsub("race", "", or_data$variable)
or_data$variable <- gsub("bmi_group", "", or_data$variable)
or_data$variable <- gsub("education", "", or_data$variable)
or_data$variable <- gsub("pir_group", "", or_data$variable)

# 添加分组标签
or_data$group <- c(
  rep("吸烟 (ref: Never)", 2),
  rep("年龄 (ref: 20-39)", 2),
  "性别 (ref: Female)",
  rep("种族(ref: Non-Hispanic White)", 5),
  rep("BMI (ref: Normal)", 3),
  rep("教育 (ref: College graduate or higher)", 4),
  rep("收入 (ref: High)", 2)
)

# 绘图

p2 <- ggplot(or_data, aes(x = OR, y = variable, color = group)) +
  geom_point(size = 2.8) +
  geom_errorbar(
    aes(xmin = LCI, xmax = UCI),
    height = 0.22,
    linewidth = 0.8,
    orientation = "y") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.7) +
  scale_x_log10(
    labels = scales::comma_format(accuracy = 0.1),
    breaks = c(0.5, 1, 2, 4, 8, 16)) +
  coord_cartesian(xlim = c(0.5, 16)) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "",
    color = "变量分组",
    title = "各因素与高血压关联的多因素加权Logistic回归森林图") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom")

print(p2)

# 保存
ggsave("C:/Users/lenovo/Documents/GitHub/JUMP-R-2026/students/panyitong/figures/figure2_forest_plot.png", p2, width = 10, height = 8, dpi = 300)
