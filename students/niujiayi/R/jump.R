# 研究主题：成年女性饮酒频次与乳腺癌患病的相关性分析
# 数据来源：NHANES 美国全国健康与营养检查调查
install.packages(c("gtsummary", "flextable", "scales"))
library(tidyverse)    # 数据清洗+可视化全套工具（含dplyr、ggplot2）
library(haven)        # 读取NHANES原始.xpt格式数据
library(gtsummary)    # 生成符合学术规范的基线表与回归表
library(scales)       # 坐标轴格式美化（百分比等）
library(flextable)    # 表格导出为Word文档

# 2. 数据读取与合并
demo <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_DEMO.xpt")
alq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_ALQ.xpt")
mcq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_MCQ.xpt")
bmx  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_BMX.xpt")
all_data <- demo %>%
  left_join(alq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN")
cat("① 原始合并总样本量：", nrow(all_data), "人\n")

# 3. 样本筛选
female <- all_data %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 18)
cat("② 筛选成年女性后样本量：", nrow(female), "人\n")
female <- female %>% 
  mutate(
    breast_cancer = case_when(
      MCQ230A == 14 | MCQ230B == 14 | MCQ230C == 14 ~ 1,
      TRUE ~ 0
    )
  )
#  剔除变量缺失样本
female_clean <- female %>% 
  filter(!is.na(ALQ121), !is.na(breast_cancer)) %>% 
  mutate(
    ALQ121 = ifelse(ALQ121 %in% c(77, 99), NA, ALQ121)
  ) %>% 
  filter(!is.na(ALQ121))
cat("③ 剔除核心变量缺失后样本量：", nrow(female_clean), "人\n")

female_clean <- female_clean %>% 
  mutate(
    # 结局变量
    breast_cancer = factor(
      breast_cancer,
      levels = c(0, 1),
      labels = c("未患乳腺癌", "患乳腺癌")
    ),  
    # 饮酒频次
    ALQ121 = factor(
      ALQ121,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0),
      labels = c(
        "每天饮酒",
        "几乎每天饮酒",
        "每周3-4次",
        "每周2次",
        "每周1次",
        "每月2-3次",
        "每月1次",
        "过去一年7-11次",
        "过去一年3-6次",
        "过去一年1-2次",
        "从不饮酒"
      )
    )
  )
# 4.合并分组
female_clean <- female_clean %>% 
  mutate(
    drink_group = case_when(
      ALQ121 == "从不饮酒" ~ "从不饮酒",
      ALQ121 %in% c("过去一年1-2次", "过去一年3-6次", "过去一年7-11次", 
                    "每月1次", "每月2-3次") ~ "少量饮酒",
      ALQ121 %in% c("每周1次", "每周2次", "每周3-4次") ~ "中等饮酒",
      ALQ121 %in% c("几乎每天饮酒", "每天饮酒") ~ "频繁饮酒"
    ) %>% 
      factor(levels = c("从不饮酒", "少量饮酒", "中等饮酒", "频繁饮酒"))
  )

# 5.导出数据
analysis_data <- female_clean %>% 
  select(SEQN, RIDAGEYR, ALQ121, drink_group, breast_cancer, BMXBMI)
write.csv(
  analysis_data, 
  "D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_最终清洗数据.csv", 
  row.names = FALSE
)
cat("\n===== 最终缺失值核查 =====\n")
print(colSums(is.na(analysis_data)))

# 6.基线特征表
baseline_table <- analysis_data %>%
  select(-SEQN) %>%
  tbl_summary(
    by = breast_cancer,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ c(1, 1), all_categorical() ~ c(0, 1)),
    label = list(
      RIDAGEYR ~ "年龄（岁）",
      BMXBMI ~ "体重指数（kg/m²）",
      drink_group ~ "饮酒频次分组"
    ),
    missing = "no"  # 不显示缺失值行
  ) %>%
  add_overall() %>%       # 添加总人群列
  add_p() %>%             # 自动计算组间比较P值
  modify_header(label = "特征") %>%
  modify_caption("表1 研究对象基线特征表")
baseline_table
# 导出为Word
baseline_table %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/基线特征表.docx")
# 导出为CSV
write.csv(
  baseline_table$table_body,
  "D:/JUMP-R-2026/students/niujiayi/doc/基线特征表.csv",
  row.names = FALSE
)
# 卡方检验
chisq_result <- chisq.test(table(analysis_data$drink_group, analysis_data$breast_cancer))
print("===== 饮酒分组与乳腺癌 卡方检验结果 =====")
print(chisq_result)

# 独立样本t检验
t_result <- t.test(RIDAGEYR ~ breast_cancer, data = analysis_data)
print("===== 年龄组间比较 t检验结果 =====")
print(t_result)

t_bmi <- t.test(BMXBMI ~ breast_cancer, data = analysis_data)
print("===== BMI组间比较 t检验结果 =====")
print(t_bmi)

# 7. 图表
common_theme <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )
#  不同患病状态的年龄分布密度图
p1 <- ggplot(analysis_data, aes(x = RIDAGEYR, fill = breast_cancer)) +
  geom_density(alpha = 0.6) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的年龄分布",
    x = "年龄（岁）",
    y = "密度",
    fill = "患病状态"
  ) +
  common_theme
print(p1)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图1_年龄分布密度图.png",
  plot = p1, width = 8, height = 5, dpi = 300
)
#  不同患病状态的饮酒频次构成比 
p2 <- ggplot(analysis_data %>% filter(!is.na(drink_group)), 
             aes(x = breast_cancer, fill = drink_group)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的饮酒频次构成",
    x = "患病状态",
    y = "组内占比",
    fill = "饮酒频次分组"
  ) +
  common_theme
print(p2)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图2_饮酒频次构成比.png",
  plot = p2, width = 8, height = 6, dpi = 300
)
#  不同患病状态的BMI分布箱线图 
p3 <- ggplot(analysis_data, aes(x = breast_cancer, y = BMXBMI, fill = breast_cancer)) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.color = "red") +
  # 白色菱形标记组内均值
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的BMI分布",
    x = "患病状态",
    y = "体重指数 BMI (kg/m²)"
  ) +
  common_theme +
  theme(legend.position = "none") 
print(p3)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图3_BMI分布箱线图.png",
  plot = p3, width = 7, height = 5, dpi = 300
)
# 8. 单因素logistic回归分析
install.packages("broom.helpers")
uni_reg_table <- analysis_data %>%
  select(-SEQN, -ALQ121) %>%
  tbl_uvregression(
    method = glm,
    y = breast_cancer,
    method.args = list(family = binomial),
    exponentiate = TRUE,  
    label = list(
      RIDAGEYR ~ "年龄（岁）",
      BMXBMI ~ "体重指数（kg/m²）",
      drink_group ~ "饮酒频次分组"
    )
  ) %>%
  modify_header(label = "特征") %>%
  modify_caption("表2 各因素与乳腺癌患病的单因素logistic回归分析")
uni_reg_table
# 导出为Word
uni_reg_table %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/单因素回归表.docx")