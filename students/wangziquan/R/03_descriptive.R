# =========================================================
# 03_descriptive.R
# 项目：基于 NHANES 数据的成年人 BMI 与高血压患病风险关联分析
# 作者：王子铨
# 目的：

# 1. 生成总体描述性统计表
# 2. 生成按BMI分组的Table 1
# 3. 生成不同BMI组高血压患病率表
# 4. 生成描述性图表
# =========================================================


# ---------------------------------------------------------
# 0. 加载R包
# ---------------------------------------------------------

library(tidyverse)
library(janitor)


# ---------------------------------------------------------
# 1. 创建输出文件夹
# ---------------------------------------------------------

dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)


# ---------------------------------------------------------
# 2. 读取6月10日清洗后的数据
# ---------------------------------------------------------

data_final <- read_csv("output/nhanes_bmi_hypertension_clean.csv")

dim(data_final)
glimpse(data_final)


# ---------------------------------------------------------
# 3. 检查最终分析样本是否为空
# ---------------------------------------------------------

nrow(data_final)

if (nrow(data_final) == 0) {
  stop("data_final 的样本量为0，请先回到 02_cleaning.R 检查纳入排除标准。")
}


# ---------------------------------------------------------
# 4. 整理变量类型
# ---------------------------------------------------------

data_final <- data_final %>%
  mutate(
    bmi_group_clean = factor(
      bmi_group_clean,
      levels = c("Normal", "Underweight", "Overweight", "Obesity")
    ),
    
    sex_clean = factor(sex_clean),
    smoking_model = factor(smoking_model),
    
    hypertension_main = factor(
      hypertension_main,
      levels = c(0, 1),
      labels = c("No hypertension", "Hypertension")
    )
  )


# ---------------------------------------------------------
# 5. 生成总体描述性统计表
# ---------------------------------------------------------

table1_overall <- data_final %>%
  summarise(
    total_n = n(),
    
    age_mean = round(mean(age, na.rm = TRUE), 2),
    age_sd = round(sd(age, na.rm = TRUE), 2),
    age_median = round(median(age, na.rm = TRUE), 2),
    age_q1 = round(quantile(age, 0.25, na.rm = TRUE), 2),
    age_q3 = round(quantile(age, 0.75, na.rm = TRUE), 2),
    
    bmi_mean = round(mean(bmi, na.rm = TRUE), 2),
    bmi_sd = round(sd(bmi, na.rm = TRUE), 2),
    bmi_median = round(median(bmi, na.rm = TRUE), 2),
    bmi_q1 = round(quantile(bmi, 0.25, na.rm = TRUE), 2),
    bmi_q3 = round(quantile(bmi, 0.75, na.rm = TRUE), 2),
    
    female_n = sum(sex_clean == "Female", na.rm = TRUE),
    female_pct = round(mean(sex_clean == "Female", na.rm = TRUE) * 100, 2),
    
    ever_smoker_n = sum(smoking_model == "Ever smoker", na.rm = TRUE),
    ever_smoker_pct = round(mean(smoking_model == "Ever smoker", na.rm = TRUE) * 100, 2),
    
    hypertension_n = sum(hypertension_main == "Hypertension", na.rm = TRUE),
    hypertension_pct = round(mean(hypertension_main == "Hypertension", na.rm = TRUE) * 100, 2)
  )

table1_overall

write_csv(table1_overall, "tables/table1_overall.csv")


# ---------------------------------------------------------
# 6. 生成按BMI分组的Table 1
# ---------------------------------------------------------

table1_by_bmi <- data_final %>%
  group_by(bmi_group_clean) %>%
  summarise(
    n = n(),
    
    age_mean = round(mean(age, na.rm = TRUE), 2),
    age_sd = round(sd(age, na.rm = TRUE), 2),
    age_median = round(median(age, na.rm = TRUE), 2),
    age_q1 = round(quantile(age, 0.25, na.rm = TRUE), 2),
    age_q3 = round(quantile(age, 0.75, na.rm = TRUE), 2),
    
    bmi_mean = round(mean(bmi, na.rm = TRUE), 2),
    bmi_sd = round(sd(bmi, na.rm = TRUE), 2),
    bmi_median = round(median(bmi, na.rm = TRUE), 2),
    bmi_q1 = round(quantile(bmi, 0.25, na.rm = TRUE), 2),
    bmi_q3 = round(quantile(bmi, 0.75, na.rm = TRUE), 2),
    
    female_n = sum(sex_clean == "Female", na.rm = TRUE),
    female_pct = round(mean(sex_clean == "Female", na.rm = TRUE) * 100, 2),
    
    ever_smoker_n = sum(smoking_model == "Ever smoker", na.rm = TRUE),
    ever_smoker_pct = round(mean(smoking_model == "Ever smoker", na.rm = TRUE) * 100, 2),
    
    hypertension_n = sum(hypertension_main == "Hypertension", na.rm = TRUE),
    hypertension_pct = round(mean(hypertension_main == "Hypertension", na.rm = TRUE) * 100, 2),
    
    .groups = "drop"
  )

table1_by_bmi

write_csv(table1_by_bmi, "tables/table1_by_bmi.csv")


# ---------------------------------------------------------
# 7. 生成吸烟状态按BMI分组分布表
# ---------------------------------------------------------

smoking_by_bmi <- data_final %>%
  count(bmi_group_clean, smoking_model, name = "n") %>%
  group_by(bmi_group_clean) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

smoking_by_bmi

write_csv(smoking_by_bmi, "tables/smoking_by_bmi.csv")


# ---------------------------------------------------------
# 8. 生成不同BMI组高血压患病率表
# ---------------------------------------------------------

hypertension_rate_by_bmi_clean <- data_final %>%
  group_by(bmi_group_clean) %>%
  summarise(
    n = n(),
    hypertension_n = sum(hypertension_main == "Hypertension", na.rm = TRUE),
    hypertension_rate_pct = round(
      mean(hypertension_main == "Hypertension", na.rm = TRUE) * 100,
      2
    ),
    .groups = "drop"
  )

hypertension_rate_by_bmi_clean

write_csv(
  hypertension_rate_by_bmi_clean,
  "tables/hypertension_rate_by_bmi_clean.csv"
)


# ---------------------------------------------------------
# 9. 绘制清洗后年龄分布图
# ---------------------------------------------------------

p_age <- ggplot(data_final, aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age distribution after data cleaning",
    x = "Age",
    y = "Count"
  )

p_age

ggsave(
  "figures/age_distribution_clean.png",
  p_age,
  width = 6,
  height = 4
)


# ---------------------------------------------------------
# 10. 绘制清洗后BMI分布图
# ---------------------------------------------------------

p_bmi <- ggplot(data_final, aes(x = bmi)) +
  geom_histogram(bins = 30) +
  labs(
    title = "BMI distribution after data cleaning",
    x = "BMI",
    y = "Count"
  )

p_bmi

ggsave(
  "figures/bmi_distribution_clean.png",
  p_bmi,
  width = 6,
  height = 4
)


# ---------------------------------------------------------
# 11. 绘制不同BMI组高血压患病率图
# ---------------------------------------------------------

p_htn <- ggplot(
  hypertension_rate_by_bmi_clean,
  aes(x = bmi_group_clean, y = hypertension_rate_pct)
) +
  geom_col() +
  labs(
    title = "Hypertension prevalence by BMI group",
    x = "BMI group",
    y = "Hypertension prevalence (%)"
  )

p_htn

ggsave(
  "figures/hypertension_rate_by_bmi_clean.png",
  p_htn,
  width = 6,
  height = 4
)


# ---------------------------------------------------------
# 12. 最后检查6月12日课前任务文件是否生成
# ---------------------------------------------------------

file.exists("tables/table1_overall.csv")
file.exists("tables/table1_by_bmi.csv")
file.exists("tables/smoking_by_bmi.csv")
file.exists("tables/hypertension_rate_by_bmi_clean.csv")
file.exists("figures/age_distribution_clean.png")
file.exists("figures/bmi_distribution_clean.png")
file.exists("figures/hypertension_rate_by_bmi_clean.png")