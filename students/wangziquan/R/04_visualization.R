# =========================================================
# 04_visualization.R
# 项目：基于 NHANES 数据的成年人 BMI 与高血压患病风险关联分析
# 作者：王子铨
# 目的：

# 1. 围绕 BMI 与高血压研究问题优化可视化图形
# 2. 生成变量分布图、组间比较图和年龄分层图
# =========================================================


# ---------------------------------------------------------
# 0. 加载R包
# ---------------------------------------------------------

library(tidyverse)
library(janitor)


# ---------------------------------------------------------
# 1. 创建输出文件夹
# ---------------------------------------------------------

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)


# ---------------------------------------------------------
# 2. 读取清洗后的分析数据
# ---------------------------------------------------------

data_final <- read_csv("output/nhanes_bmi_hypertension_clean.csv")

dim(data_final)
glimpse(data_final)


# ---------------------------------------------------------
# 3. 统一变量格式
# ---------------------------------------------------------

data_plot <- data_final %>%
  mutate(
    bmi_group_clean = factor(
      bmi_group_clean,
      levels = c("Normal", "Underweight", "Overweight", "Obesity")
    ),
    
    hypertension_binary = case_when(
      hypertension_main == 1 ~ 1,
      hypertension_main == 0 ~ 0,
      as.character(hypertension_main) == "1" ~ 1,
      as.character(hypertension_main) == "0" ~ 0,
      as.character(hypertension_main) == "Hypertension" ~ 1,
      as.character(hypertension_main) == "No hypertension" ~ 0,
      TRUE ~ NA_real_
    ),
    
    age_group = case_when(
      age < 40 ~ "<40",
      age >= 40 & age < 60 ~ "40-59",
      age >= 60 ~ ">=60",
      TRUE ~ NA_character_
    ),
    
    age_group = factor(
      age_group,
      levels = c("<40", "40-59", ">=60")
    )
  )

# 检查关键变量
table(data_plot$bmi_group_clean, useNA = "ifany")
table(data_plot$hypertension_binary, useNA = "ifany")
table(data_plot$age_group, useNA = "ifany")


# ---------------------------------------------------------
# 4. 图1：BMI分布图，标出BMI分组切点
# ---------------------------------------------------------

p_bmi_cutoff <- ggplot(data_plot, aes(x = bmi)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 18.5, linetype = "dashed") +
  geom_vline(xintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = 30, linetype = "dashed") +
  labs(
    title = "BMI distribution with classification cutoffs",
    subtitle = "Dashed lines indicate BMI cutoffs: 18.5, 25, and 30",
    x = "BMI",
    y = "Number of participants",
    caption = "Data source: NHANES 2017-2018; analysis sample after cleaning."
  ) +
  theme_minimal()

p_bmi_cutoff

ggsave(
  "figures/04_bmi_distribution_cutoffs.png",
  p_bmi_cutoff,
  width = 7,
  height = 5
)


# ---------------------------------------------------------
# 5. 准备图2数据：不同BMI组高血压患病率和95%CI
# ---------------------------------------------------------

plot_htn_rate_by_bmi_ci <- data_plot %>%
  filter(!is.na(bmi_group_clean), !is.na(hypertension_binary)) %>%
  group_by(bmi_group_clean) %>%
  summarise(
    n = n(),
    hypertension_n = sum(hypertension_binary == 1, na.rm = TRUE),
    hypertension_rate = hypertension_n / n,
    
    se = sqrt(hypertension_rate * (1 - hypertension_rate) / n),
    ci_low = pmax(hypertension_rate - 1.96 * se, 0),
    ci_high = pmin(hypertension_rate + 1.96 * se, 1),
    
    hypertension_rate_pct = round(hypertension_rate * 100, 1),
    ci_low_pct = round(ci_low * 100, 1),
    ci_high_pct = round(ci_high * 100, 1),
    
    .groups = "drop"
  )

plot_htn_rate_by_bmi_ci

write_csv(
  plot_htn_rate_by_bmi_ci,
  "tables/plot_htn_rate_by_bmi_ci.csv"
)


# ---------------------------------------------------------
# 6. 图2：不同BMI组高血压患病率图，带95%CI和标签
# ---------------------------------------------------------

p_htn_bmi_ci <- ggplot(
  plot_htn_rate_by_bmi_ci,
  aes(x = bmi_group_clean, y = hypertension_rate_pct)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = ci_low_pct, ymax = ci_high_pct),
    width = 0.2
  ) +
  geom_text(
    aes(label = paste0(hypertension_rate_pct, "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Hypertension prevalence by BMI group",
    subtitle = "Bars show prevalence; error bars show approximate 95% confidence intervals",
    x = "BMI group",
    y = "Hypertension prevalence (%)",
    caption = "Hypertension was defined using the main analysis definition."
  ) +
  ylim(0, max(plot_htn_rate_by_bmi_ci$ci_high_pct, na.rm = TRUE) + 10) +
  theme_minimal()

p_htn_bmi_ci

ggsave(
  "figures/04_hypertension_prevalence_by_bmi_ci.png",
  p_htn_bmi_ci,
  width = 7,
  height = 5
)


# ---------------------------------------------------------
# 7. 准备图3数据：年龄分层后的BMI组高血压患病率
# ---------------------------------------------------------

plot_htn_rate_by_bmi_age <- data_plot %>%
  filter(
    !is.na(age_group),
    !is.na(bmi_group_clean),
    !is.na(hypertension_binary)
  ) %>%
  group_by(age_group, bmi_group_clean) %>%
  summarise(
    n = n(),
    hypertension_n = sum(hypertension_binary == 1, na.rm = TRUE),
    hypertension_rate_pct = round(
      mean(hypertension_binary == 1, na.rm = TRUE) * 100,
      1
    ),
    .groups = "drop"
  )

plot_htn_rate_by_bmi_age

write_csv(
  plot_htn_rate_by_bmi_age,
  "tables/plot_htn_rate_by_bmi_age.csv"
)


# ---------------------------------------------------------
# 8. 图3：按年龄分层的BMI组高血压患病率图
# ---------------------------------------------------------

p_htn_bmi_age <- ggplot(
  plot_htn_rate_by_bmi_age,
  aes(x = bmi_group_clean, y = hypertension_rate_pct)
) +
  geom_col() +
  facet_wrap(~ age_group) +
  labs(
    title = "Hypertension prevalence by BMI group and age group",
    subtitle = "Faceted by age group",
    x = "BMI group",
    y = "Hypertension prevalence (%)",
    caption = "This figure explores whether the BMI-hypertension pattern differs by age group."
  ) +
  theme_minimal()

p_htn_bmi_age

ggsave(
  "figures/04_hypertension_prevalence_by_bmi_age.png",
  p_htn_bmi_age,
  width = 8,
  height = 5
)


# ---------------------------------------------------------
# 9. 生成图形说明表
# ---------------------------------------------------------

figure_summary <- tibble(
  figure_file = c(
    "04_bmi_distribution_cutoffs.png",
    "04_hypertension_prevalence_by_bmi_ci.png",
    "04_hypertension_prevalence_by_bmi_age.png"
  ),
  purpose = c(
    "Show BMI distribution and BMI classification cutoffs",
    "Compare hypertension prevalence across BMI groups",
    "Explore BMI-hypertension pattern across age groups"
  ),
  relation_to_research_question = c(
    "Describes the exposure variable BMI",
    "Directly addresses whether hypertension prevalence differs by BMI group",
    "Assesses whether the observed pattern may vary by age"
  )
)

figure_summary

write_csv(
  figure_summary,
  "tables/figure_summary_0616.csv"
)


# ---------------------------------------------------------
# 10. 最后检查6月16日课前任务文件是否生成
# ---------------------------------------------------------

file.exists("figures/04_bmi_distribution_cutoffs.png")
file.exists("figures/04_hypertension_prevalence_by_bmi_ci.png")
file.exists("figures/04_hypertension_prevalence_by_bmi_age.png")
file.exists("tables/plot_htn_rate_by_bmi_ci.csv")
file.exists("tables/plot_htn_rate_by_bmi_age.csv")
file.exists("tables/figure_summary_0616.csv")