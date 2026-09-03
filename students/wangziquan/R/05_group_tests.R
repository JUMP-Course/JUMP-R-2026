# =========================================================
# 05_group_tests.R
# 项目：基于 NHANES 数据的成年人 BMI 与高血压患病风险关联分析
# 作者：王子铨
# 目的：
# 1. 完成顺延至6月23日的基础统计分析任务
# 2. 对 BMI 分组与高血压状态进行组间比较
# 3. 对 Table 1 中关键变量进行统计检验
# 4. 使用自定义函数和 UTF-8 输出
# =========================================================

library(tidyverse)
library(janitor)

options(encoding = "UTF-8")

source("R_utils_functions.R", encoding = "UTF-8")

dir.create("tables", showWarnings = FALSE)
# 检查卡方/Fisher检验函数是否成功加载
exists("run_chisq_or_fisher")

# 检查Kruskal-Wallis检验函数是否成功加载
exists("run_kruskal_test")

# ---------------------------------------------------------
# 1. 读取清洗后数据
# ---------------------------------------------------------

data_final <- read_csv("output/nhanes_bmi_hypertension_clean.csv")

dim(data_final)
glimpse(data_final)


# ---------------------------------------------------------
# 2. 整理统计检验所需变量
# ---------------------------------------------------------

data_test <- data_final %>%
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
    
    hypertension_label = case_when(
      hypertension_binary == 1 ~ "Hypertension",
      hypertension_binary == 0 ~ "No hypertension",
      TRUE ~ NA_character_
    ),
    
    hypertension_label = factor(
      hypertension_label,
      levels = c("No hypertension", "Hypertension")
    ),
    
    sex_clean = factor(sex_clean),
    smoking_model = factor(smoking_model)
  )

table(data_test$bmi_group_clean, useNA = "ifany")
table(data_test$hypertension_label, useNA = "ifany")


# ---------------------------------------------------------
# 3. 核心检验：BMI组与高血压状态
# ---------------------------------------------------------

bmi_hypertension_table <- table(
  data_test$bmi_group_clean,
  data_test$hypertension_label,
  useNA = "no"
)

bmi_hypertension_table

bmi_hypertension_test <- run_chisq_or_fisher(
  data = data_test,
  row_var = "bmi_group_clean",
  col_var = "hypertension_label"
)

bmi_hypertension_test

save_csv_utf8(
  as_tibble(as.data.frame.matrix(bmi_hypertension_table), rownames = "bmi_group"),
  "tables/bmi_hypertension_contingency_table.csv"
)

save_csv_utf8(
  bmi_hypertension_test,
  "tables/test_bmi_hypertension.csv"
)


# ---------------------------------------------------------
# 4. Table 1 中分类变量的组间比较
# ---------------------------------------------------------

test_sex_by_bmi <- run_chisq_or_fisher(
  data = data_test,
  row_var = "bmi_group_clean",
  col_var = "sex_clean"
)

test_smoking_by_bmi <- run_chisq_or_fisher(
  data = data_test,
  row_var = "bmi_group_clean",
  col_var = "smoking_model"
)

test_hypertension_by_bmi <- run_chisq_or_fisher(
  data = data_test,
  row_var = "bmi_group_clean",
  col_var = "hypertension_label"
)


# ---------------------------------------------------------
# 5. Table 1 中连续变量的组间比较
# ---------------------------------------------------------

test_age_by_bmi <- run_kruskal_test(
  data = data_test,
  value_var = "age",
  group_var = "bmi_group_clean"
)

test_bmi_by_bmi <- run_kruskal_test(
  data = data_test,
  value_var = "bmi",
  group_var = "bmi_group_clean"
)


# ---------------------------------------------------------
# 6. 汇总所有检验结果
# ---------------------------------------------------------

test_table1_variables <- bind_rows(
  test_age_by_bmi %>%
    transmute(
      variable = value_variable,
      group = group_variable,
      test = test,
      p_value = p_value
    ),
  
  test_bmi_by_bmi %>%
    transmute(
      variable = value_variable,
      group = group_variable,
      test = test,
      p_value = p_value
    ),
  
  test_sex_by_bmi %>%
    transmute(
      variable = col_variable,
      group = row_variable,
      test = test,
      p_value = p_value
    ),
  
  test_smoking_by_bmi %>%
    transmute(
      variable = col_variable,
      group = row_variable,
      test = test,
      p_value = p_value
    ),
  
  test_hypertension_by_bmi %>%
    transmute(
      variable = col_variable,
      group = row_variable,
      test = test,
      p_value = p_value
    )
) %>%
  mutate(
    p_value_rounded = round(p_value, 4),
    interpretation = case_when(
      p_value < 0.05 ~ "There is statistical evidence of group difference.",
      p_value >= 0.05 ~ "No statistical evidence of group difference.",
      TRUE ~ "P value unavailable."
    )
  )

test_table1_variables

save_csv_utf8(
  test_table1_variables,
  "tables/test_table1_variables.csv"
)


# ---------------------------------------------------------
# 7. 生成简明结果解释表
# ---------------------------------------------------------

group_comparison_summary <- tibble(
  question = c(
    "Is hypertension prevalence different across BMI groups?",
    "Is age distribution different across BMI groups?",
    "Is sex distribution different across BMI groups?",
    "Is smoking status different across BMI groups?"
  ),
  method = c(
    test_hypertension_by_bmi$test,
    test_age_by_bmi$test,
    test_sex_by_bmi$test,
    test_smoking_by_bmi$test
  ),
  p_value = c(
    test_hypertension_by_bmi$p_value,
    test_age_by_bmi$p_value,
    test_sex_by_bmi$p_value,
    test_smoking_by_bmi$p_value
  ),
  interpretation = case_when(
    p_value < 0.05 ~ "Group difference was statistically significant at alpha = 0.05.",
    p_value >= 0.05 ~ "Group difference was not statistically significant at alpha = 0.05.",
    TRUE ~ "P value unavailable."
  )
)

group_comparison_summary

save_csv_utf8(
  group_comparison_summary,
  "tables/group_comparison_summary.csv"
)


# ---------------------------------------------------------
# 8. 生成文字版解释文件
# ---------------------------------------------------------

interpretation_text <- c(
  "# 6月23日前任务：基础统计分析与组间比较",
  "",
  "## 研究问题",
  "",
  "本项目关注成年人 BMI 分组与高血压患病状态之间是否存在统计学关联。",
  "",
  "## 方法选择",
  "",
  "1. BMI 分组与高血压状态均为分类变量，因此采用卡方检验；如果列联表理论频数较小，则自动改用 Fisher 精确检验。",
  "",
  "2. 年龄为连续变量，不同 BMI 组之间的年龄分布比较采用 Kruskal-Wallis 检验。该方法不要求年龄严格服从正态分布，适合作为当前阶段的基础组间比较方法。",
  "",
  "3. 性别和吸烟状态均为分类变量，因此采用卡方检验或 Fisher 精确检验比较不同 BMI 组之间的分布差异。",
  "",
  "## 结果解释原则",
  "",
  "P 值用于判断观察到的组间差异是否可能由随机误差解释。P < 0.05 表示在当前样本中有统计学证据支持组间差异，但不能直接解释为因果关系。",
  "",
  "本阶段结果属于基础统计分析，后续仍需通过 Logistic 回归进一步调整年龄、性别和吸烟等混杂因素。"
)

save_text_utf8(
  interpretation_text,
  "tables/test_interpretation_0623.md"
)


# ---------------------------------------------------------
# 9. 最后检查文件
# ---------------------------------------------------------

file.exists("tables/bmi_hypertension_contingency_table.csv")
file.exists("tables/test_bmi_hypertension.csv")
file.exists("tables/test_table1_variables.csv")
file.exists("tables/group_comparison_summary.csv")
file.exists("tables/test_interpretation_0623.md")
# =========================================================
# 课后补充：整理并输出组间比较结果
# =========================================================

# 定义一个自定义函数：统一显示P值
# p是输入的P值；函数返回便于汇报和表格展示的字符结果
format_p_value <- function(p) {
  
  # 当P值小于0.001时，显示为<0.001，避免显示过多小数
  ifelse(
    p < 0.001,
    "<0.001",
    sprintf("%.3f", p)
  )
}

# 读取前面已经生成的组间比较汇总表
group_comparison_summary <- read_csv(
  "tables/group_comparison_summary.csv",
  show_col_types = FALSE
)

# 对P值增加格式化显示，并生成“是否有统计学差异”的中文解释
group_comparison_summary_display <- group_comparison_summary %>%
  mutate(
    # 生成适合汇报展示的P值
    p_display = format_p_value(p_value),
    
    # 根据0.05阈值生成初步解释
    statistical_conclusion = case_when(
      p_value < 0.05 ~ "存在统计学差异",
      p_value >= 0.05 ~ "未发现统计学差异",
      TRUE ~ "无法判断"
    )
  )

# 在控制台查看整理后的结果
group_comparison_summary_display

# 保存为UTF-8编码CSV文件
write_csv(
  group_comparison_summary_display,
  "tables/group_comparison_summary_display.csv",
  na = ""
)

# 创建组间比较的文字说明
group_test_text <- c(
  "# 组间比较与假设检验",
  "",
  "## 核心问题",
  "",
  "本阶段比较不同BMI分组之间的高血压患病状态是否存在统计学差异。",
  "",
  "## 方法选择",
  "",
  "BMI分组和高血压状态均为分类变量，因此使用卡方检验。",
  "当列联表中存在理论频数较小的单元格时，改用Fisher精确检验。",
  "年龄为连续变量，使用Kruskal-Wallis检验比较不同BMI组的年龄分布。",
  "",
  "## 解释原则",
  "",
  "P值用于判断组间观察到的差异是否可能由随机误差造成。",
  "P < 0.05表示存在统计学差异，但不表示BMI导致高血压。",
  "后续需要使用Logistic回归控制年龄、性别和吸烟等混杂因素。"
)

# 建立UTF-8连接，确保中文Markdown文件不乱码
con <- file(
  "tables/test_interpretation_0619.md",
  open = "w",
  encoding = "UTF-8"
)

# 将文字写入文件
writeLines(group_test_text, con = con, useBytes = TRUE)

# 关闭文件连接，避免文件被占用
close(con)

# 最后检查新增文件是否生成成功
file.exists("tables/group_comparison_summary_display.csv")
file.exists("tables/test_interpretation_0619.md")