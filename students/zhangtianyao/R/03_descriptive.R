#==========描述性分析============
library(dplyr)
library(tableone)

# -------第一步：数据预处理与变量重编码-------
cleaned_data <- current_data %>%
  mutate(
    # 1. ELD评分为四分位数
    ELD_quartile = cut(ELD_total_score,
                       breaks = quantile(ELD_total_score, 
                                         probs = c(0, 0.25, 0.5, 0.75, 1), 
                                         na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = c("Q1 (Lowest)", 
                                  "Q2", 
                                  "Q3", 
                                  "Q4 (Highest)")),
    
    # 2. 性别
    Sex = factor(Sex, levels = c("Male", "Female")),
    
    # 3. 种族重编码
    Race = case_when(
      Race == 1 ~ "Mexican American",
      Race == 2 ~ "Other Hispanic",
      Race == 3 ~ "Non-Hispanic White",
      Race == 4 ~ "Non-Hispanic Black",
      Race == 6 ~ "Non-Hispanic Asian",
      Race == 7 ~ "Other Race",
      TRUE ~ NA_character_
    ),
    Race = factor(Race),
    
    # 4. 教育水平重编码
    Education = case_when(
      Education %in% c(1, 2) ~ "Less than high school",
      Education == 3 ~ "High school",
      Education %in% c(4, 5) ~ "Above high school",
      TRUE ~ NA_character_
    ),
    Education = factor(Education, levels = c("Less than high school", 
                                             "High school", 
                                             "Above high school")),
    
    # 5. 吸烟状态
    Smoking = case_when(
      Smoking == 1 ~ "Current smoker",
      Smoking == 0 ~ "Non-smoker",
      TRUE ~ NA_character_
    ),
    Smoking = factor(Smoking, levels = c("Current smoker", "Non-smoker")),
    
    # 6. 饮酒状态
    Alcohol = case_when(
      Alcohol == 1 ~ "Drinker",
      Alcohol == 0 ~ "Non-drinker",
      TRUE ~ NA_character_
    ),
    Alcohol = factor(Alcohol, levels = c("Drinker", "Non-drinker")),
    
    # 7. 体力活动
    PhysicalActivity = case_when(
      PhysicalActivity == 1 ~ "Active",
      PhysicalActivity == 0 ~ "Inactive",
      TRUE ~ NA_character_
    ),
    PhysicalActivity = factor(PhysicalActivity, levels = c("Active", "Inactive")),
    
    # 8. 高血压
    Hypertension = case_when(
      Hypertension == 1 ~ "Yes",
      Hypertension == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    Hypertension = factor(Hypertension, levels = c("Yes", "No")),
    
    # 9. 糖尿病
    Diabetes = case_when(
      Diabetes == 1 ~ "Yes",
      Diabetes == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    Diabetes = factor(Diabetes, levels = c("Yes", "No")),
    
    # 10. 血脂异常
    Dyslipidemia = case_when(
      Dyslipidemia == 1 ~ "Yes",
      Dyslipidemia == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    Dyslipidemia = factor(Dyslipidemia, levels = c("Yes", "No")),
    
    # 11. 心力衰竭
    HF = case_when(
      HF == 1 ~ "Yes",
      HF == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    HF = factor(HF, levels = c("Yes", "No"))
  )

# ------第二步：筛选分析数据集-----------
analysis_data <- cleaned_data %>%
  filter(
    !is.na(ELD_quartile),
    !is.na(Sex),
    !is.na(Race),
    !is.na(ELD_total_score)
  )
cat(paste0("分析样本量（不加权）：", nrow(analysis_data), "\n"))

# 检查各四分位组的样本量
cat("\n各四分位组样本量：\n")
print(table(analysis_data$ELD_quartile, useNA = "ifany"))

# --------第三步：使用 tableone 生成不加权基线表---------------

# 定义要展示的变量
myVars <- c("ELD_total_score", "Age", "Sex", "Race", "Education", "INDFMPIR", "BMI",
            "Smoking", "Alcohol", "PhysicalActivity",
            "Hypertension", "Diabetes", "Dyslipidemia","Energy")

# 定义分类变量
catVars <- c("Sex", "Race", "Education", "Smoking", "Alcohol", 
             "PhysicalActivity", "Hypertension", "Diabetes", "Dyslipidemia")

# 生成不加权基线表
table1_unweighted <- CreateTableOne(
  vars = myVars,
  strata = "ELD_quartile",
  data = analysis_data,
  factorVars = catVars,
  includeNA = FALSE
)

# -------第四步：打印表格-----------
# 基本打印
print(table1_unweighted, 
      showAllLevels = TRUE,
      formatOptions = list(big.mark = ","))

# 更详细的打印（带检验p值）
print(table1_unweighted, 
      showAllLevels = TRUE,
      quote = FALSE,
      noSpaces = TRUE,
      printToggle = TRUE)

# 将表格转换为数据框
table1_df <- print(table1_unweighted, 
                   printToggle = FALSE,
                   quote = FALSE,
                   noSpaces = TRUE,
                   explain = FALSE)

#导出
write.csv(table1_df, "Table1_ELD_Quartiles_Complete.csv", row.names = TRUE)

# --------第六步：计算各组样本量和ELD评分范围----------

summary_table_descpriptive <- analysis_data %>%
  group_by(ELD_quartile) %>%
  summarise(
    N = n(),
    ELD_Score_Mean = mean(ELD_total_score, na.rm = TRUE),
    ELD_Score_SD = sd(ELD_total_score, na.rm = TRUE),
    ELD_Score_Min = min(ELD_total_score, na.rm = TRUE),
    ELD_Score_Max = max(ELD_total_score, na.rm = TRUE),
    ELD_Score_Median = median(ELD_total_score, na.rm = TRUE),
    Age_Mean = mean(Age, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    BMI_Mean = mean(BMI, na.rm = TRUE),
    BMI_SD = sd(BMI, na.rm = TRUE),
    PIR_Median = median(INDFMPIR, na.rm = TRUE),
    PIR_IQR = IQR(INDFMPIR, na.rm = TRUE)
  )

cat("\n各组汇总统计：\n")
print(summary_table_descpriptive)
#查看ELD评分分布
summary(analysis_data$ELD_total_score)

# ---------第七步：单独提取各统计量-----------
# 连续变量格式：Mean (SD)
cont_vars <- c("Age", "BMI", "INDFMPIR","Energy","ELD_total_score")

cat("\n\n连续变量详情（均值±标准差）：\n")
for(var in cont_vars) {
  cat("\n", var, ":\n")
  overall_mean <- mean(analysis_data[[var]], na.rm = TRUE)
  overall_sd <- sd(analysis_data[[var]], na.rm = TRUE)
  cat("  总体: ", round(overall_mean, 1), " (", round(overall_sd, 1), ")\n", sep = "")
  
  for(q in levels(analysis_data$ELD_quartile)) {
    subset_data <- analysis_data[analysis_data$ELD_quartile == q, ]
    q_mean <- mean(subset_data[[var]], na.rm = TRUE)
    q_sd <- sd(subset_data[[var]], na.rm = TRUE)
    cat("  ", q, ": ", round(q_mean, 1), " (", round(q_sd, 1), ")\n", sep = "")
  }
}

# 分类变量格式：n (%)
cat("\n\n分类变量详情（频数，百分比）：\n")
for(var in catVars) {
  cat("\n", var, ":\n")
  # 总体
  overall_tab <- table(analysis_data[[var]], useNA = "ifany")
  overall_pct <- prop.table(overall_tab) * 100
  for(lvl in names(overall_tab)) {
    cat("  总体 - ", lvl, ": ", overall_tab[lvl], " (", round(overall_pct[lvl], 1), "%)\n", sep = "")
  }
  
  # 各四分位组
  for(q in levels(analysis_data$ELD_quartile)) {
    subset_data <- analysis_data[analysis_data$ELD_quartile == q, ]
    q_tab <- table(subset_data[[var]], useNA = "ifany")
    q_pct <- prop.table(q_tab) * 100
    for(lvl in names(q_tab)) {
      cat("  ", q, " - ", lvl, ": ", q_tab[lvl], " (", round(q_pct[lvl], 1), "%)\n", sep = "")
    }
  }
}
# 食物组成
# 定义食物组变量
food_items <- c("Vegetables", "Fruits", "WholeGrains", "Legumes", "Nuts", 
                "Dairy", "AddedSugar", "UnsaturatedOil", "Fish", 
                "Beef_Lamb", "Pork", "Poultry", "Egg", "Potatoes")

# 创建结果表格
result_table <- data.frame(
  Food_Component = food_items,
  Total = sapply(food_items, function(x) sprintf("%.1f (%.1f)", 
                                                 mean(analysis_data[[x]], na.rm = TRUE), 
                                                 sd(analysis_data[[x]], na.rm = TRUE))),
  Q1 = sapply(food_items, function(x) sprintf("%.1f (%.1f)", 
                                              mean(analysis_data[[x]][analysis_data$ELD_quartile == "Q1 (Lowest)"], na.rm = TRUE), 
                                              sd(analysis_data[[x]][analysis_data$ELD_quartile == "Q1 (Lowest)"], na.rm = TRUE))),
  Q2 = sapply(food_items, function(x) sprintf("%.1f (%.1f)", 
                                              mean(analysis_data[[x]][analysis_data$ELD_quartile == "Q2"], na.rm = TRUE), 
                                              sd(analysis_data[[x]][analysis_data$ELD_quartile == "Q2"], na.rm = TRUE))),
  Q3 = sapply(food_items, function(x) sprintf("%.1f (%.1f)", 
                                              mean(analysis_data[[x]][analysis_data$ELD_quartile == "Q3"], na.rm = TRUE), 
                                              sd(analysis_data[[x]][analysis_data$ELD_quartile == "Q3"], na.rm = TRUE))),
  Q4 = sapply(food_items, function(x) sprintf("%.1f (%.1f)", 
                                              mean(analysis_data[[x]][analysis_data$ELD_quartile == "Q4 (Highest)"], na.rm = TRUE), 
                                              sd(analysis_data[[x]][analysis_data$ELD_quartile == "Q4 (Highest)"], na.rm = TRUE)))
)

# 显示表格
print(result_table)

# 同时输出样本量
cat("\n样本量：\n")
table(analysis_data$ELD_quartile)

