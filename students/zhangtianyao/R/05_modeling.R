#========加载安装包=======
library(dplyr)
library(survey)
library(ggplot2)
library(forestplot)

#==========加权logistic回归=========
# 第一步
# 确保变量格式正确，结局变量二分类
analysis_data <- analysis_data %>%
  mutate(
    # 因变量：心力衰竭（1=患病，0=未患病），将字符型转换为数值型
    HF_binary = case_when(
      HF == "Yes" ~ 1,
      HF == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ELD四分位数（作为因子）
    ELD_quartile = factor(ELD_quartile, 
                          levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    
    # 其他分类变量设置为因子，以第一水平为参照
    Sex = factor(Sex, levels = c("Male", "Female")),
    Race = factor(Race),
    Education = factor(Education, 
                       levels = c("Less than high school", "High school", "Above high school")),
    Smoking = factor(Smoking, levels = c("Non-smoker", "Current smoker")),
    Alcohol = factor(Alcohol, levels = c("Non-drinker", "Drinker")),
    PhysicalActivity = factor(PhysicalActivity, levels = c("Inactive", "Active")),
    Hypertension = factor(Hypertension, levels = c("No", "Yes")),
    Diabetes = factor(Diabetes, levels = c("No", "Yes")),
    Dyslipidemia = factor(Dyslipidemia, levels = c("No", "Yes"))
  )

# 第二步：创建调查设计对象
#加权回归
nhanes_design <- svydesign(
  id = ~SDMVPSU,#初级抽样单位
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = analysis_data
)

# 第三步：模型1 - 未调整协变量（Crude model）

cat("\n========== 模型1：未调整协变量 ==========\n")

# 3.1 ELD评分作为连续变量
model1_continuous <- svyglm(
  HF_binary ~ ELD_total_score,
  design = nhanes_design,
  family = quasibinomial()
)

# 计算OR和95% CI
OR1_cont <- exp(coef(model1_continuous))
CI1_cont <- exp(confint(model1_continuous))

cat("\n【ELD评分（连续变量）】\n")
print(data.frame(
  Variable = "ELD_total_score",
  OR = round(OR1_cont[2], 3),
  CI_2.5 = round(CI1_cont[2, 1], 3),
  CI_97.5 = round(CI1_cont[2, 2], 3),
  P_value = round(coef(summary(model1_continuous))[2, 4], 4)
))

# 3.2 ELD评分作为四分位数（以Q1为参考）
model1_quartile <- svyglm(
  HF_binary ~ ELD_quartile,
  design = nhanes_design,
  family = quasibinomial()
)

OR1_quart <- exp(coef(model1_quartile))
CI1_quart <- exp(confint(model1_quartile))

cat("\n【ELD评分（四分位数，以Q1为参考）】\n")
results1 <- data.frame(
  Variable = c("Q2", "Q3", "Q4 (Highest)"),
  OR = round(OR1_quart[2:4], 3),
  CI_2.5 = round(CI1_quart[2:4, 1], 3),
  CI_97.5 = round(CI1_quart[2:4, 2], 3),
  P_value = round(coef(summary(model1_quartile))[2:4, 4], 4)
)
print(results1)

# 第四步：模型2 - 调整人口学变量

cat("\n========== 模型2：调整年龄、性别、种族、教育水平==========\n")

# 4.1 ELD评分作为连续变量
model2_continuous <- svyglm(
  HF_binary ~ ELD_total_score + Age + Sex + Race + Education,
  design = nhanes_design,
  family = quasibinomial()
)

OR2_cont <- exp(coef(model2_continuous))
CI2_cont <- exp(confint(model2_continuous))

cat("\n【ELD评分（连续变量）】\n")
print(data.frame(
  Variable = "ELD_total_score",
  OR = round(OR2_cont[2], 3),
  CI_2.5 = round(CI2_cont[2, 1], 3),
  CI_97.5 = round(CI2_cont[2, 2], 3),
  P_value = round(coef(summary(model2_continuous))[2, 4], 4)
))

# 4.2 ELD评分作为四分位数
model2_quartile <- svyglm(
  HF_binary ~ ELD_quartile + Age + Sex + Race + Education,
  design = nhanes_design,
  family = quasibinomial()
)

OR2_quart <- exp(coef(model2_quartile))
CI2_quart <- exp(confint(model2_quartile))

cat("\n【ELD评分（四分位数，以Q1为参考）】\n")
results2 <- data.frame(
  Variable = c("Q2", "Q3", "Q4 (Highest)"),
  OR = round(OR2_quart[2:4], 3),
  CI_2.5 = round(CI2_quart[2:4, 1], 3),
  CI_97.5 = round(CI2_quart[2:4, 2], 3),
  P_value = round(coef(summary(model2_quartile))[2:4, 4], 4)
)
print(results2)

# 查看所有协变量的结果
cat("\n【完整模型2结果】\n")
print(round(exp(coef(model2_quartile)), 3))#提取协变量的系数
print(round(exp(confint(model2_quartile)), 3))#提取协变量95%置信区间

# 第五步：模型3 - 完全调整模型

cat("\n========== 模型3：完全调整模型 ==========\n")
cat("调整变量：年龄、性别、种族、教育水平、吸烟、饮酒、体力活动、BMI、高血压、糖尿病、血脂异常、总能量摄入\n")

# 5.1 ELD评分作为连续变量
model3_continuous <- svyglm(
  HF_binary ~ ELD_total_score + Age + Sex + Race + Education + 
    Smoking + Alcohol + PhysicalActivity + BMI + 
    Hypertension + Diabetes + Dyslipidemia + Energy,
  design = nhanes_design,
  family = quasibinomial()
)

OR3_cont <- exp(coef(model3_continuous))
CI3_cont <- exp(confint(model3_continuous))

cat("\n【ELD评分（连续变量）】\n")
print(data.frame(
  Variable = "ELD_total_score",
  OR = round(OR3_cont[2], 3),
  CI_2.5 = round(CI3_cont[2, 1], 3),
  CI_97.5 = round(CI3_cont[2, 2], 3),
  P_value = round(coef(summary(model3_continuous))[2, 4], 4)
))

# 5.2 ELD评分作为四分位数
model3_quartile <- svyglm(
  HF_binary ~ ELD_quartile + Age + Sex + Race + Education + 
    Smoking + Alcohol + PhysicalActivity + BMI + 
    Hypertension + Diabetes + Dyslipidemia + Energy,
  design = nhanes_design,
  family = quasibinomial()
)

OR3_quart <- exp(coef(model3_quartile))
CI3_quart <- exp(confint(model3_quartile))

cat("\n【ELD评分（四分位数，以Q1为参考）】\n")
results3 <- data.frame(
  Variable = c("Q2", "Q3", "Q4 (Highest)"),
  OR = round(OR3_quart[2:4], 3),
  CI_2.5 = round(CI3_quart[2:4, 1], 3),
  CI_97.5 = round(CI3_quart[2:4, 2], 3),
  P_value = round(coef(summary(model3_quartile))[2:4, 4], 4)
)
print(results3)

# 第六步：趋势检验（P for trend）

cat("\n========== 趋势检验（P for trend）==========\n")

# 将四分位数作为连续变量纳入模型
# 创建趋势检验变量（将四分位数转换为数值：1,2,3,4）
analysis_data$ELD_trend <- as.numeric(analysis_data$ELD_quartile)

# 重新创建设计对象（包含趋势变量）
design_trend <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = analysis_data
)

# 模型1的趋势检验
model1_trend <- svyglm(
  HF_binary ~ ELD_trend,
  design = design_trend,
  family = quasibinomial()
)

# 模型2的趋势检验
model2_trend <- svyglm(
  HF_binary ~ ELD_trend + Age + Sex + Race + Education,
  design = design_trend,
  family = quasibinomial()
)

# 模型3的趋势检验
model3_trend <- svyglm(
  HF_binary ~ ELD_trend + Age + Sex + Race + Education + 
    Smoking + Alcohol + PhysicalActivity + BMI + 
    Hypertension + Diabetes + Dyslipidemia + Energy,
  design = design_trend,
  family = quasibinomial()
)

# 提取趋势检验P值
trend_results <- data.frame(
  Model = c("Model 1 (Crude)", "Model 2 (Demographic adjusted)", "Model 3 (Fully adjusted)"),
  P_for_trend = c(
    round(coef(summary(model1_trend))[2, 4], 4),
    round(coef(summary(model2_trend))[2, 4], 4),
    round(coef(summary(model3_trend))[2, 4], 4)
  ),
  OR_per_quartile = c(
    round(exp(coef(model1_trend)[2]), 3),
    round(exp(coef(model2_trend)[2]), 3),
    round(exp(coef(model3_trend)[2]), 3)
  )
)

print(trend_results)

# 第七步：汇总所有结果

cat("\n========== 结果汇总表 ==========\n")

# 创建汇总表格
summary_table <- data.frame(
  Model = rep(c("Model 1 (Crude)", "Model 2 (Demographic adjusted)", "Model 3 (Fully adjusted)"), each = 4),
  Variable = c("ELD_total_score (per 1-point increase)", "Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1",
               "ELD_total_score (per 1-point increase)", "Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1",
               "ELD_total_score (per 1-point increase)", "Q2 vs Q1", "Q3 vs Q1", "Q4 vs Q1"),
  OR = c(
    OR1_cont[2], OR1_quart[2], OR1_quart[3], OR1_quart[4],
    OR2_cont[2], OR2_quart[2], OR2_quart[3], OR2_quart[4],
    OR3_cont[2], OR3_quart[2], OR3_quart[3], OR3_quart[4]
  ),
  CI_lower = c(
    CI1_cont[2, 1], CI1_quart[2, 1], CI1_quart[3, 1], CI1_quart[4, 1],
    CI2_cont[2, 1], CI2_quart[2, 1], CI2_quart[3, 1], CI2_quart[4, 1],
    CI3_cont[2, 1], CI3_quart[2, 1], CI3_quart[3, 1], CI3_quart[4, 1]
  ),
  CI_upper = c(
    CI1_cont[2, 2], CI1_quart[2, 2], CI1_quart[3, 2], CI1_quart[4, 2],
    CI2_cont[2, 2], CI2_quart[2, 2], CI2_quart[3, 2], CI2_quart[4, 2],
    CI3_cont[2, 2], CI3_quart[2, 2], CI3_quart[3, 2], CI3_quart[4, 2]
  ),
  P_value = c(
    coef(summary(model1_continuous))[2, 4], coef(summary(model1_quartile))[2, 4], 
    coef(summary(model1_quartile))[3, 4], coef(summary(model1_quartile))[4, 4],
    coef(summary(model2_continuous))[2, 4], coef(summary(model2_quartile))[2, 4],
    coef(summary(model2_quartile))[3, 4], coef(summary(model2_quartile))[4, 4],
    coef(summary(model3_continuous))[2, 4], coef(summary(model3_quartile))[2, 4],
    coef(summary(model3_quartile))[3, 4], coef(summary(model3_quartile))[4, 4]
  )
)

# 格式化输出
summary_table$OR_95CI <- sprintf("%.2f (%.2f-%.2f)", 
                                 summary_table$OR, 
                                 summary_table$CI_lower, 
                                 summary_table$CI_upper)

print(summary_table %>% select(Model, Variable, OR_95CI, P_value))


# 第八步：创建完整结果表格

final_results <- data.frame(
  ` ` = c("ELD score (per 1-point increase)", 
          "ELD score quartiles",
          "  Q2 vs Q1",
          "  Q3 vs Q1", 
          "  Q4 vs Q1",
          "P for trend"),
  
  `Model 1 (Crude)` = c(
    sprintf("%.2f (%.2f-%.2f)", OR1_cont[2], CI1_cont[2,1], CI1_cont[2,2]),
    "",
    sprintf("%.2f (%.2f-%.2f)", OR1_quart[2], CI1_quart[2,1], CI1_quart[2,2]),
    sprintf("%.2f (%.2f-%.2f)", OR1_quart[3], CI1_quart[3,1], CI1_quart[3,2]),
    sprintf("%.2f (%.2f-%.2f)", OR1_quart[4], CI1_quart[4,1], CI1_quart[4,2]),
    sprintf("%.3f", coef(summary(model1_trend))[2, 4])
  ),
  
  `Model 2 (Demographic adjusted)` = c(
    sprintf("%.2f (%.2f-%.2f)", OR2_cont[2], CI2_cont[2,1], CI2_cont[2,2]),
    "",
    sprintf("%.2f (%.2f-%.2f)", OR2_quart[2], CI2_quart[2,1], CI2_quart[2,2]),
    sprintf("%.2f (%.2f-%.2f)", OR2_quart[3], CI2_quart[3,1], CI2_quart[3,2]),
    sprintf("%.2f (%.2f-%.2f)", OR2_quart[4], CI2_quart[4,1], CI2_quart[4,2]),
    sprintf("%.3f", coef(summary(model2_trend))[2, 4])
  ),
  
  `Model 3 (Fully adjusted)` = c(
    sprintf("%.2f (%.2f-%.2f)", OR3_cont[2], CI3_cont[2,1], CI3_cont[2,2]),
    "",
    sprintf("%.2f (%.2f-%.2f)", OR3_quart[2], CI3_quart[2,1], CI3_quart[2,2]),
    sprintf("%.2f (%.2f-%.2f)", OR3_quart[3], CI3_quart[3,1], CI3_quart[3,2]),
    sprintf("%.2f (%.2f-%.2f)", OR3_quart[4], CI3_quart[4,1], CI3_quart[4,2]),
    sprintf("%.3f", coef(summary(model3_trend))[2, 4])
  )
)

cat("\n\n========== 最终结果表格==========\n")
print(final_results)

#======基于模型3（完全调整模型）绘制森林图=====

# 创建森林图
quartile_data <- data.frame(
  Quartile = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
  OR = c(1.00, 0.87, 0.92, 0.62),
  Lower = c(1.00, 0.48, 0.52, 0.33),
  Upper = c(1.00, 1.57, 1.64, 1.19)
)

# 森林图

forestplot(
  labeltext = cbind(Quartile = quartile_data$Quartile, 
                    `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", 
                                            quartile_data$OR, 
                                            quartile_data$Lower, 
                                            quartile_data$Upper)),
  mean = quartile_data$OR,
  lower = quartile_data$Lower,
  upper = quartile_data$Upper,
  xlab = "Odds Ratio (95% CI) for Heart Failure",
  txt_gp = fpTxtGp(label = gpar(cex = 0.8)),
  title = "Association between ELD Score Quartiles and Heart Failure",
  col = fpColors(box = "steelblue", line = "steelblue"),
  zero = 1,
  xlog = TRUE,
  boxsize = 0.1
)

#=========分层分析("Age", "Sex", "Smoking", "Alcohol", "PhysicalActivity", "BMI", "Hypertension", "Diabetes","Dyslipidemia","Energy")========
#第一步：准备数据
analysis_data <- analysis_data %>%
  mutate(
    # 因变量
    HF_binary = as.numeric(HF_binary),
    ELD_total_score = as.numeric(ELD_total_score),
    
    # ELD四分位数
    ELD_quartile = cut(ELD_total_score,
                       breaks = quantile(ELD_total_score, 
                                         probs = c(0, 0.25, 0.5, 0.75, 1), 
                                         na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = c("Q1", "Q2", "Q3", "Q4")),
    
    # 分层变量（连续变量）
    Age_group = ifelse(Age < 60, "<60", "≥60"),
    BMI_group = ifelse(BMI < 30, "<30", "≥30"),
    
    # 趋势检验变量
    ELD_trend = as.numeric(ELD_quartile)
  )
# 第二步：创建调查设计对象
design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = analysis_data
)
# 第三步：定义基础协变量（将协变量存入列表，后续循环拼接公式）

base_covariates <- c("Age", "BMI","Energy")
all_covariates <- c("Age", "Sex", "Race", "Education", 
                    "Smoking", "Alcohol", "PhysicalActivity", 
                    "BMI", "Hypertension", "Diabetes","Dyslipidemia","Energy")

# 第四步：自定义分层分析函数（动态构建公式）
run_stratified_analysis <- function(data_design, strata_var, strata_level) {
  
  # 子集设计（筛选当前亚组水平，只筛选出当前这个亚组的样本）
  subset_design <- subset(data_design, data_design$variables[[strata_var]] == strata_level)
  
  # 样本量信息（统计当前亚组的总样本量、结局事件数）
  n_total <- nrow(subset_design$variables)
  n_events <- sum(subset_design$variables$HF_binary == 1, na.rm = TRUE)
  
  if(n_events < 5) {
    return(data.frame(
      Stratum = strata_level, #当前亚组名称
      N = n_total, #该亚组总样本量
      Events = n_events, #心衰病例数
      Q2_OR = NA, Q2_lower = NA, Q2_upper = NA,
      Q3_OR = NA, Q3_lower = NA, Q3_upper = NA,
      Q4_OR = NA, Q4_lower = NA, Q4_upper = NA,
      P_trend = NA,
      Note = "Insufficient events"#备注
    ))
  }
  
  # 动态构建公式：排除当前分层的变量
  # 需要排除的变量：strata_var 本身
  exclude_vars <- strata_var
  
  # 筛选当前亚组中可用的协变量
  available_covariates <- c()
  for(cov in all_covariates) {
    if(cov != strata_var) {
      # 检查该变量在子集中是否有至少2个水平
      var_values <- subset_design$variables[[cov]]
      if(length(unique(var_values[!is.na(var_values)])) >= 2) {
        available_covariates <- c(available_covariates, cov)
      }
    }
  }
  
  # 动态构建四分位数回归公式
  if(length(available_covariates) > 0) {
    formula_str <- paste("HF_binary ~ ELD_quartile +", paste(available_covariates, collapse = " + "))
  } else {
    formula_str <- "HF_binary ~ ELD_quartile"
  }
  
  # 把上面拼接好的字符串公式，转换成 R 回归函数能识别的公式格式，用于拟合加权 Logistic 回归
  quartile_fml <- as.formula(formula_str)
  
  # 构建趋势检验回归公式
  if(length(available_covariates) > 0) {
    trend_formula_str <- paste("HF_binary ~ ELD_trend +", paste(available_covariates, collapse = " + "))
  } else {
    trend_formula_str <- "HF_binary ~ ELD_trend"
  }
  trend_fml <- as.formula(trend_formula_str)
  
  cat("\n  使用的协变量:", if(length(available_covariates)>0) paste(available_covariates, collapse=", ") else "无", "\n")
  
  # 拟合四分位数模型
  model <- tryCatch({
    svyglm(quartile_fml, design = subset_design, family = quasibinomial())
  }, error = function(e) {
    cat("  错误:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(model)) {
    return(data.frame(
      Stratum = strata_level,
      N = n_total,
      Events = n_events,
      Q2_OR = NA, Q2_lower = NA, Q2_upper = NA,
      Q3_OR = NA, Q3_lower = NA, Q3_upper = NA,
      Q4_OR = NA, Q4_lower = NA, Q4_upper = NA,
      P_trend = NA,
      Note = "Model failed"
    ))
  }
  
  # 模型拟合成功，则提取结果
  coef_model <- coef(model)
  ci_model <- confint(model)
 
  # 自定义函数（根据变量位置索引，批量提取OR、CI_lower、CI_upper） 
  get_or_ci <- function(idx) {
    if(length(idx) == 0) return(c(NA, NA, NA))
    or <- exp(coef_model[idx])
    ci <- exp(ci_model[idx, ])
    return(c(or, ci[1], ci[2]))
  }
  
  # 匹配Q2、Q3、Q4 三组回归系数的位置
  q2_idx <- grep("ELD_quartileQ2", names(coef_model))
  q3_idx <- grep("ELD_quartileQ3", names(coef_model))
  q4_idx <- grep("ELD_quartileQ4", names(coef_model))
  
  # 调用函数批量获取三组统计结果
  q2_res <- get_or_ci(q2_idx)
  q3_res <- get_or_ci(q3_idx)
  q4_res <- get_or_ci(q4_idx)
  
  # 趋势检验
  trend_model <- tryCatch({
    svyglm(trend_fml, design = subset_design, family = quasibinomial())
  }, error = function(e) {
    return(NULL)
  })
  
  # 提取趋势检验 P 值
  p_trend <- NA
  if(!is.null(trend_model)) {
    coef_trend <- coef(summary(trend_model))
    if("ELD_trend" %in% rownames(coef_trend)) {
      p_trend <- coef_trend["ELD_trend", 4]
    }
  }
  
  #整理并以数据框形式返回当前亚组的全部统计结果
  return(data.frame(
    Stratum = strata_level,
    N = n_total,
    Events = n_events,
    Q2_OR = round(q2_res[1], 2),
    Q2_lower = round(q2_res[2], 2),
    Q2_upper = round(q2_res[3], 2),
    Q3_OR = round(q3_res[1], 2),
    Q3_lower = round(q3_res[2], 2),
    Q3_upper = round(q3_res[3], 2),
    Q4_OR = round(q4_res[1], 2),
    Q4_lower = round(q4_res[2], 2),
    Q4_upper = round(q4_res[3], 2),
    P_trend = round(p_trend, 3),
    Note = "OK"
  ))
}

# ---------第五步：交互作用P值计算-------
# 自定义函数
calculate_interaction_p <- function(data_design, strata_var) {
  
# 数据预处理
  temp_design <- data_design
  temp_design$variables$ELD_trend <- as.numeric(temp_design$variables$ELD_quartile)
  temp_design$variables$subgroup <- temp_design$variables[[strata_var]]
  temp_design$variables$subgroup <- factor(temp_design$variables$subgroup)
 
  #限制有2个水平的分组变量 
  if(nlevels(temp_design$variables$subgroup) != 2) {
    return(NA)
  }
  
  # 检查各水平样本量，规定每个亚组样本量≥30
  level_counts <- table(temp_design$variables$subgroup)
  if(any(level_counts < 30)) {
    return(NA)
  }
  
  # 构建带交互项的加权logistic模型
  interaction_model <- tryCatch({
    svyglm(
      HF_binary ~ ELD_trend * subgroup + Age + BMI + Hypertension + Diabetes,
      design = temp_design,
      family = quasibinomial()
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if(is.null(interaction_model)) {
    return(NA)
  }
  
  # 提取交互项p值
  coef_sum <- coef(summary(interaction_model))
  interaction_term <- grep("ELD_trend:subgroup", rownames(coef_sum))
  
  if(length(interaction_term) > 0) {
    p_val <- coef_sum[interaction_term, 4]
    return(round(p_val, 4))
  }
  
  return(NA)
}
# ---------第六步：执行分层分析-------
cat("========================================\n")
cat("分层分析结果\n")
cat("========================================\n")

# 定义分层变量和水平
strata_list <- list(
  Age_group = c("<60", "≥60"),
  Sex = c("Male", "Female"),
  BMI_group = c("<30", "≥30"),
  Smoking = c("Non-smoker", "Current smoker"),
  Alcohol = c("Non-drinker", "Drinker"),
  PhysicalActivity = c("Inactive", "Active"),
  Hypertension = c("No", "Yes"),
  Diabetes = c("No", "Yes"),
  Dyslipidemia = c("No", "Yes")
)

# 存储结果
all_results <- data.frame()
interaction_results <- data.frame()

for(strata_var in names(strata_list)) {
  cat("\n========================================\n")
  cat("分层变量:", strata_var, "\n")
  cat("========================================\n")
  
  strata_levels <- strata_list[[strata_var]]
  strata_results <- data.frame()
  
  for(level in strata_levels) {
    cat("\n  亚组:", level, "\n")
    
    result <- run_stratified_analysis(
      data_design = design,
      strata_var = strata_var,
      strata_level = level
    )
    
    strata_results <- bind_rows(strata_results, result)
    cat("  样本量:", result$N, ", 事件数:", result$Events, "\n")
  }
  
  # 计算交互作用P值
  p_interaction <- calculate_interaction_p(design, strata_var)
  cat("\n  交互作用P值:", ifelse(is.na(p_interaction), "NA", sprintf("%.4f", p_interaction)), "\n")
  
  # 将交互作用P值添加到当前亚组结果表，记录分层变量
  strata_results$P_interaction <- p_interaction
  strata_results$Stratum_Variable <- strata_var
  
  # 汇总两张结果表
  interaction_results <- bind_rows(
    interaction_results,
    data.frame(Subgroup = strata_var, P_interaction = p_interaction)
  )
  
  all_results <- bind_rows(all_results, strata_results)
}
# ------第七步：格式化输出表格-----
final_table <- all_results %>%
  mutate(
    Q2_Display = ifelse(is.na(Q2_OR), "-", sprintf("%.2f (%.2f-%.2f)", Q2_OR, Q2_lower, Q2_upper)),
    Q3_Display = ifelse(is.na(Q3_OR), "-", sprintf("%.2f (%.2f-%.2f)", Q3_OR, Q3_lower, Q3_upper)),
    Q4_Display = ifelse(is.na(Q4_OR), "-", sprintf("%.2f (%.2f-%.2f)", Q4_OR, Q4_lower, Q4_upper)),
    P_trend_display = ifelse(is.na(P_trend), "-", sprintf("%.3f", P_trend))
  ) %>%
  select(Stratum_Variable, Stratum, N, Events, 
         Q2_Display, Q3_Display, Q4_Display, 
         P_trend_display, P_interaction, Note)

cat("\n\n========================================\n")
cat("分层分析结果汇总表\n")
cat("========================================\n\n")
print(final_table)

#=========分层分析+ELD连续得分========

# 第八步：ELD连续得分分层分析函数

run_stratified_continuous <- function(data_design, strata_var, strata_level) {
  
  # 子集设计
  subset_design <- subset(data_design, data_design$variables[[strata_var]] == strata_level)
  
  # 样本量信息
  n_total <- nrow(subset_design$variables)
  n_events <- sum(subset_design$variables$HF_binary == 1, na.rm = TRUE)
  
  if(n_events < 5) {
    return(data.frame(
      Stratum = strata_level,
      N = n_total,
      Events = n_events,
      OR = NA, Lower = NA, Upper = NA,
      P_value = NA,
      Note = "Insufficient events"
    ))
  }
  
  # 动态构建公式：排除当前分层的变量
  available_covariates <- c()
  for(cov in all_covariates) {
    if(cov != strata_var) {
      var_values <- subset_design$variables[[cov]]
      if(length(unique(var_values[!is.na(var_values)])) >= 2) {
        available_covariates <- c(available_covariates, cov)
      }
    }
  }
  
  # 构建连续变量公式
  if(length(available_covariates) > 0) {
    formula_str <- paste("HF_binary ~ ELD_total_score +", paste(available_covariates, collapse = " + "))
  } else {
    formula_str <- "HF_binary ~ ELD_total_score"
  }
  
  cont_fml <- as.formula(formula_str)
  
  cat("\n  使用的协变量:", if(length(available_covariates)>0) paste(available_covariates, collapse=", ") else "无", "\n")
  
  # 拟合模型
  model <- tryCatch({
    svyglm(cont_fml, design = subset_design, family = quasibinomial())
  }, error = function(e) {
    cat("  错误:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(model)) {
    return(data.frame(
      Stratum = strata_level,
      N = n_total,
      Events = n_events,
      OR = NA, Lower = NA, Upper = NA,
      P_value = NA,
      Note = "Model failed"
    ))
  }
  
  # 提取ELD_total_score结果
  coef_model <- coef(model)
  ci_model <- confint(model)
  
  eld_idx <- grep("ELD_total_score", names(coef_model))
  
  if(length(eld_idx) == 0) {
    return(data.frame(
      Stratum = strata_level,
      N = n_total,
      Events = n_events,
      OR = NA, Lower = NA, Upper = NA,
      P_value = NA,
      Note = "ELD not in model"
    ))
  }
  
  or <- exp(coef_model[eld_idx])
  ci <- exp(ci_model[eld_idx, ])
  p_val <- coef(summary(model))[eld_idx, 4]
  
  return(data.frame(
    Stratum = strata_level,
    N = n_total,
    Events = n_events,
    OR = round(or, 2),
    Lower = round(ci[1], 2),
    Upper = round(ci[2], 2),
    P_value = round(p_val, 4),
    Note = "OK"
  ))
}

# 第九步：执行ELD连续得分分层分析
cat("\n========================================\n")
cat("ELD连续得分分层分析\n")
cat("========================================\n")


# 存储连续得分结果
continuous_results <- data.frame()

for(strata_var in names(strata_list)) {
  cat("\n---", strata_var, "---\n")
  
  for(level in strata_list[[strata_var]]) {
    cat("  ", level, "...")
    
    result <- run_stratified_continuous(
      data_design = design,
      strata_var = strata_var,
      strata_level = level
    )
    
    # 添加分组信息
    result$Stratum_Variable <- strata_var
    continuous_results <- bind_rows(continuous_results, result)
    
    cat(" N=", result$N, ", Events=", result$Events, "\n", sep = "")
  }
}

# 格式化连续得分结果
continuous_final <- continuous_results %>%
  mutate(
    OR_Display = ifelse(is.na(OR), "-", sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper)),
    P_display = ifelse(is.na(P_value), "-", sprintf("%.4f", P_value))
  ) %>%
  select(Stratum_Variable, Stratum, N, Events, OR_Display, P_display, Note)

cat("\n\n========================================\n")
cat("ELD连续得分分层分析汇总表\n")
cat("========================================\n\n")
print(continuous_final)

# =========合并 final_table 和 continuous_final======
# 从 continuous_final 提取需要的列并重命名
continuous_merge <- continuous_final %>%
  select(Stratum_Variable, Stratum, OR_Display, P_display) %>%
  rename(
    Continuous_OR = OR_Display,
    Continuous_P = P_display
  )

# 合并到 final_table
final_table_merged <- final_table %>%
  left_join(continuous_merge, 
            by = c("Stratum_Variable" = "Stratum_Variable", 
                   "Stratum" = "Stratum"))

#调整列顺序（将Continuous_OR和Continuous_P放在P_trend_display后面）
final_table_merged <- final_table_merged %>%
  select(Stratum_Variable, Stratum, N, Events,Continuous_OR, Continuous_P,
         Q2_Display, Q3_Display, Q4_Display, P_trend_display,
         P_interaction, Note)

# 查看结果
print(final_table_merged)
write.csv(final_table_merged, "Logistic stratification analysis.csv", row.names = FALSE)

# ==========生成三线表==================
library(knitr)
library(kableExtra)
# 准备表格数据
table_for_print <- final_table_merged %>%
  mutate(
    # 格式化连续变量
    Continuous_OR = ifelse(is.na(Continuous_OR), "-", Continuous_OR),
    Continuous_P = ifelse(is.na(Continuous_P), "-", Continuous_P),
    # 格式化交互P值
    P_interaction = ifelse(is.na(P_interaction), "-", sprintf("%.4f", P_interaction)),
    # 添加分组标题行
    Group = case_when(
      Stratum_Variable == "Age_group" ~ "Age",
      Stratum_Variable == "Sex" ~ "Sex",
      Stratum_Variable == "BMI_group" ~ "BMI",
      Stratum_Variable == "Smoking" ~ "Smoking",
      Stratum_Variable == "Alcohol" ~ "Alcohol",
      Stratum_Variable == "PhysicalActivity" ~ "Physical Activity",
      Stratum_Variable == "Hypertension" ~ "Hypertension",
      Stratum_Variable == "Diabetes" ~ "Diabetes",
      Stratum_Variable == "Dyslipidemia" ~ "Dyslipidemia",
      TRUE ~ Stratum_Variable
    )
  ) %>%
  select(Group, Stratum, N, Events, Continuous_OR, Continuous_P,
         Q2_Display, Q3_Display, Q4_Display, P_trend_display,
         P_interaction, Note)

# 生成三线表
kable(table_for_print, 
      caption = "Table. Subgroup Analysis of ELD Score and Heart Failure",
      col.names = c("Group", "Subgroup", "N", "Events", 
                    "Continuous OR (95% CI)", "P value",
                    "Q2 OR (95% CI)", "Q3 OR (95% CI)", "Q4 OR (95% CI)", 
                    "P for trend", "P for interaction", "Note"),
      align = c("l", "l", "r", "r", "c", "c", "c", "c", "c", "c", "c", "c"),
      digits = 2) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  add_header_above(c(" " = 2, " " = 2, "Continuous" = 2, "Quartiles" = 4, " " = 2)) %>%
  row_spec(0, bold = TRUE, color = "black", background = "white") %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle")
# ===============以ELD连续得分为主绘制分层分析的森林图====================


# 准备森林图数据（使用连续得分结果）
forest_data_cont <- continuous_results %>%
  filter(!is.na(OR), !is.na(Lower), !is.na(Upper)) %>%
  mutate(
    # 创建显示标签
    Subgroup_Label = case_when(
      Stratum == "<60" ~ "age <60 years",
      Stratum == "≥60" ~ "age ≥60 years",
      Stratum == "Male" ~ "Male",
      Stratum == "Female" ~ "Female",
      Stratum == "<30" ~ "BMI <30",
      Stratum == "≥30" ~ "BMI ≥30",
      Stratum == "Non-smoker" ~ "Non-smoker",
      Stratum == "Current smoker" ~ "Current smoker",
      Stratum == "Non-drinker" ~ "Non-drinker",
      Stratum == "Drinker" ~ "Drinker",
      Stratum == "Inactive" ~ "Inactive",
      Stratum == "Active" ~ "Active",
      Stratum == "No" & Stratum_Variable == "Hypertension" ~ "No hypertension",
      Stratum == "Yes" & Stratum_Variable == "Hypertension" ~ "Hypertension",
      Stratum == "No" & Stratum_Variable == "Diabetes" ~ "No diabetes",
      Stratum == "Yes" & Stratum_Variable == "Diabetes" ~ "Diabetes",
      Stratum == "No" & Stratum_Variable == "Dyslipidemia" ~ "No dyslipidemia",
      Stratum == "Yes" & Stratum_Variable == "Dyslipidemia" ~ "Dyslipidemia",
      TRUE ~ Stratum
    ),
    # 分组
    Group = case_when(
      Stratum %in% c("<60", "≥60") ~ "Age",
      Stratum %in% c("Male", "Female") ~ "Sex",
      Stratum %in% c("<30", "≥30") ~ "BMI",
      Stratum %in% c("Non-smoker", "Current smoker") ~ "Smoking",
      Stratum %in% c("Non-drinker", "Drinker") ~ "Alcohol",
      Stratum %in% c("Inactive", "Active") ~ "Physical Activity",
      Stratum %in% c("No", "Yes") & Stratum_Variable == "Hypertension" ~ "Hypertension",
      Stratum %in% c("No", "Yes") & Stratum_Variable == "Diabetes" ~ "Diabetes",
      Stratum %in% c("No", "Yes") & Stratum_Variable == "Dyslipidemia" ~ "Dyslipidemia",
      TRUE ~ "Other"
    ),
    # 固定顺序
    Order = case_when(
      Stratum == "<60" ~ 1,
      Stratum == "≥60" ~ 2,
      Stratum == "Male" ~ 3,
      Stratum == "Female" ~ 4,
      Stratum == "<30" ~ 5,
      Stratum == "≥30" ~ 6,
      Stratum == "Non-smoker" ~ 7,
      Stratum == "Current smoker" ~ 8,
      Stratum == "Non-drinker" ~ 9,
      Stratum == "Drinker" ~ 10,
      Stratum == "Inactive" ~ 11,
      Stratum == "Active" ~ 12,
      Stratum == "No hypertension" ~ 13,
      Stratum == "Hypertension" ~ 14,
      Stratum == "No diabetes" ~ 15,
      Stratum == "Diabetes" ~ 16,
      Stratum == "No dyslipidemia" ~ 17,
      Stratum == "Dyslipidemia" ~ 18,
      TRUE ~ 99
    )
  ) %>%
  arrange(Order)

# 设置因子顺序
forest_data_cont$Subgroup_Label <- factor(forest_data_cont$Subgroup_Label, 
                                          levels = rev(forest_data_cont$Subgroup_Label))

# 分组颜色
group_colors <- c(
  "Age" = "#E41A1C",
  "Sex" = "#377EB8",
  "BMI" = "#4DAF4A",
  "Smoking" = "#984EA3",
  "Alcohol" = "#FF7F00",
  "Physical Activity" = "#FDBF6F",
  "Hypertension" = "#A65628",
  "Diabetes" = "#F781BF",
  "Dyslipidemia" = "#999999"
)


# 绘制森林图

# 计算需要的X轴范围（为标签留出空间）
x_max_label <- max(forest_data_cont$Upper, na.rm = TRUE) * 1.25

forest_data_cont <- forest_data_cont %>%
  mutate(
    OR_CI_Label = sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper)
  )
p1_cont <- ggplot(forest_data_cont, aes(x = OR, y = Subgroup_Label, color = Group)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), 
                 height = 0.15, size = 0.8, color = "black") +
  geom_point(size = 3) +
  # 添加文本标签（右对齐）
  geom_text(aes(x = Upper * 1.08, label = OR_CI_Label), 
            size = 3.2, hjust = 0, color = "black") +
  scale_color_manual(values = group_colors) +
  scale_x_log10(limits = c(0.6, x_max_label), 
                breaks = c(0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5)) +
  labs(
    title = "Subgroup Analysis: ELD Score (per 1-point increase) and Heart Failure",
    x = "Odds Ratio (95% CI)",
    y = "",
    color = "Subgroup"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p1_cont)
