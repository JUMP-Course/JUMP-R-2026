##======================数据可视化==============================
# 1. 绘制Kaplan——Meier生存曲线
km_fit <- survfit(Surv(time, status) ~ tumor_grade_sens, data = df_cox)
library(survminer)

ggsurvplot(
  km_fit,
  data = df_cox,
  pval = TRUE,           # 自动计算并显示 Log-rank 检验 p 值
  conf.int = TRUE,       # 显示置信区间
  risk.table = TRUE,     # 显示风险人数表
  palette = c("#E7B800", "#2E9FDF", "#FC4E07", "#999999"),
  legend.title = "Tumor Grade",
  legend.labs = c("G1", "G2", "G3","unknown"),
  xlab = "Time (Days)",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Tumor Grade",
  ggtheme = theme_minimal()
)
#--------------------------------------------------------------------------

# 2.单因素 Cox 回归分析
library(broom)
names(df_cox)

# 肿瘤分级（分类变量）
univ_grade <- coxph(Surv(time, status) ~ tumor_grade_sens, data = df_cox)

# 年龄（连续变量）
univ_age <- coxph(Surv(time, status) ~ age_at_index, data = df_cox)

# 肿瘤分期（分类变量）
univ_stage <- coxph(Surv(time, status) ~ figo_stage_group, data = df_cox)

# 治疗方式（分类变量）
univ_treatment <- coxph(Surv(time, status) ~ treatment_type, data = df_cox)

#  汇总结果并导出表格
results_list <- list(univ_grade, univ_age, univ_stage, univ_treatment)
var_names <- c("Tumor Grade", "Age", "FIGO Stage", "Treatment Type")

# 提取 HR、95%CI 和 P 值
final_table <- data.frame()

for(i in 1:length(results_list)) {
  model <- results_list[[i]]
  summary_model <- summary(model)
  
  # 提取系数表
  coef_table <- summary_model$coefficients
  confint_table <- summary_model$conf.int
  
  # 整理成表格
  temp_df <- data.frame(
    Variable = var_names[i],
    Level = rownames(coef_table),
    HR = round(confint_table[, "exp(coef)"], 2),
    Lower_CI = round(confint_table[, "lower .95"], 2),
    Upper_CI = round(confint_table[, "upper .95"], 2),
    P_value = round(coef_table[, "Pr(>|z|)"], 4)
  )
  
  final_table <- rbind(final_table, temp_df)
}

# 加载 knitr 包
library(knitr)

# 美化输出（复制到 Excel 或 Word）
kable(final_table, 
      digits = c(0, 0, 2, 2, 2, 4),   # 列小数位数：Variable(0), Level(0), HR(2), Lower_CI(2), Upper_CI(2), P_value(4)
      align = "lrrrrr",                 # 左对齐文字，右对齐数字
      caption = "单因素 Cox 回归结果")   # 表格标题
# 打印结果
print("===== 单因素 Cox 回归结果 =====")
print(final_table)

write.csv(final_table, "univariate_cox_results.csv", row.names = FALSE)
#--------------------------------------------------------------------------------

##3.多因素cox回归模型分析
multiv_model <- coxph(
  Surv(time, status) ~ tumor_grade_sens + age_at_index + figo_stage_group + treatment_type , 
  data = df_cox
)

# 查看多因素模型的结果
summary(multiv_model)

summ_multiv <- summary(multiv_model)
multiv_results <- data.frame(
  Variable = rownames(summ_multiv$coefficients),
  Level = rownames(summ_multiv$coefficients),
  HR = round(summ_multiv$conf.int[, "exp(coef)"], 2),
  Lower_CI = round(summ_multiv$conf.int[, "lower .95"], 2),
  Upper_CI = round(summ_multiv$conf.int[, "upper .95"], 2),
  P_value = round(summ_multiv$coefficients[, "Pr(>|z|)"], 4)
)

#  导出
write.csv(multiv_results, "mulvariate_Cox_Results.csv", row.names = FALSE)
#-------------------------------------------------------------------------

## 4. 森林图
library(forestmodel)

forest_model(multiv_model, 
             format_options = forest_model_format_options(colour = "black")) 
#————————————————————————————————————————————————————————--——————————————————-——