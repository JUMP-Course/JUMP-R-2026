##======================数据清洗============================
# 1. 创建一个新变量 `figo_stage_group`，用于存放整合后的四大期
df_final <- df_final %>%
  mutate(figo_stage_group = case_when(
    figo_stage %in% c("Stage I", "Stage IA", "Stage IA2", "Stage IB", "Stage IB1", "Stage IB2") ~ "Stage I",
    figo_stage %in% c("Stage II", "Stage IIA", "Stage IIA1", "Stage IIA2", "Stage IIB") ~ "Stage II",
    figo_stage %in% c("Stage III", "Stage IIIA", "Stage IIIB") ~ "Stage III",
    figo_stage %in% c("Stage IVA", "Stage IVB") ~ "Stage IV",
    TRUE ~ "Unknown"  # 其他情况（如 '--', 'Stage X' 等）归为未知
  ))

# 将新变量转换为因子，并指定顺序
df_final$figo_stage_group <- factor(df_final$figo_stage_group,
                                    levels = c("Stage I", "Stage II", "Stage III", "Stage IV", "Unknown"),
                                    ordered = TRUE) # 设为有序因子，保留趋势信息

# 验证整合结果
table(df_final$figo_stage_group, useNA = "ifany")

#----------------------------------------------------------------------------------

# 2.对治疗方式列进行整理：移除可能的空格和统一大小写
df_final$all_treatments_clean <- trimws(tolower(as.character(df_final$all_treatments)))

# 创建二分类变量：Surgery vs Non-Surgery
df_final$treatment_type <- ifelse(grepl("hysterectomy", df_final$all_treatments_clean), 
                                  "Surgery", 
                                  "Non-Surgery")

# 转化为因子并设置参照组
df_final$treatment_type <- factor(df_final$treatment_type, 
                                  levels = c("Non-Surgery", "Surgery"))

# 检查转换结果
table(df_final$treatment_type)
#——————————————————————————————————————————————————————————————————————————————————————————
# 3.主分析
# 删除 tumor_grade 为 ''--' 或 'GX' 的记录
df_main <- df_final %>%
  filter(tumor_grade %in% c("G1", "G2", "G3")) %>%
  mutate(tumor_grade_clean = factor(tumor_grade, levels = c("G1", "G2", "G3")))

# 检查结果
cat("主分析样本量:", nrow(df_main), "\n")
table(df_main$tumor_grade_clean, useNA = "ifany")

# 主分析 Cox 模型
cox_main <- coxph(Surv(time, status) ~ tumor_grade + age_at_index, data = df_main)
summary(cox_main)

# 4.敏感性分析
df_sensitivity <- df_final  
df_sensitivity <- df_sensitivity %>%
  mutate(
    tumor_grade_sens = case_when(
      tumor_grade %in% c("G1", "G2", "G3") ~ tumor_grade,
      TRUE ~ "Unknown"  
    )
  ) %>%
  mutate(
    tumor_grade_sens = factor(
      tumor_grade_sens,
      levels = c("G1", "G2", "G3", "Unknown")
    )
  )

library(dplyr)
df_cox <- df_sensitivity %>%
  filter(
    !is.na(time), 
    !is.na(status), 
    !is.na(tumor_grade_sens), 
    !is.na(age_at_index)
  )
# 检查结果
cat("敏感性分析样本量:", nrow(df_cox), "\n")
table(df_cox$tumor_grade_sens, useNA = "ifany")

cox_sensitivity<- coxph(Surv(time, status) ~ tumor_grade_sens + age_at_index, data = df_cox)
summary(cox_sensitivity)


# ---------- 主分析结果提取 --------------------------------
# 加载 knitr 包（用于输出美观表格）
library(knitr)
hr_main <- exp(coef(cox_main))
ci_main <- exp(confint(cox_main))
p_main <- summary(cox_main)$coefficients[, "Pr(>|z|)"]

main_results <- data.frame(
  Analysis = "Main",
  Variable = names(hr_main),
  HR = round(hr_main, 2),
  Lower_CI = round(ci_main[, 1], 2),
  Upper_CI = round(ci_main[, 2], 2),
  P_value = round(p_main, 4)
)

# ---------- 敏感性分析结果提取 ----------
hr_sens <- exp(coef(cox_sensitivity))
ci_sens <- exp(confint(cox_sensitivity))
p_sens <- summary(cox_sensitivity)$coefficients[, "Pr(>|z|)"]

sens_results <- data.frame(
  Analysis = "Sensitivity",
  Variable = names(hr_sens),
  HR = round(hr_sens, 2),
  Lower_CI = round(ci_sens[, 1], 2),
  Upper_CI = round(ci_sens[, 2], 2),
  P_value = round(p_sens, 4)
)

# ---------- 合并为一个表格 ----------
combined_results <- rbind(main_results, sens_results)

# ---------- 输出表格 ----------
kable(combined_results, 
      caption = "Table: Cox Regression Results - Main and Sensitivity Analysis",
      col.names = c("Analysis", "Variable", "HR", "95% CI Lower", "95% CI Upper", "P value"))

# ---------- 保存为 CSV 文件----------
write.csv(combined_results, "cox_main_and_sensitivity_results.csv", row.names = FALSE)
cat("\n主分析和敏感性分析结果已保存为: cox_main_and_sensitivity_results.csv\n")


