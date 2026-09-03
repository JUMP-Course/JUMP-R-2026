#=========================5.基础统计分析=================================
library(car)
library(FSA)

# 1.年龄正态性检验
norm_test <- aggregate(age_at_index ~ tumor_grade_sens, 
                       data = df_cox, 
                       FUN = function(x) shapiro.test(x)$p.value)
names(norm_test)[2] <- "Shapiro_P"
print(norm_test)

# 年龄的 Kruskal-Wallis 检验
kruskal.test(age_at_index ~ tumor_grade_sens, data = df_cox)

#修改表1
library(tableone)
# 1. 定义变量
vars <- c("age_at_index", "figo_stage_group", "treatment_type")
cat_vars <- c("figo_stage_group", "treatment_type")

# 2. 确保分组变量是因子
df_cox$tumor_grade_sens <- factor(df_cox$tumor_grade_sens, 
                                  levels = c("G1", "G2", "G3", "Unknown"))

# 3. 生成基线表（关键：指定 nonnormal 参数！）
table1 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  strata = "tumor_grade_sens",
  data = df_cox,
  test = TRUE,
)

print(table1, 
      quote = FALSE, 
      noSpaces = TRUE, 
      showAllLevels = TRUE,
      nonnormal = "age_at_index") 

library(openxlsx)
table1_df <- print(table1, quote = FALSE, noSpaces = TRUE, showAllLevels = TRUE, nonnormal = "age_at_index")
write.xlsx(table1_df, "baseline_table.xlsx")

#------------------------------------------------------------------------------
# 2.治疗方式和FIGO分期的卡方检验
tab_treatment <- table(df_cox$tumor_grade_sens, df_cox$treatment_type)
tab_figo <- table(df_cox$tumor_grade_sens, df_cox$figo_stage_group)

# 卡方检验
chisq.test(tab_treatment)
fisher.test(tab_figo, simulate.p.value = TRUE, B = 10000)  # B=模拟次数，可调整
#———————————————————————————————————————————————————————————————————————————————