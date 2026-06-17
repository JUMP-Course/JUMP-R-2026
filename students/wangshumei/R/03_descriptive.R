##=================描述性统计与基线表========================
install.packages("tableone", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(tableone)

vars <- c("age_at_index", "treatment_type", "figo_stage_group")

# 分类变量
cat_vars <- c("treatment_type", "figo_stage_group")

# 1. 生成总体基线特征表
table1_overall <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  data = df_cox,  
)
# 创建基线特征表（按肿瘤分级分组）
table1 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  strata = "tumor_grade_sens",  # 按肿瘤分级分组
  data = df_cox,
)


print(table1_overall, quote = FALSE, noSpaces = TRUE, printToggle = TRUE)
print(table1, quote = FALSE, noSpaces = TRUE)

# 保存为 CSV
write.csv(print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE), 
          "baseline_table_by_grade.csv")
write.csv(print(table1_overall, quote = FALSE, noSpaces = TRUE, printToggle = FALSE), 
          "baseline_table_overall.csv")

cat("\n基线特征表已保存为: baseline_table_by_grade.csv\n")
cat("\n基线特征表已保存为: baseline_table_overall.csv\n")
#————————————————————————————————————————————————————————————————————————————————