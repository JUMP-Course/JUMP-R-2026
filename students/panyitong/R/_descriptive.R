install.packages(c("gtsummary","dplyr","flextable","survey","tidyr"), repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(survey)
library(dplyr)
library(flextable)
library(tidyr)

# 抽样设计
design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = clean_data,
  nest = TRUE)

# 完整变量列表
vars <- c("smoking", "age_group", "gender", "race", "bmi_group", "education", "pir_group")
cat_table <- data.frame()

# 循环计算加权频数百分比
for(var in vars) {
  tab_wt <- svytable(as.formula(paste0("~", var, " + hypertension")), design = design, na.rm = TRUE)
  tab_df <- as.data.frame(tab_wt)
  colnames(tab_df)[1] <- "category"
  tab_df <- tab_df %>%
    group_by(hypertension) %>%
    mutate(percent = Freq / sum(Freq) * 100) %>%
    ungroup()
  tab_df$variable <- var
  cat_table <- bind_rows(cat_table, tab_df)}

# 循环计算加权卡方P值
p_values <- c()
for(var in vars) {
  formula <- as.formula(paste0("~", var, " + hypertension"))
  chi_test <- svychisq(formula, design = design)
  p_values[var] <- ifelse(chi_test$p.value < 0.001, "<0.001", as.character(round(chi_test$p.value, 3)))
}
p_df <- data.frame(variable = vars, `P值` = p_values)

# 转为宽表，拼接P值
table_wide <- cat_table %>%
  mutate(content = paste0(round(Freq, 0), " (", round(percent, 1), "%)")) %>%
  select(variable, category, hypertension, content) %>%
  pivot_wider(names_from = hypertension, values_from = content) %>%
  left_join(p_df, by = "variable")

# 生成标准三线表
ft <- flextable(table_wide) %>%
  theme_booktabs() %>%
  merge_v(j = ~variable) %>%
  set_header_labels(
    variable = "变量",
    category = "分类",
    `0` = "非高血压组",
    `1` = "高血压组") %>%
  autofit()

# 预览+导出
print(ft)
save_as_docx(ft, path = "~/GitHub/JUMP-R-2026/students/panyitong/tables/高血压基线特征完整三线表.docx")









