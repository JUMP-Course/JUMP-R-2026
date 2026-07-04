install.packages(c("gtsummary","dplyr","flextable","survey","tidyr"), repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(survey)
library(dplyr)
library(flextable)
library(tidyr)
library(officer)

# ============================================================
# # 基线特征
# ============================================================

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
  left_join(p_df, by = "variable")%>%
  group_by(variable) %>%
  mutate(`P值` = case_when(
      row_number() == ceiling(n() / 2) ~ `P值`,
      TRUE ~ "")) %>%
  ungroup()

# 生成标准三线表
ft <- flextable(table_wide) %>%
  set_caption("表1 按高血压分组的人群基线特征（加权分析）") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline(border = fp_border(width = 0.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
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

# 1. 保存【未做宽表转换、未拼接文本的原始加权统计结果】到 output
# cat_table 循环得到的最原始的加权频数、百分比长格式数据框
saveRDS(cat_table, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/desc_raw.rds")

# 2. 各变量的卡方检验P值结果，方便正文提取P值描述组间差异
saveRDS(p_df, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/desc_pvalue.rds")

# 3. 成品格式化宽表备份
saveRDS(table_wide, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/desc_formatted_table.rds")

# 4. 顺带计算总加权样本量，存入全局统计结果方便QMD正文调用
total_wt <- sum(weights(design))
sample_summary <- data.frame(
  加权总样本量 = total_wt,
  原始有效样本量 = nrow(clean_data)
)
#  统计需要的全部样本信息
n_total <- nrow(clean_data)
n_htn <- sum(clean_data$hypertension == 1)
n_non_htn <- sum(clean_data$hypertension == 0)
n_smoking <- table(clean_data$smoking)
total_wt <- sum(weights(design))

# 改成列表存储，用英文名称，方便Rmd读取
sample_summary <- list(
  total_wt = total_wt,
  n_total = n_total,
  n_htn = n_htn,
  n_non_htn = n_non_htn,
  n_smoking = n_smoking
)

saveRDS(sample_summary, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/sample_summary.rds")

cat("✅ 基线分析原始数据已保存至 output 文件夹\n")

# ============================================================
# # 基础统计分析
# ============================================================

# 1. 加权交叉表
tab_weighted <- svytable(~smoking + hypertension, design)
tab_weighted <- as.data.frame(tab_weighted)
colnames(tab_weighted) <- c("smoking", "hypertension", "weighted_n")

tab_wide <- tab_weighted %>%
  pivot_wider(names_from = hypertension, values_from = weighted_n) %>%
  rename(非高血压 = `0`, 高血压 = `1`)

tab_wide$总人数 <- tab_wide$非高血压 + tab_wide$高血压
tab_wide$非高血压_n_pct <- sprintf("%.0f (%.1f)", tab_wide$非高血压, tab_wide$非高血压 / tab_wide$总人数 * 100)
tab_wide$高血压_n_pct <- sprintf("%.0f (%.1f)", tab_wide$高血压, tab_wide$高血压 / tab_wide$总人数 * 100)

# 2. 卡方检验
chisq_result <- svychisq(~smoking + hypertension, design)
chi_square <- chisq_result$statistic
p_value <- chisq_result$p.value

chi_formatted <- sprintf("%.3f", chi_square)
p_formatted <- ifelse(p_value < 0.001, "<0.001", round(p_value, 3))

# 3. 准备表格数据
table_data <- data.frame(
  吸烟状态 = tab_wide$smoking,
  总人数 = tab_wide$总人数,
  非高血压 = tab_wide$非高血压_n_pct,
  高血压 = tab_wide$高血压_n_pct,
  χ2 = c(chi_formatted, "", ""),
  p值 = c(p_formatted, "", ""))

ft <- flextable(table_data) %>%
  set_caption("表2 不同吸烟状态的高血压患病情况") %>%
  set_header_labels(
    吸烟状态 = "吸烟状态",
    总人数 = "总人数 (N)",
    非高血压 = "非高血压 n (%)",
    高血压 = "高血压 n (%)",
    χ2 = "χ²",
    p值 = "P 值") %>%
  align(align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline(border = fp_border(width = 0.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  autofit()
# 预览
print(ft)

# 导出Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "~/GitHub/JUMP-R-2026/students/panyitong/tables/吸烟高血压卡方检验结果表.docx")

# 1. 保存【未拼接格式化文本的原始加权频数数据】，用于QMD正文提取数值
saveRDS(tab_weighted, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/smoking_htn_cross_raw.rds")

# 2. 保存卡方检验原始结果（卡方值、原始P值，未做格式美化）
chisq_raw <- data.frame(
  chi_square = chisq_result$statistic,
  p_value = chisq_result$p.value
)
saveRDS(chisq_raw, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/smoking_chisq_raw.rds")

# 3. 备份格式化后的汇总表
saveRDS(table_data, file = "~/GitHub/JUMP-R-2026/students/panyitong/output/smoking_htn_table_formatted.rds")

cat("✅ 吸烟与高血压交叉表、卡方检验原始数据已保存至output文件夹\n")



