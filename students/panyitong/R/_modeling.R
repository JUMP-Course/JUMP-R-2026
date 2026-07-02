library(dplyr)
library(survey)
library(flextable)
library(officer)
library(broom)

# 1.主分析logistic回归
clean_data <- readRDS("clean_data.rds")


# 检查因变量（高血压）是否为数值型 
# 如果 hypertension 是字符型，转为数值型 0/1
if (is.character(clean_data$hypertension)) {
  clean_data$hypertension <- as.numeric(clean_data$hypertension)
}
# 如果是因子型，转为数值型
if (is.factor(clean_data$hypertension)) {
  clean_data$hypertension <- as.numeric(as.character(clean_data$hypertension))
}
# 验证
cat("\n高血压分布：\n")
print(table(clean_data$hypertension, useNA = "ifany"))


# 设置因子变量和参照组
clean_data$smoking <- factor(clean_data$smoking,
                             levels = c("Never", "Former", "Current"))

clean_data$age_group <- factor(clean_data$age_group,
                               levels = c("≤60", ">60"))

clean_data$gender <- factor(clean_data$gender,
                            levels = c("Female","Male"))

clean_data$race <- factor(clean_data$race)

clean_data$bmi_group <- factor(clean_data$bmi_group,
                               levels = c("Normal", "Underweight", "Overweight", "Obese"))

clean_data$education <- factor(clean_data$education,
                               levels = c("College or above","High school","Below high school"))




# 设置 survey 设计对象（加权）
design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE,
  data = clean_data)

# 粗模型：仅吸烟
model1 <- svyglm(
  hypertension ~ smoking,
  design = design,
  family = binomial())
summary(model1)

# 提取模型一结果
coef1 <- coef(summary(model1))[-1, ]  # 去掉截距
wald1 <- (coef1[, "Estimate"] / coef1[, "Std. Error"])^2
p1 <- pchisq(wald1, df = 1, lower.tail = FALSE)

OR1 <- exp(coef1[, "Estimate"])
LCL1 <- exp(coef1[, "Estimate"] - 1.96 * coef1[, "Std. Error"])
UCL1 <- exp(coef1[, "Estimate"] + 1.96 * coef1[, "Std. Error"])

results_model1 <- data.frame(
  变量 = rownames(coef1),
  OR = round(OR1, 3),
  CI = paste0("(", round(LCL1, 3), ", ", round(UCL1, 3), ")"),
  P值 = ifelse(p1 < 0.001, "<0.001", round(p1, 3))
)
print(results_model1)


# 模型二：全调整模型（未纳入教育和PIR，单因素分析不显著）
model2 <- svyglm(
  hypertension ~ smoking + age_group + gender + race + bmi_group ,
  design = design,
  family = binomial(link = "logit"))

coef2 <- coef(summary(model2))[-1, ]
wald2 <- (coef2[, "Estimate"] / coef2[, "Std. Error"])^2
p2 <- pchisq(wald2, df = 1, lower.tail = FALSE)

OR2 <- exp(coef2[, "Estimate"])
LCL2 <- exp(coef2[, "Estimate"] - 1.96 * coef2[, "Std. Error"])
UCL2 <- exp(coef2[, "Estimate"] + 1.96 * coef2[, "Std. Error"])

results_model2 <- data.frame(
  变量 = rownames(coef2),
  OR = round(OR2, 3),
  CI = paste0("(", round(LCL2, 3), ", ", round(UCL2, 3), ")"),
  P值 = ifelse(p2 < 0.001, "<0.001", round(p2, 3)))

# 只保留吸烟相关的结果
results_model2_smk <- results_model2[grep("smoking", results_model2$变量), ]
print(results_model2_smk)

model1_smk <- results_model1[grep("smoking", results_model1$变量), ]
model2_smk <- results_model2[grep("smoking", results_model2$变量), ]

model1_smk <- model1_smk[order(model1_smk$变量), ]  # Current 在前，Former 在后
model2_smk <- model2_smk[order(model2_smk$变量), ]

final_table <- data.frame(
  变量 = c("Current vs Never", "Former vs Never"),
  粗模型_OR = model1_smk$OR,
  粗模型_95CI = model1_smk$CI,
  粗模型_P = model1_smk$P值,
  调整模型_OR = model2_smk$OR,
  调整模型_95CI = model2_smk$CI,
  调整模型_P = model2_smk$P值)

print(final_table)


# 制作三线表
ft <- flextable(final_table) %>%
  set_caption("表3 吸烟与高血压关联的加权logistic回归分析") %>%
  add_header_row(values = c("", "粗模型", "", "", "调整模型", "", ""), top = TRUE) %>%
  merge_at(i = 1, j = 2:4, part = "header") %>%
  merge_at(i = 1, j = 5:7, part = "header") %>%
  set_header_labels(
    吸烟状态 = "吸烟状态",
    粗模型_OR = "OR",
    粗模型_95CI = "95% CI",
    粗模型_P = "P 值",
    调整模型_OR = "OR",
    调整模型_95CI = "95% CI",
    调整模型_P = "P 值"
  ) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "header") %>%
  hline(i = 1, j = 2:4, border = fp_border(width = 0.5), part = "header") %>%
  hline(i = 1, j = 5:7, border = fp_border(width = 0.5), part = "header") %>%
  hline(i = 2, part = "header", border = fp_border(width = 0.5)) %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body")

ft <- width(ft, j = 1, width = 2)
ft <- width(ft, j = 2:7, width = 1.2)

print(ft)

save_as_docx(ft, path = "~/GitHub/JUMP-R-2026/students/panyitong/tables/吸烟与高血压关联的加权logistic回归分析.docx")


# 2.敏感性分析：验证主分析结果稳健性

library(dplyr)
library(survey)
library(brglm2)

clean_data <- readRDS("clean_data.rds")


# 因子设置

clean_data$smoking <- factor(clean_data$smoking, levels = c("Never", "Former", "Current"))
clean_data$age_group <- factor(clean_data$age_group, levels = c("≤60", ">60"))
clean_data$gender <- factor(clean_data$gender, levels = c("Female", "Male"))
clean_data$race <- factor(clean_data$race)
clean_data$bmi_group <- factor(clean_data$bmi_group, 
                               levels = c("Normal", "Underweight", "Overweight", "Obese"))
clean_data$education <- factor(clean_data$education,
                               levels = c("College or above","High school","Below high school"))
clean_data$pir_group <- factor(clean_data$pir_group, levels = c("High", "Middle", "Low"))


# 创建 survey 设计

design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE,
  data = clean_data)


# 提取吸烟结果的函数（返回 OR、CI、P 值）

extract_smk <- function(model) {
  coef_data <- coef(summary(model))[-1, ]
  smk_idx <- grep("smoking", rownames(coef_data))
  coef_smk <- coef_data[smk_idx, ]
  
  wald <- (coef_smk[, "Estimate"] / coef_smk[, "Std. Error"])^2
  p <- pchisq(wald, df = 1, lower.tail = FALSE)
  
  OR <- exp(coef_smk[, "Estimate"])
  LCL <- exp(coef_smk[, "Estimate"] - 1.96 * coef_smk[, "Std. Error"])
  UCL <- exp(coef_smk[, "Estimate"] + 1.96 * coef_smk[, "Std. Error"])
  
  data.frame(
    变量 = rownames(coef_smk),
    OR = round(OR, 3),
    CI = paste0("(", round(LCL, 3), ", ", round(UCL, 3), ")"),
    P值 = ifelse(p < 0.001, "<0.001", round(p, 3)))
}

# 三个模型

cat("\n========== 模型1：主分析（无教育，无PIR）==========\n")
model_main <- svyglm(
  hypertension ~ smoking + age_group + gender + race + bmi_group ,
  design = design,
  family = binomial(link = "logit")
)
res_main <- extract_smk(model_main)
print(res_main)

cat("\n========== 模型2：主分析 + PIR ==========\n")
model_pir <- svyglm(
  hypertension ~ smoking + age_group + gender + race + bmi_group  + pir_group,
  design = design,
  family = binomial(link = "logit")
)
res_pir <- extract_smk(model_pir)
print(res_pir)

cat("\n========== 模型3：主分析 + 教育 ==========\n")
model_noedu <- svyglm(
  hypertension ~ smoking + age_group + gender + race + bmi_group + education,
  design = design,
  family = binomial(link = "logit")
)
res_noedu <- extract_smk(model_noedu)
print(res_noedu)


# 合并结果对比表

comparison <- data.frame(
  变量 = res_main$变量,
  主分析_OR = res_main$OR,
  主分析_CI = res_main$CI,
  主分析_P = res_main$P值,
  加PIR_OR = res_pir$OR,
  加PIR_CI = res_pir$CI,
  加PIR_P = res_pir$P值,
  加教育_OR = res_noedu$OR,
  加教育_CI = res_noedu$CI,
  加教育_P = res_noedu$P值
)

cat("\n========== 敏感性分析对比表 ==========\n")
print(comparison)

#做敏感性分析三线表

# 数据
sensitivity_table <- data.frame(
  模型设定 = c("主分析", "主分析+PIR", "主分析+教育"),
  Former_OR = c("1.416", "1.410", "1.397"),
  Former_CI = c("(1.184, 1.694)", "(1.180, 1.684)", "(1.170, 1.668)"),
  Former_P = c("<0.001", "<0.001", "<0.001"),
  Current_OR = c("1.286", "1.252", "1.234"),
  Current_CI = c("(1.007, 1.642)", "(0.981, 1.598)", "(0.975, 1.563)"),
  Current_P = c("0.044", "0.071", "0.081")
)

ft <- flextable(sensitivity_table) %>%
  set_caption("表4 吸烟与高血压关联的敏感性分析") %>%
  set_header_labels(
    模型设定 = "模型设定",
    Former_OR = "OR",
    Former_CI = "95% CI",
    Former_P = "P 值",
    Current_OR = "OR",
    Current_CI = "95% CI",
    Current_P = "P 值"
  ) %>%
  add_header_row(
    top = TRUE,
    values = c("", "Former vs Never", "Former vs Never", "Former vs Never",
               "Current vs Never", "Current vs Never", "Current vs Never")
  ) %>%
  merge_at(i = 1, j = 2:4, part = "header") %>%
  merge_at(i = 1, j = 5:7, part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:7, align = "center", part = "all") %>%
  font(j = 1, fontname = "SimSun", part = "all") %>%
  font(j = 2:7, fontname = "Times New Roman", part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  # 表格最顶部：通栏1.5磅粗线
  hline_top(border = fp_border(width = 1.5), part = "all") %>%
  # 一级分组表头下方：仅在2-4列、5-7列画0.5磅细线，第1列不画线（你要的：线只在两级标题重合区域）
  hline(i = 1, j = 2:4, border = fp_border(width = 0.5), part = "header") %>%
  hline(i = 1, j = 5:7, border = fp_border(width = 0.5), part = "header") %>%
  # 二级表头下方：整表通栏0.5磅细线，分隔表头与数据
  hline(i = 2, border = fp_border(width = 0.5), part = "header") %>%
  # 表格最底部：通栏1.5磅粗线
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  padding(padding = 6, part = "all") %>%
  autofit()

print(ft)

# 导出到 Word
save_as_docx(ft, path = "~/GitHub/JUMP-R-2026/students/panyitong/tables/吸烟与高血压关联的敏感性分析.docx")




# stratification_analysis.R
# 分层多因素分析
library(dplyr)
library(survey)
library(flextable)

clean_data <- readRDS("clean_data.rds")

# 确保变量为因子
clean_data$smoking <- factor(clean_data$smoking, levels = c("Never", "Former", "Current"))
clean_data$gender <- factor(clean_data$gender, levels = c("Female", "Male"))
clean_data$age_group <- factor(clean_data$age_group, levels = c("≤60", ">60"))
clean_data$race <- factor(clean_data$race)
clean_data$bmi_group <- factor(clean_data$bmi_group, 
                               levels = c("Normal", "Underweight", "Overweight", "Obese"))

# ============================================================
# 提取吸烟结果的函数
# ============================================================
extract_smk <- function(model) {
  coef_data <- coef(summary(model))
  coef_data <- coef_data[-1, , drop = FALSE]
  smk_idx <- grep("smokingFormer|smokingCurrent", rownames(coef_data))
  if(length(smk_idx) == 0) {
    return(data.frame(变量 = character(), OR = numeric(), CI = character(), P值 = character()))
  }
  coef_smk <- coef_data[smk_idx, , drop = FALSE]
  
  OR <- exp(coef_smk[, "Estimate"])
  LCL <- exp(coef_smk[, "Estimate"] - 1.96 * coef_smk[, "Std. Error"])
  UCL <- exp(coef_smk[, "Estimate"] + 1.96 * coef_smk[, "Std. Error"])
  P <- coef_smk[, "Pr(>|t|)"]
  
  data.frame(
    变量 = rownames(coef_smk),
    OR = round(OR, 3),
    CI = paste0("(", round(LCL, 3), ", ", round(UCL, 3), ")"),
    P值 = ifelse(P < 0.001, "<0.001", round(P, 3))
  )
}

# 设置全数据 survey 设计
design_full <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE,
  data = clean_data
)

# ============================================================
# 1. 性别分层
# ============================================================
cat("\n========== 性别分层 ==========\n")
gender_groups <- c("Male", "Female")
res_gender <- list()

for(g in gender_groups) {
  data_sub <- clean_data %>% filter(gender == g)
  design_sub <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC2YR,
    nest = TRUE,
    data = data_sub
  )
  model_sub <- svyglm(
    hypertension ~ smoking + age_group + race + bmi_group,
    design = design_sub,
    family = binomial()
  )
  res_gender[[g]] <- extract_smk(model_sub)
  cat("\n", g, "组：\n")
  print(res_gender[[g]])
}

# 交互：smoking × gender（分项P用于正文描述，全局Wald P放入三线表）
cat("\n========== 交互：吸烟 × 性别 ==========\n")
model_int_gender <- svyglm(
  hypertension ~ smoking * gender + age_group + race + bmi_group,
  design = design_full,
  family = binomial()
)
# 提取两个分项交互P（Former、Current，论文正文详细解读用）
coef_int <- coef(summary(model_int_gender))
int_idx <- grep("smoking.*gender", rownames(coef_int))
int_p_gender_vec <- if(length(int_idx) > 0) coef_int[int_idx, 4] else NA
cat("性别分层两个交互项P值（Former、Current）：", int_p_gender_vec, "\n")
# 全局Wald检验，仅得到1个整体交互P（放入三线表，解决长度报错）
wald_gender <- regTermTest(model_int_gender, ~smoking:gender)
int_p_gender_global <- wald_gender$p[1]

# ============================================================
# 2. 年龄分层
# ============================================================
cat("\n========== 年龄分层 ==========\n")
age_groups <- c("≤60", ">60")
res_age <- list()

for(ag in age_groups) {
  data_sub <- clean_data %>% filter(age_group == ag)
  design_sub <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC2YR,
    nest = TRUE,
    data = data_sub
  )
  model_sub <- svyglm(
    hypertension ~ smoking + gender + race + bmi_group,
    design = design_sub,
    family = binomial()
  )
  res_age[[ag]] <- extract_smk(model_sub)
  cat("\n", ag, "组：\n")
  print(res_age[[ag]])
}

# 交互：smoking × age
cat("\n========== 交互：吸烟 × 年龄 ==========\n")
model_int_age <- svyglm(
  hypertension ~ smoking * age_group + gender + race + bmi_group,
  design = design_full,
  family = binomial()
)
# 提取两个分项交互P
coef_int_age <- coef(summary(model_int_age))
int_age_idx <- grep("smoking.*age", rownames(coef_int_age))
int_p_age_vec <- if(length(int_age_idx) > 0) coef_int_age[int_age_idx, 4] else NA
cat("年龄分层两个交互项P值（Former、Current）：", int_p_age_vec, "\n")
# 全局Wald整体交互P
wald_age <- regTermTest(model_int_age, ~smoking:age_group)
int_p_age_global <- wald_age$p[1]

# ============================================================
# 3. 构建三线表数据框（自动提取）
# ============================================================
# 格式化全局交互P，固定4个元素，彻底解决长度不匹配报错
p_gender <- ifelse(is.na(int_p_gender_global), "",
                   ifelse(int_p_gender_global < 0.001, "<0.001", as.character(round(int_p_gender_global, 3))))
p_age <- ifelse(is.na(int_p_age_global), "",
                ifelse(int_p_age_global < 0.001, "<0.001", as.character(round(int_p_age_global, 3))))

stratification_data <- data.frame(
  分层变量 = c("性别", "", "年龄", ""),
  亚组 = c("Male", "Female", "≤60", ">60"),
  Former_OR = c(res_gender$Male$OR[1], res_gender$Female$OR[1],
                res_age$`≤60`$OR[1], res_age$`>60`$OR[1]),
  Former_CI = c(res_gender$Male$CI[1], res_gender$Female$CI[1],
                res_age$`≤60`$CI[1], res_age$`>60`$CI[1]),
  Former_P = c(res_gender$Male$P值[1], res_gender$Female$P值[1],
               res_age$`≤60`$P值[1], res_age$`>60`$P值[1]),
  Current_OR = c(res_gender$Male$OR[2], res_gender$Female$OR[2],
                 res_age$`≤60`$OR[2], res_age$`>60`$OR[2]),
  Current_CI = c(res_gender$Male$CI[2], res_gender$Female$CI[2],
                 res_age$`≤60`$CI[2], res_age$`>60`$CI[2]),
  Current_P = c(res_gender$Male$P值[2], res_gender$Female$P值[2],
                res_age$`≤60`$P值[2], res_age$`>60`$P值[2]),
  交互_P = c(p_gender, "", p_age, "")
)

# ============================================================
# 4. 生成三线表（修复管道断裂、重复列名错误）
# ============================================================
ft <- flextable(stratification_data) %>%
  set_caption("表5 吸烟与高血压关联的分层分析") %>%
  set_header_labels(
    分层变量 = "分层变量",
    亚组 = "亚组",
    Former_OR = "OR",
    Former_CI = "95% CI",
    Former_P = "P 值",
    Current_OR = "OR",
    Current_CI = "95% CI",
    Current_P = "P 值",
    交互_P = "交互 P 值"
  ) %>%
  add_header_row(top = TRUE,
                 values = c("", "", "Former vs Never", "Former vs Never", "Former vs Never",
                            "Current vs Never", "Current vs Never", "Current vs Never", "")) %>%
  merge_at(i = 1, j = 3:5, part = "header") %>%
  merge_at(i = 1, j = 6:8, part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:9, align = "center", part = "all") %>%
  font(j = 1:2, fontname = "SimSun") %>%
  font(j = 3:9, fontname = "Times New Roman") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5), part = "all") %>%
  hline(i = 1, j = 3:5, border = fp_border(width = 0.5), part = "header") %>%
  hline(i = 1, j = 6:8, border = fp_border(width = 0.5), part = "header") %>%
  hline(i = 2, border = fp_border(width = 0.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
  padding(padding = 8, part = "all") %>%
  autofit()

ft <- ft %>%
  width(j = 1, width = 1.2) %>%  # 加宽第1列
  # 设置所有单元格文字垂直居中
  valign(valign = "center", part = "all") %>%
  # 禁止单元格文字自动换行
  set_table_properties(layout = "fixed")

# 打印预览
print(ft)

# 导出到 Word
save_as_docx(ft, path = "~/GitHub/JUMP-R-2026/students/panyitong/tables/吸烟与高血压关联的分层分析.docx")


