library(survey)
library(dplyr)
library(gtsummary)
library(flextable)
library(tidyr)   

# ========== 1. 正态性判断（加权图形 + 加权偏度/峰度） ==========
cont_vars <- c("URXUCD_cr", "RIDAGEYR", "BMXBMI", "INDFMPIR")
norm_decision <- data.frame(
  variable = cont_vars,
  skewness = NA,
  kurtosis = NA,
  decision = NA
)

# 设置图形布局
par(mfrow = c(2, length(cont_vars)), mar = c(3,3,2,1))

for (i in seq_along(cont_vars)) {
  v <- cont_vars[i]
  
  # 加权直方图
  svyhist(as.formula(paste0("~", v)), design = svy_design, 
          main = paste(v, "- 加权直方图"), col = "steelblue", xlab = "")
  
  # 加权 Q-Q 正态图
  p <- ppoints(200)
  quant_w <- svyquantile(as.formula(paste0("~", v)), design = svy_design,
                         quantiles = p, ci = FALSE)
  plot(qnorm(p), quant_w[[1]], type = "l", lwd = 2, col = "blue",
       xlab = "理论正态分位数", ylab = "加权分位数", main = paste(v, "- 加权Q-Q图"))
  abline(0, 1, col = "red", lty = 2)
  
  # 提取数据+权重，并过滤缺失值
  dat <- svy_design$variables
  w <- weights(svy_design)
  x <- dat[[v]]
  # 统一过滤变量、权重、分组缺失值
  valid <- !is.na(x) & !is.na(w) & !is.na(dat$hashimoto)
  x <- x[valid]
  w <- w[valid]
  w <- w / sum(w)  # 权重归一化
  
  # 计算加权均值、标准差、偏度、超额峰度
  mu <- sum(w * x)
  sigma <- sqrt(sum(w * (x - mu)^2))
  skew <- sum(w * ((x - mu) / sigma)^3)
  kurt <- sum(w * ((x - mu) / sigma)^4) - 3
  
  norm_decision$skewness[i] <- round(skew, 3)
  norm_decision$kurtosis[i] <- round(kurt, 3)
  
  # 判定规则：|偏度|>1 或 |超额峰度|>2 判定为偏态
  if (abs(skew) > 1 || abs(kurt) > 2) {
    norm_decision$decision[i] <- "偏态 -> 使用加权秩和检验"
  } else {
    norm_decision$decision[i] <- "近似正态 -> 使用加权t检验"
  }
}
par(mfrow = c(1,1))
print(norm_decision)

# =========== 2.按正态性结果执行组间比较 ==========
# 尿镉（偏态 → 加权秩和检验）
urine_test <- svyranktest(URXUCD_cr ~ hashimoto, design = svy_design)
urine_pvalue <- urine_test$p.value
cat("尿镉: 加权 Wilcoxon 秩和检验\n")
cat("P 值:", format(urine_pvalue, scientific = FALSE, digits = 4), "\n\n")

# 两组尿镉加权中位数及置信区间
urine_median <- svyby(~URXUCD_cr, ~hashimoto, svy_design, 
                      svyquantile, quantiles = 0.5, ci = TRUE)
print(urine_median)
cat("\n")

# 分类协变量：加权卡方检验
cat_vars <- c("sex", "race", "education", "smoke", "hypertension")
chisq_results <- lapply(cat_vars, function(var) {
  formula <- as.formula(paste("~", var, "+ hashimoto"))
  test <- svychisq(formula, design = svy_design, statistic = "F")
  data.frame(
    variable = var,
    p_value = test$p.value,  # 保留原始数值，不提前round
    statistic = round(test$statistic, 2),
    df = paste(test$parameter, collapse = ", ")
  )
})
chisq_table <- bind_rows(chisq_results)
cat("分类变量卡方检验结果：\n")
print(chisq_table)
cat("\n")

# 连续协变量检验
age_test <- svyttest(RIDAGEYR ~ hashimoto, design = svy_design)
cat("年龄: 加权 t 检验, P =", format(age_test$p.value, scientific = FALSE, digits = 4), "\n")

bmi_test <- svyranktest(BMXBMI ~ hashimoto, design = svy_design)
cat("BMI: 加权秩和检验, P =", format(bmi_test$p.value, scientific = FALSE, digits = 4), "\n")

pir_test <- svyttest(INDFMPIR ~ hashimoto, design = svy_design)
cat("贫困收入比: 加权 t 检验, P =", format(pir_test$p.value, scientific = FALSE, digits = 4), "\n\n")

# 连续变量结果汇总表
rank_cont_table <- data.frame(
  variable = c("RIDAGEYR", "BMXBMI", "INDFMPIR"),
  p_value = c(age_test$p.value, bmi_test$p.value, pir_test$p.value)
)
print(rank_cont_table)

# ==========3.加权基线特征表==========
# 提取原始P值（不提前四舍五入）
age_p   <- age_test$p.value
bmi_p   <- bmi_test$p.value
pir_p   <- pir_test$p.value
cad_p   <- urine_test$p.value

# 构建基础基线表
tbl_base <- tbl_svysummary(
  svy_design,
  by = hashimoto,
  include = c(RIDAGEYR, BMXBMI, INDFMPIR, URXUCD_cr,
              sex, race, education, smoke, hypertension),
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
  label = list(
    RIDAGEYR ~ "年龄 (岁)",
    BMXBMI ~ "身体质量指数 (kg/m²)",
    INDFMPIR ~ "贫困收入比",
    URXUCD_cr ~ "尿镉 (μg/g 肌酐)",
    sex ~ "性别",
    race ~ "种族",
    education ~ "教育程度",
    smoke ~ "吸烟状况",
    hypertension ~ "高血压"
  )
) %>%
  modify_header(all_stat_cols() ~ "**{level} (加权 n = {n})**") %>%
  modify_caption("表2. 研究人群加权基线特征及组间比较 (NHANES 2007-2012)") %>%
  bold_labels()

# 按变量名精准匹配原始P值
p_df <- data.frame(
  variable = c("RIDAGEYR", "BMXBMI", "INDFMPIR", "URXUCD_cr",
               "sex", "race", "education", "smoke", "hypertension"),
  p_val = c(age_p, bmi_p, pir_p, cad_p,
            chisq_table$p_value[chisq_table$variable == "sex"],
            chisq_table$p_value[chisq_table$variable == "race"],
            chisq_table$p_value[chisq_table$variable == "education"],
            chisq_table$p_value[chisq_table$variable == "smoke"],
            chisq_table$p_value[chisq_table$variable == "hypertension"])
)

tbl_body <- tbl_base$table_body
# 匹配P值
tbl_body$p.value <- p_df$p_val[match(tbl_body$variable, p_df$variable)]

# 删去无效的fmt代码
tbl_final <- tbl_base
tbl_final$table_body <- tbl_body

# ==========4.结果导出文件（优化P值格式+分类变量填充P值）==========
write.csv(chisq_table, "分类变量加权卡方结果.csv", row.names = FALSE)
write.csv(rank_cont_table, "连续变量加权检验结果.csv", row.names = FALSE)

# 提取数据框并填充分类变量子行P值
ft_data <- tbl_final$table_body %>%
  select(variable, var_label, label, stat_1, stat_2, p.value) %>%
  mutate(display_label = ifelse(is.na(label) | label == "", var_label, label)) %>%
  select(display_label, stat_1, stat_2, p.value)

# 向下填充P值（解决分类变量多行P值为空）
ft_data <- ft_data %>% fill(p.value, .direction = "down")

names(ft_data) <- c("变量", "桥本阴性", "桥本阳性", "P值")

# 统一格式化P值：<0.0001 / 保留4位小数
ft_data$`P值` <- sapply(ft_data$`P值`, function(x) {
  if (is.na(x)) return("NA")
  if (x < 0.0001) return("<0.0001")
  return(format(round(x, 4), scientific = FALSE))
})

# 创建 flextable 并美化
ft <- flextable(ft_data) %>%
  theme_box() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  autofit() %>%
  add_footer_lines("组间比较：P < 0.05 为差异有统计学意义。")

print(ft)
save_as_docx(ft, path = "Table2_mixed_tests.docx")

# ==========5.加权多因素 Logistic 回归（三个模型）===========

# 在svy_design内部数据框新增log10转换后的尿镉变量
svy_design$variables$log_URXUCD_cr <- log10(svy_design$variables$URXUCD_cr)
c("log_URXUCD_cr") %in% colnames(svy_design$variables)
# 模型1：粗模型（不调整）
m1 <- svyglm(hashimoto ~ log_URXUCD_cr, 
             design = svy_design, 
             family = quasibinomial)

# 模型2：调整人口学 + 社会经济变量（年龄、性别、种族、教育、贫困收入比）
m2 <- svyglm(hashimoto ~ log_URXUCD_cr + RIDAGEYR + sex + race + education + INDFMPIR, 
             design = svy_design, 
             family = quasibinomial)

# 模型3：完全调整模型（模型2 + 吸烟、高血压、BMI）
m3 <- svyglm(hashimoto ~ log_URXUCD_cr + RIDAGEYR + sex + race + education + INDFMPIR + 
               smoke + hypertension + BMXBMI, 
             design = svy_design, 
             family = quasibinomial)

# 提取结果（OR、95% CI、P值）
tidy_m1 <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == "log_URXUCD_cr")
tidy_m2 <- tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == "log_URXUCD_cr")
tidy_m3 <- tidy(m3, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == "log_URXUCD_cr")

# 合并成结果表
logistic_results <- bind_rows(
  mutate(tidy_m1, Model = "模型1 (粗模型)"),
  mutate(tidy_m2, Model = "模型2 (+人口学+社会经济)"),
  mutate(tidy_m3, Model = "模型3 (+生活方式/慢性病)")
) %>%
  select(Model, term, estimate, conf.low, conf.high, p.value) %>%
  rename(OR = estimate, CI_lower = conf.low, CI_upper = conf.high, P_value = p.value) %>%
  mutate(across(c(OR, CI_lower, CI_upper), ~ round(., 3)),
         P_value = round(P_value, 4))

# 导出 
logistic_results %>%
  flextable() %>%
  autofit() %>%
  print()

save_as_docx(logistic_results %>% flextable() %>% autofit(), 
             path = "Logistic_regression_table.docx")
