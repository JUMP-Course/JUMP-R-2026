install.packages("broom")

library(broom)
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
  
  # 提取数据+权重
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

# 两组尿镉加权中位数及标准误
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
    p_value = test$p.value,  
    statistic = round(test$statistic, 2),
    df = paste(test$parameter, collapse = ", ")
  )
})
chisq_table <- bind_rows(chisq_results)
cat("分类变量卡方检验结果：\n")
chisq_table %>%
  mutate(p_value = format(p_value, scientific = FALSE, digits = 4)) %>%
  print()
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
# 提取原始P值
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

tbl_final <- tbl_base
tbl_final$table_body <- tbl_body

# ==========4.结果导出文件==========
write.csv(chisq_table, "分类变量加权卡方结果.csv", row.names = FALSE)
write.csv(rank_cont_table, "连续变量加权检验结果.csv", row.names = FALSE)

# 提取数据框
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

# ==========6. 剂量反应分析：四分位数分类比较 + 线性趋势检验==========

# 加权四分位数分组 
cat("\n===== 计算尿镉加权四分位数切点 =====\n")
quant_obj <- svyquantile(~URXUCD_cr, design = svy_design, 
                         quantiles = c(0, 0.25, 0.5, 0.75, 1), 
                         na.rm = TRUE)
quantiles <- as.numeric(quant_obj$URXUCD_cr[,1])
cat("四分位数切点值：", quantiles, "\n")

# 创建四分位数变量（Q1为参照）
svy_design$variables$urine_q <- cut(
  svy_design$variables$URXUCD_cr,
  breaks = quantiles,
  include.lowest = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4")
)

cat("\n各四分位原始样本人数：\n")
print(table(svy_design$variables$urine_q))
cat("\n各四分位加权样本人数：\n")
print(svytotal(~urine_q, svy_design))

# 完全调整模型公式（模型3全部协变量）
formula_q <- as.formula(
  hashimoto ~ urine_q + RIDAGEYR + sex + race + education + 
    INDFMPIR + smoke + hypertension + BMXBMI
)

# 四分位数模型
fit_q <- svyglm(formula_q, design = svy_design, family = quasibinomial)
tidy_q <- tidy(fit_q, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("urine_q", term))
cat("\n四分位数分组OR及95%置信区间：\n")
print(tidy_q)

# 线性趋势检验（等级作为连续变量）
svy_design$variables$urine_q_num <- as.numeric(svy_design$variables$urine_q)
formula_trend <- update(formula_q, . ~ . - urine_q + urine_q_num)
fit_trend <- svyglm(formula_trend, design = svy_design, family = quasibinomial)
tidy_trend <- tidy(fit_trend, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "urine_q_num")
print(tidy_trend)

# 合并导出剂量-反应汇总表 
q_results <- tidy_q %>%
  mutate(
    OR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    P = round(p.value, 4),
    Exposure = case_when(
      term == "urine_qQ2" ~ "Q2",
      term == "urine_qQ3" ~ "Q3",
      term == "urine_qQ4" ~ "Q4",
      TRUE ~ term
    )
  ) %>%
  select(Exposure, OR_CI, P)

trend_line <- data.frame(
  Exposure = "线性趋势（每升高1个四分位等级）",
  OR_CI = paste0(
    round(tidy_trend$estimate, 2), " (",
    round(tidy_trend$conf.low, 2), "-",
    round(tidy_trend$conf.high, 2), ")"
  ),
  P = round(tidy_trend$p.value, 4)
)

dose_response_table <- bind_rows(q_results, trend_line)
cat("\n===== 剂量-反应分析汇总表 =====\n")
print(dose_response_table)
write.csv(dose_response_table, "1_尿镉剂量反应分析结果.csv", row.names = FALSE, fileEncoding = "UTF-8")

#========== 7.亚组分层分析（年龄、性别）==========

# 创建年龄分组
svy_design$variables$age_group <- ifelse(svy_design$variables$RIDAGEYR < 45, "<45岁", "≥45岁")
svy_design$variables$age_group <- factor(svy_design$variables$age_group, 
                                         levels = c("<45岁", "≥45岁"))

# 定义函数：根据子集动态构建公式（剔除单水平因子）
build_formula <- function(sub_ds) {
  # 基础暴露变量
  base_vars <- c("urine_q_num")
  # 待检查的协变量（数值变量直接保留，因子需检查水平数）
  candidates <- c("RIDAGEYR", "sex", "race", "education", "INDFMPIR", "smoke", "hypertension", "BMXBMI")
  keep_vars <- base_vars  # 先放入urine_q
  
  for (v in candidates) {
    if (v %in% names(sub_ds$variables)) {
      val <- sub_ds$variables[[v]]
      # 如果是因子或字符型，检查水平数（去重后>1）
      if (is.factor(val) || is.character(val)) {
        if (length(unique(na.omit(val))) > 1) {
          keep_vars <- c(keep_vars, v)
        }
      } else {
        # 数值型直接保留（如年龄、PIR、BMI）
        keep_vars <- c(keep_vars, v)
      }
    }
  }
  # 组装公式字符串：结局 ~ 变量1 + 变量2 + ...
  formula_str <- paste("hashimoto ~", paste(keep_vars, collapse = " + "))
  return(as.formula(formula_str))
}

#===== 年龄分层=====
age_levels <- levels(svy_design$variables$age_group)
age_list_cont <- list()

for (g in age_levels) {
  sub_ds <- subset(svy_design, age_group == g)
  sub_ds <- subset(sub_ds,
                   !is.na(hashimoto) & !is.na(RIDAGEYR) & !is.na(race) & !is.na(education) &
                     !is.na(INDFMPIR) & !is.na(smoke) & !is.na(hypertension) & !is.na(BMXBMI))
  
  # 动态构建公式（使用 urine_q_num 连续等级，而不是 Q4 vs Q1）
  formula_cont <- build_formula(sub_ds)
  
  fit <- tryCatch(
    svyglm(formula_cont, design = sub_ds, family = quasibinomial),
    error = function(e) {
      cat("在年龄", g, "中连续模型失败：", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(fit)) next
  # 提取尿镉四分位结果  
  tidy_fit <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "urine_q_num") %>%
    mutate(Subgroup = paste0("年龄：", g),
           OR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
           P = round(p.value, 4))
  age_list_cont[[g]] <- tidy_fit
}

age_df_cont <- bind_rows(age_list_cont)
cat("\n===== 年龄分层（连续 IQR 等级） =====\n")
print(age_df_cont)

# ===== 性别分层=====
sex_levels <- levels(svy_design$variables$sex)
sex_list <- list()

for (g in sex_levels) {
  sub_ds <- subset(svy_design, sex == g)
  # 过滤所有协变量缺失
  sub_ds <- subset(sub_ds,
                   !is.na(hashimoto) & !is.na(RIDAGEYR) & !is.na(race) & !is.na(education) &
                     !is.na(INDFMPIR) & !is.na(smoke) & !is.na(hypertension) & !is.na(BMXBMI))
  # 检查样本量
  if (nrow(sub_ds) < 30) {
    cat("性别", g, "样本量过小（", nrow(sub_ds), "），跳过\n")
    next
  }
  
  # 手动构建公式（只包含核心暴露 + 协变量，但自动检查单水平）
  # 先检查每个协变量在亚组里是否至少有2个水平
  cox_vars <- c("RIDAGEYR", "race", "education", "INDFMPIR", "smoke", "hypertension", "BMXBMI")
  keep_vars <- "urine_q_num"  # 核心暴露始终保留
  
  for (v in cox_vars) {
    if (v %in% names(sub_ds$variables)) {
      val <- sub_ds$variables[[v]]
      # 数值变量直接保留，分类变量检查水平数
      if (is.numeric(val) || is.integer(val)) {
        keep_vars <- c(keep_vars, v)
      } else if (is.factor(val) || is.character(val)) {
        if (length(unique(na.omit(val))) > 1) {
          keep_vars <- c(keep_vars, v)
        }
      }
    }
  }
  # 拼成公式
  formula_str <- paste("hashimoto ~", paste(keep_vars, collapse = " + "))
  formula_sub <- as.formula(formula_str)
  # 跑模型
  fit <- tryCatch(
    svyglm(formula_sub, design = sub_ds, family = quasibinomial),
    error = function(e) {
      cat("在性别", g, "中模型失败：", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(fit)) next
  # 提取结果
  tidy_fit <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "urine_q_num") %>%
    mutate(Subgroup = paste0("性别：", g),
           OR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
           P = round(p.value, 4))
  sex_list[[g]] <- tidy_fit
}
 # 合并结果
sex_df <- bind_rows(sex_list)
cat("\n===== 性别分层结果（连续 IQR 等级） =====\n")
print(sex_df)

# 合并导出
subgroup_results <- bind_rows(
  sex_df %>% mutate(Subgroup = as.character(Subgroup)),
  age_df_cont %>% mutate(Subgroup = as.character(Subgroup))
) %>% 
  mutate(
    OR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    P = round(p.value, 4),
    Exposure = case_when(
      term == "urine_qQ2" ~ "Q2",
      term == "urine_qQ3" ~ "Q3",
      term == "urine_qQ4" ~ "Q4",
      TRUE ~ term
    )
  ) %>% 
  select(Subgroup, Exposure, OR_CI, P)

cat("\n===== 亚组分析汇总表（45岁切点，动态剔除单水平因子） =====\n")
print(subgroup_results)

write.csv(subgroup_results, "2_亚组分层分析结果_45岁切点.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("亚组结果已保存为：2_亚组分层分析结果_45岁切点.csv\n")

library(survey)
library(broom)
library(dplyr)
library(stringr)

# ==========8.交互作用检验==========
# 新增交互变量
svy_design_inter <- update(svy_design,
                           age_group_num = ifelse(age_group == "≥45岁", 1, 0),  # 1=≥45岁，0=<45岁
                           sex_num = ifelse(sex == "女", 1, 0)                  # 1=女性，0=男性
)

# =====年龄 × 尿镉交互模型=====
interaction_age <- svyglm(
  hashimoto ~ urine_q_num * age_group_num +     
    sex + race + education + INDFMPIR +         
    smoke + hypertension + BMXBMI,
  design = svy_design_inter,
  family = quasibinomial
)

# 提取年龄交互P值
tidy_age_interaction <- tidy(interaction_age, conf.int = TRUE, exponentiate = TRUE)
age_inter_p_df <- tidy_age_interaction %>%
  filter(str_detect(term, "urine_q_num:age_group_num"))
if(nrow(age_inter_p_df) == 0) stop("未匹配到年龄交互项，请检查变量名！")
age_interaction_p <- pull(age_inter_p_df, p.value)
if(is.na(age_interaction_p)) warning("年龄交互项P值为NA，请检查模型收敛与样本量！")

# ===== 性别 × 尿镉交互模型 =====
interaction_sex <- svyglm(
  hashimoto ~ urine_q_num * sex_num +           
    RIDAGEYR + race + education + INDFMPIR +    
    smoke + hypertension + BMXBMI,
  design = svy_design_inter,
  family = quasibinomial
)

# 提取性别交互P值
tidy_sex_interaction <- tidy(interaction_sex, conf.int = TRUE, exponentiate = TRUE)
sex_inter_p_df <- tidy_sex_interaction %>%
  filter(str_detect(term, "urine_q_num:sex_num"))
if(nrow(sex_inter_p_df) == 0) stop("未匹配到性别交互项，请检查变量名！")
sex_interaction_p <- pull(sex_inter_p_df, p.value)
if(is.na(sex_interaction_p)) warning("性别交互项P值为NA，请检查模型收敛与样本量！")

# 自定义P值格式化函数
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  return(sprintf("%.4f", p))
}

cat("\n===== 交互作用 P 值 =====\n")
cat("年龄 × 尿镉: P =", format_p(age_interaction_p), "\n")
cat("性别 × 尿镉: P =", format_p(sex_interaction_p), "\n")

# 汇总交互结果
interaction_results <- data.frame(
  Interaction = c("年龄 × 尿镉", "性别 × 尿镉"),
  P_value_raw = c(age_interaction_p, sex_interaction_p),
  P_formatted = c(format_p(age_interaction_p), format_p(sex_interaction_p))
)

print(interaction_results)
write.csv(interaction_results, "交互作用检验结果.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 核对模型系数，排查共线性、奇异值
cat("\n===== 年龄交互模型全部系数 =====\n")
print(tidy_age_interaction)
cat("\n===== 性别交互模型全部系数 =====\n")
print(tidy_sex_interaction)

# ========== 9.亚组森林图（尿镉效应） ==========
library(ggplot2)
library(stringr) 

# 定义P值格式化函数
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  return(sprintf("%.4f", p))
}

# 合并数据 
all_sub_res <- bind_rows(
  sex_df,
  age_df_cont
) %>%
  rename(
    estimate = estimate,
    conf.low = conf.low,
    conf.high = conf.high
  )

#  准备绘图数据
fforest_input <- all_sub_res %>%
  rename(
    Subgroup = Subgroup,
    OR = estimate,
    CI_low = conf.low,
    CI_high = conf.high
  ) %>%
  mutate(
    Group = ifelse(str_detect(Subgroup, "性别"), "性别", "年龄"),
    Subgroup = case_when(
      Subgroup == "≥45岁" ~ "年龄：≥45岁",
      Subgroup == "<45岁" ~ "年龄：<45岁",
      TRUE ~ Subgroup
    ),
    Subgroup = factor(Subgroup,
                      levels = c("性别：女", "性别：男",
                                 "年龄：<45岁", "年龄：≥45岁")),
    p_label = case_when(
      p.value < 0.001 ~ "P < 0.001",
      p.value < 0.01  ~ paste0("P = ", round(p.value, 3)),
      TRUE            ~ paste0("P = ", round(p.value, 2))
    ),
    label_text = paste0(
      round(OR, 2), " (", round(CI_low, 2), "-", round(CI_high, 2), ")\n",
      p_label
    )
  )

# 绘制森林图
p <- ggplot(fforest_input, aes(x = OR, y = Subgroup, color = Group)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 1) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), 
                width = 0.2, 
                orientation = "y",
                linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = label_text, x = CI_high), 
            hjust = -0.3, size = 4, show.legend = FALSE) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4),
                labels = c("0.25", "0.5", "1", "2", "4"),
                limits = c(0.2, 5.5)) +  # 扩大范围
  scale_color_manual(values = c("steelblue", "#e63946")) +
  labs(
    title = "尿镉与桥本甲状腺炎风险的亚组分析",
    subtitle = "分层维度：性别、年龄（45岁切点）",
    x = "比值比 OR (95% 置信区间)", 
    y = NULL,
    color = "亚组类型",
    # ===== 关键修改：添加交互P值脚注 =====
    caption = paste0(
      "交互作用 P 值：年龄×尿镉 = ", format_p(age_interaction_p),
      "；性别×尿镉 = ", format_p(sex_interaction_p)
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),  # 脚注样式
    legend.position = "bottom",
    axis.text.y = element_text(size = 13),
    plot.margin = margin(10, 150, 20, 10)  # 右侧和底部留更多空间
  )

print(p)
ggsave("forest_subgroup_cd_HT.tiff", p, width = 10, height = 5, dpi = 600)