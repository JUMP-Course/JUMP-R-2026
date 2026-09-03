# ==============================================
# 亚组分析（年龄分组 + BMI分组）
# ==============================================

# ---------- 0. 环境初始化 ----------
if (!require("dplyr")) install.packages("dplyr")
if (!require("flextable")) install.packages("flextable")

library(dplyr)
library(flextable)

setwd("D:/ukb-jump/gdm-chd-ukb")
df_clean <- readRDS("./data/ukb_cleaned_analysis_final.rds")


# ---------- 1. 定义亚组变量 + 工具函数 ----------
subgroup_list <- list(
  "年龄分组" = "age_group",
  "BMI分组" = "bmi_group"
)

# 工具函数：从回归模型中稳定提取GDM的OR、95%CI、P值
get_gdm_result <- function(model) {
  coef_tab <- summary(model)$coefficients
  # 模糊匹配GDM对应的行，适配不同因子命名规则
  gdm_row <- grep("gdm", rownames(coef_tab), ignore.case = TRUE)[1]
  
  beta  <- coef_tab[gdm_row, "Estimate"]
  se    <- coef_tab[gdm_row, "Std. Error"]
  p_val <- coef_tab[gdm_row, "Pr(>|z|)"]
  
  # 正态近似计算95%CI，纯算术计算，永远不会报错
  or_val  <- exp(beta)
  ci_low  <- exp(beta - 1.96 * se)
  ci_high <- exp(beta + 1.96 * se)
  
  return(c(OR = round(or_val,2), 
           CI_low = round(ci_low,2), 
           CI_high = round(ci_high,2), 
           P = p_val))
}

# 初始化结果汇总表
subgroup_result <- data.frame()


# ---------- 2. 分层Logistic回归核心计算 ----------
for (sub_name in names(subgroup_list)) {
  sub_var <- subgroup_list[[sub_name]]
  sub_levels <- levels(df_clean[[sub_var]])
  
  for (level in sub_levels) {
    sub_data <- df_clean %>% filter(.data[[sub_var]] == level)
    
    # 校验1：冠心病病例<5例直接跳过，避免结果失真
    n_case <- table(sub_data$chd)[["发生冠心病"]]
    if (n_case < 5) next
    
    # 校验2：所有因子协变量水平数≥2，避免对比报错
    factor_covars <- c("smoking", "alcohol", "education")
    level_ok <- TRUE
    for (cov in factor_covars) {
      if (nlevels(droplevels(sub_data[[cov]])) < 2) {
        level_ok <- FALSE
        break
      }
    }
    if (!level_ok) next
    
    # 拟合全校正Logistic回归（和主分析模型完全一致，保证可比）
    mod_sub <- glm(
      chd ~ gdm + age + bmi + smoking + alcohol + education,
      data = sub_data, family = binomial
    )
    
    # 提取GDM的效应值
    res <- get_gdm_result(mod_sub)
    
    # 合并到总结果表
    subgroup_result <- rbind(
      subgroup_result,
      data.frame(
        亚组类型 = sub_name,
        亚组类别 = level,
        OR值 = res["OR"],
        CI95下限 = res["CI_low"],
        CI95上限 = res["CI_high"],
        P值 = ifelse(res["P"] < 0.001, "<0.001", round(res["P"], 3))
      )
    )
  }
}

cat("===== 亚组分析结果 =====\n")
print(subgroup_result)


# ---------- 3. 导出独立Word表格 ----------
subgroup_result %>%
  flextable() %>%
  set_caption("表4 不同亚组中GDM与冠心病发病的关联") %>%
  autofit() %>%
  save_as_docx(path = "./output/表4_年龄BMI亚组分析结果.docx")

cat("\n✅ 亚组分析完成，Word表格已导出\n")


# ---------- 4. 交互作用检验（似然比检验，输出整体P值）----------
cat("\n===== 交互作用检验结果 =====\n")

inter_result <- data.frame()

for (sub_name in names(subgroup_list)) {
  sub_var <- subgroup_list[[sub_name]]
  
  # 模型A：仅主效应（简化模型，无交互项）
  formula_reduced <- paste0("chd ~ gdm + ", sub_var, " + age + bmi + smoking + alcohol + education")
  mod_reduced <- glm(as.formula(formula_reduced), data = df_clean, family = binomial)
  
  # 模型B：主效应 + 交互项（全模型）
  formula_full <- paste0("chd ~ gdm * ", sub_var, " + age + bmi + smoking + alcohol + education")
  mod_full <- glm(as.formula(formula_full), data = df_clean, family = binomial)
  
  # 似然比检验：比较两个模型的拟合差异，得到整体交互作用的P值
  lrt <- anova(mod_reduced, mod_full, test = "Chisq")
  inter_p <- lrt$`Pr(>Chi)`[2]  # 第二行是模型差异的检验结果
  
  # 汇总结果
  inter_result <- rbind(
    inter_result,
    data.frame(
      亚组变量 = sub_name,
      交互整体P值 = ifelse(inter_p < 0.001, "<0.001", round(inter_p, 3))
    )
  )
}

print(inter_result)
cat("\n✅ 交互作用检验完成\n")
# 敏感性分析：不同协变量集下GDM的效应
mod_sens1 <- glm(chd ~ gdm + age, data = df_clean, family = binomial)
mod_sens2 <- glm(chd ~ gdm + age + bmi, data = df_clean, family = binomial)
mod_sens3 <- glm(chd ~ gdm + age + bmi + smoking + alcohol + education, data = df_clean, family = binomial)

tbl_sens <- tbl_merge(
  tbls = list(
    tbl_regression(mod_sens1, exponentiate = TRUE, include = gdm, label = list(gdm ~ "妊娠期糖尿病病史")),
    tbl_regression(mod_sens2, exponentiate = TRUE, include = gdm, label = list(gdm ~ "妊娠期糖尿病病史")),
    tbl_regression(mod_sens3, exponentiate = TRUE, include = gdm, label = list(gdm ~ "妊娠期糖尿病病史"))
  ),
  tab_spanner = c("**校正年龄**", "**+BMI**", "**全校正**")
) %>%
  modify_caption("敏感性分析：不同协变量集下GDM的效应")

print(tbl_sens)
