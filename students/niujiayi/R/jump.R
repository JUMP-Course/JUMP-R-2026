# 研究主题：成年女性饮酒频次与乳腺癌患病的相关性分析
# 数据来源：NHANES 2017-2020 美国全国健康与营养检查调查

# install.packages(c("tidyverse", "haven", "gtsummary", "flextable", 
#                    "scales", "broom", "broom.helpers", "car", "ResourceSelection", "pROC"))
library(tidyverse)       
library(haven)           
library(gtsummary)      
library(scales)          
library(flextable)       
library(broom)          
library(car)             
library(ResourceSelection) 
library(pROC)       
# ========== 1. 读取合并数据 ==========
demo <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_DEMO.xpt")
alq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_ALQ.xpt")
mcq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_MCQ.xpt")
bmx  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_BMX.xpt")
smq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_SMQ.xpt")

all_data <- demo %>%
  left_join(alq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(smq, by = "SEQN")

cat("① 原始总样本量：", nrow(all_data), "人\n")

# ========== 2. 筛选研究人群 ==========
female <- all_data %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 18)
cat("② 筛选成年女性后样本量：", nrow(female), "人\n")

# ========== 3. 构建结局变量==========
female <- female %>%
  mutate(
    MCQ220 = ifelse(MCQ220 %in% c(7, 9), NA, MCQ220),
    MCQ230A = ifelse(MCQ230A %in% c(77, 99), NA, MCQ230A),
    MCQ230B = ifelse(MCQ230B %in% c(77, 99), NA, MCQ230B),
    MCQ230C = ifelse(MCQ230C %in% c(77, 99), NA, MCQ230C),
    
    breast_cancer = case_when(
      MCQ220 == 2 ~ 0,
      MCQ220 == 1 & (MCQ230A == 14 | MCQ230B == 14 | MCQ230C == 14) ~ 1,
      MCQ220 == 1 & MCQ230A != 14 & MCQ230B != 14 & MCQ230C != 14 ~ 0,
      TRUE ~ NA_real_
    )
  )
cat("③ 乳腺癌信息完整样本：", sum(!is.na(female$breast_cancer)), "人\n")

# ========== 4. 暴露变量杂协变量编码 ==========
female <- female %>%
  mutate(
    ALQ111 = ifelse(ALQ111 %in% c(7, 9), NA, ALQ111),
    ALQ121 = ifelse(ALQ121 %in% c(77, 99), NA, ALQ121),
    ALQ130 = ifelse(ALQ130 %in% c(777, 999), NA, ALQ130),
    
    drink_group = case_when(
      ALQ111 == 2 ~ "从不饮酒",
      ALQ121 == 0 ~ "从不饮酒",
      ALQ121 %in% c(6,7,8,9,10) ~ "少量饮酒",
      ALQ121 %in% c(3,4,5) ~ "中等饮酒",
      ALQ121 %in% c(1,2) ~ "频繁饮酒",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("从不饮酒", "少量饮酒", "中等饮酒", "频繁饮酒")),
    
    daily_drink_cups = case_when(drink_group == "从不饮酒" ~ 0, TRUE ~ as.numeric(ALQ130)),
    # 敏感性分析简化3分组
    drink_group_simple = case_when(
      drink_group == "从不饮酒" ~ "从不饮酒",
      drink_group == "少量饮酒" ~ "低频饮酒",
      drink_group %in% c("中等饮酒", "频繁饮酒") ~ "高频饮酒"
    ) %>% factor(levels = c("从不饮酒", "低频饮酒", "高频饮酒")),
    drink_score = as.numeric(drink_group),
    
    # 种族
    race = factor(ifelse(RIDRETH3 %in% c(7,9), NA, RIDRETH3),
                  levels = c(3,1,2,4,6,7),
                  labels = c("非西裔白人", "墨西哥裔", "其他西班牙裔", "非西裔黑人", "其他人种", "多种族")),
    # 教育程度
    education = factor(ifelse(DMDEDUC2 %in% c(7,9), NA, DMDEDUC2),
                       levels = c(4,1,2,3,5),
                       labels = c("本科", "初中及以下", "高中", "大专", "研究生及以上")),
    poverty_ratio = INDFMPIR,
    # 吸烟状态 
    smoke_status = factor(ifelse(SMQ020 %in% c(7,9), NA, SMQ020),
                          levels = c(2,1), labels = c("从不吸烟", "曾吸烟")),
    # 年龄分层
    age_group = ifelse(RIDAGEYR >= 50, "≥50岁", "<50岁") %>% factor(levels = c("<50岁", "≥50岁")),
    # 结局转为分类变量
    breast_cancer = factor(breast_cancer, levels = c(0,1), labels = c("未患乳腺癌", "患乳腺癌"))
  )

# ========== 5. 剔除变量缺失 ==========
female_clean <- female %>% 
  filter(!is.na(breast_cancer), !is.na(drink_group), !is.na(RIDAGEYR), !is.na(BMXBMI),
         !is.na(race), !is.na(education), !is.na(poverty_ratio), !is.na(smoke_status))
cat("④ 最终有效分析样本：", nrow(female_clean), "人\n")

# ========== 6. 基线特征表 ==========
baseline_table <- female_clean %>%
  select(RIDAGEYR, race, education, poverty_ratio, BMXBMI, smoke_status, drink_group, breast_cancer) %>%
  tbl_summary(
    by = breast_cancer,
    statistic = list(all_continuous() ~ "{median}({p25},{p75})", all_categorical() ~ "{n}({p}%)"),
    digits = list(all_continuous() ~ c(1,1), all_categorical() ~ c(0,1)),
    label = list(
      RIDAGEYR ~ "年龄(岁)", race ~ "种族", education ~ "教育程度",
      poverty_ratio ~ "收入贫困比", BMXBMI ~ "BMI(kg/m²)", smoke_status ~ "吸烟状态",
      drink_group ~ "饮酒频次"
    ), missing = "no"
  ) %>%
  add_overall() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test", all_categorical() ~ "chisq.test")) %>%
  modify_caption("表1 研究对象基线特征表")
baseline_table
print(baseline_table)
# 导出Word文件
baseline_table %>% as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表1_基线特征.docx")

# ========== 7. Logistic回归素 ==========
# 单因素回归
uni_model <- glm(breast_cancer ~ drink_group, data = female_clean, family = binomial())
uni_table <- tbl_regression(uni_model, exponentiate = TRUE, label = list(drink_group ~ "饮酒频次")) %>%
  modify_caption("单因素logistic回归")

# 多因素调整回归
multi_model <- glm(breast_cancer ~ drink_group + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
                   data = female_clean, family = binomial())
multi_table <- tbl_regression(multi_model, exponentiate = TRUE, label = list(drink_group ~ "饮酒频次")) %>%
  modify_caption("多因素调整logistic回归")

# 合并
reg_combined <- tbl_merge(list(uni_table, multi_table), tab_spanner = c("模型1 未调整", "模型2 调整全部混杂"))
reg_combined
print(reg_combined)
reg_combined %>% as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表2_回归结果汇总.docx")

# ========== 8. 年龄分层分析 ==========
strat_model <- glm(breast_cancer ~ drink_group + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status + age_group,
                   data = female_clean, family = binomial())
strat_table <- tbl_regression(strat_model, exponentiate = TRUE) %>% modify_caption("表3 年龄分层回归分析")
strat_table
print(strat_table)
strat_table %>% as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表3_分层分析.docx")

# ========== 9. 敏感性分析 ==========
sens_model <- glm(breast_cancer ~ drink_group_simple + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
                  data = female_clean, family = binomial())
sens_table <- tbl_regression(sens_model, exponentiate = TRUE) %>% modify_caption("表4 敏感性分析（饮酒简化分组）")
sens_table
print(sens_table)
sens_table %>% as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表4_敏感性分析.docx")

# ========== 10. 模型诊断 ==========
vif_result <- vif(multi_model)
hl_test <- hoslem.test(female_clean$breast_cancer, fitted(multi_model), g = 10)
cat("\n===== 模型诊断结果 =====\n")
cat("VIF最大值：", max(vif_result), "\n")
cat("Hosmer-Lemeshow检验P值：", hl_test$p.value, "\n")

# ========== 11. ROC曲线 ==========
roc_obj <- roc(female_clean$breast_cancer, fitted(multi_model), quiet = TRUE)
auc_value <- as.numeric(auc(roc_obj))
cat("模型AUC值：", auc_value, "\n")
plot(roc_obj, print.auc = TRUE, main = "多因素回归模型ROC曲线", lwd = 2, cex.main = 1.2)
png("D:/JUMP-R-2026/students/niujiayi/doc/ROC曲线图.png", width = 950, height = 900, res = 300)
plot(roc_obj, print.auc = TRUE, main = "多因素回归模型ROC曲线", lwd = 2)
dev.off()

# ========== 12. 描述性ggplot绘图 ==========
# 图1 年龄分布
p1 <- ggplot(female_clean, aes(x = RIDAGEYR, fill = breast_cancer)) +
  geom_density(alpha = 0.6) + labs(x = "年龄", y = "密度", fill = "乳腺癌状态", title = "研究人群年龄分布") + theme_bw()
print(p1)
ggsave("D:/JUMP-R-2026/students/niujiayi/doc/图1_年龄分布.png", p1, width = 8, height = 5, dpi = 300)

# 图2 饮酒分组构成
p2 <- ggplot(female_clean, aes(x = drink_group, fill = breast_cancer)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent_format()) + # 替换原报错scale_y_percent
  labs(x = "饮酒频次", y = "占比", fill = "患病状态", title = "各组乳腺癌患病占比") + theme_bw()
print(p2)
ggsave("D:/JUMP-R-2026/students/niujiayi/doc/图2_饮酒构成比.png", p2, width = 9, height = 5, dpi = 300)

# 图3 BMI箱线图
p3 <- ggplot(female_clean, aes(x = breast_cancer, y = BMXBMI, fill = breast_cancer)) +
  geom_boxplot() + labs(x = "患病状态", y = "BMI", title = "BMI分布对比") + theme_bw()
print(p3)
ggsave("D:/JUMP-R-2026/students/niujiayi/doc/图3_BMI箱线图.png", p3, width = 7, height = 5, dpi = 300)

# ========== 13. 森林图 ==========
forest_data <- tidy(multi_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)")
p_forest <- ggplot(forest_data, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_point(size = 2) + geom_errorbarh(height = 0.2) +
  labs(x = "OR值", y = "变量", title = "多因素回归森林图") + theme_bw()
print(p_forest)
ggsave("D:/JUMP-R-2026/students/niujiayi/doc/图4_森林图.png", p_forest, width = 10, height = 6, dpi = 300)

#修改ROC图
# 1. 拟合多因素Logistic回归
fit_multi <- glm(
  breast_cancer ~ drink_group + RIDAGEYR + factor(race) + factor(education) + poverty_ratio + BMXBMI + factor(smoke_status),
  data = female, 
  family = binomial(link = "logit"),
  na.action = na.exclude
)

# 2. 提取ROC数据
pred <- predict(fit_multi, type = "response")
valid_idx <- !is.na(pred)
roc_obj <- roc(female$breast_cancer[valid_idx], pred[valid_idx])
roc_data <- data.frame(
  spec = roc_obj$specificities,
  sens = roc_obj$sensitivities
)

# 3. 标准ROC绘图
ggplot(roc_data, aes(x = spec, y = sens)) +
  geom_line(linewidth = 1, color = "black") +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "gray40") +
  labs(
    title = "多因素回归模型ROC曲线",
    x = "特异度 Specificity",
    y = "灵敏度 Sensitivity",
    subtitle = paste0("AUC = ", round(roc_obj$auc, 3))
  ) +
  scale_x_reverse(limits = c(1, 0)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

cat("\n==================== 汇报核心数据汇总 ====================\n")
cat("最终样本量：", nrow(female_clean), "\n")
cat("乳腺癌病例数：", sum(female_clean$breast_cancer == "患乳腺癌"), "\n")
cat("人群患病率：", round(mean(female_clean$breast_cancer == "患乳腺癌")*100,2), "%\n")
cat("VIF最大值：", round(max(vif_result),3), "\n")
cat("HL检验P值：", round(hl_test$p.value,4), "\n")
cat("模型AUC：", round(auc_value,3), "\n")
