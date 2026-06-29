# ==============================================
# 研究主题：成年女性饮酒频次与乳腺癌患病的相关性分析
# 数据来源：NHANES 2017-2020 美国全国健康与营养检查调查
# 覆盖内容：全流程数据清洗 → 基线描述 → 统计检验 → 可视化 → 核心回归 → 稳健性验证 → 模型诊断 → 进阶分析
# ==============================================

# ========== 0. 安装与加载全部依赖包（首次运行去掉#安装一次即可） ==========
# install.packages(c("tidyverse", "haven", "gtsummary", "flextable", 
#                    "scales", "broom.helpers", "car", "ResourceSelection", "pROC"))

library(tidyverse)       # 数据清洗+可视化全套工具（含ggplot2、broom）
library(haven)           # 读取NHANES原始.xpt格式数据
library(gtsummary)       # 生成学术规范基线表与回归表
library(scales)          # 坐标轴格式美化
library(flextable)       # 表格批量导出Word
library(car)             # 多重共线性VIF检验
library(ResourceSelection) # Hosmer-Lemeshow拟合优度检验
library(pROC)            # ROC曲线与AUC计算
library(broom)

# ========== 1. 数据读取与全数据集合并 ==========
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

cat("① 原始合并总样本量：", nrow(all_data), "人\n")

# ========== 2. 研究人群初筛：18岁及以上成年女性 ==========
female <- all_data %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 18)

cat("② 筛选成年女性后样本量：", nrow(female), "人\n")

# ========== 3. 结局变量定义：乳腺癌患病状态 ==========
# 严格匹配NHANES官方跳题逻辑：MCQ220为总开关，MCQ230a/b/c判定癌症类型
female <- female %>%
  mutate(
    # 无效编码（拒绝/不知道）统一设为缺失
    MCQ220 = ifelse(MCQ220 %in% c(7, 9), NA, MCQ220),
    MCQ230A = ifelse(MCQ230A %in% c(77, 99), NA, MCQ230A),
    MCQ230B = ifelse(MCQ230B %in% c(77, 99), NA, MCQ230B),
    MCQ230C = ifelse(MCQ230C %in% c(77, 99), NA, MCQ230C),
    
    breast_cancer = case_when(
      MCQ220 == 2 ~ 0,                              # 未患任何癌症 → 未患乳腺癌
      MCQ220 == 1 & (MCQ230A == 14 | MCQ230B == 14 | MCQ230C == 14) ~ 1, # 确诊乳腺癌
      MCQ220 == 1 & (MCQ230A != 14 & MCQ230B != 14 & MCQ230C != 14) ~ 0, # 患其他癌症
      TRUE ~ NA_real_                               # 信息不全 → 缺失
    )
  )

cat("③ 乳腺癌信息完整样本量：", sum(!is.na(female$breast_cancer)), "人\n")

# ========== 4. 暴露变量 + 全部协变量标准化定义 ==========
female <- female %>%
  mutate(
    # --------------------------
    # 4.1 核心暴露：饮酒频次分组
    # --------------------------
    ALQ111 = ifelse(ALQ111 %in% c(7, 9), NA, ALQ111),
    ALQ121 = ifelse(ALQ121 %in% c(77, 99), NA, ALQ121),
    ALQ130 = ifelse(ALQ130 %in% c(777, 999), NA, ALQ130),
    
    drink_group = case_when(
      ALQ111 == 2 ~ "从不饮酒",       # 终身未饮酒
      ALQ121 == 0 ~ "从不饮酒",       # 过去12个月未饮酒（戒酒）
      ALQ121 %in% c(6,7,8,9,10) ~ "少量饮酒", # 每月及以下频率
      ALQ121 %in% c(3,4,5) ~ "中等饮酒",      # 每周1-4次
      ALQ121 %in% c(1,2) ~ "频繁饮酒",        # 几乎每天/每天
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("从不饮酒", "少量饮酒", "中等饮酒", "频繁饮酒")),
    
    # 协变量：日均饮酒杯数（控制饮酒量混杂）
    daily_drink_cups = case_when(
      drink_group == "从不饮酒" ~ 0,
      TRUE ~ as.numeric(ALQ130)
    ),
    
    # 敏感性分析用：简化3分组
    drink_group_simple = case_when(
      drink_group == "从不饮酒" ~ "从不饮酒",
      drink_group == "少量饮酒" ~ "低频率饮酒",
      drink_group %in% c("中等饮酒", "频繁饮酒") ~ "高频率饮酒"
    ) %>% 
      factor(levels = c("从不饮酒", "低频率饮酒", "高频率饮酒")),
    
    # 趋势检验用：有序评分
    drink_score = as.numeric(drink_group),
    
    # --------------------------
    # 4.2 人口学协变量（优化参照组，符合学术惯例）
    # --------------------------
    # 种族：参照组为占比最高的非西裔白人
    race = factor(ifelse(RIDRETH3 %in% c(7,9), NA, RIDRETH3),
                  levels = c(3,1,2,4,6,7),
                  labels = c("非西裔白人", "墨西哥裔", "其他西班牙裔",
                             "非西裔黑人", "其他种族", "多民族")),
    
    # 教育程度：参照组为中间水平本科
    education = factor(ifelse(DMDEDUC2 %in% c(7,9), NA, DMDEDUC2),
                       levels = c(4,1,2,3,5),
                       labels = c("本科", "初中及以下", "高中", "大专", "研究生及以上")),
    
    # 家庭收入贫困比（连续变量）
    poverty_ratio = INDFMPIR,
    
    # --------------------------
    # 4.3 吸烟状态（终身吸烟史金标准定义）
    # --------------------------
    smoke_status = factor(ifelse(SMQ020 %in% c(7,9), NA, SMQ020),
                          levels = c(2, 1),
                          labels = c("从不吸烟", "曾吸烟")),
    
    # --------------------------
    # 4.4 分层用变量
    # --------------------------
    age_group = ifelse(RIDAGEYR >= 50, "≥50岁", "<50岁") %>% 
      factor(levels = c("<50岁", "≥50岁")),
    
    # --------------------------
    # 4.5 结局变量转因子（用于表格与绘图）
    # --------------------------
    breast_cancer = factor(
      breast_cancer,
      levels = c(0, 1),
      labels = c("未患乳腺癌", "患乳腺癌")
    )
  )

# ========== 5. 缺失值剔除，确定最终分析样本 ==========
female_clean <- female %>% 
  filter(!is.na(breast_cancer), !is.na(drink_group),
         !is.na(RIDAGEYR), !is.na(BMXBMI),
         !is.na(race), !is.na(education), 
         !is.na(poverty_ratio), !is.na(smoke_status))

cat("④ 剔除全部协变量缺失后最终分析样本量：", nrow(female_clean), "人\n")

# 样本筛选流程标准化输出（写报告直接复制）
cat("\n===== 样本筛选流程 =====\n")
cat("原始合并总样本：", nrow(all_data), "人\n")
cat("筛选18岁及以上成年女性：", nrow(female), "人\n")
cat("剔除乳腺癌患病信息缺失：", sum(is.na(female$breast_cancer)), "人\n")
cat("剔除饮酒频次信息缺失：", sum(is.na(female$drink_group) & !is.na(female$breast_cancer)), "人\n")
cat("剔除协变量信息缺失：", nrow(female %>% filter(!is.na(breast_cancer), !is.na(drink_group))) - nrow(female_clean), "人\n")
cat("最终纳入分析样本：", nrow(female_clean), "人\n")

# 核心变量分布核查
cat("\n===== 核心变量分布核查 =====\n")
print(table(female_clean$drink_group, useNA = "ifany"))
cat("\n")
print(table(female_clean$breast_cancer, useNA = "ifany"))
cat("\n")
print(table(female_clean$smoke_status, useNA = "ifany"))

# ========== 6. 导出清洗后分析数据集 ==========
analysis_data <- female_clean %>% 
  select(SEQN, RIDAGEYR, age_group, race, education, poverty_ratio, 
         BMXBMI, smoke_status, drink_group, drink_group_simple, 
         drink_score, daily_drink_cups, breast_cancer, MCQ220)

write.csv(
  analysis_data, 
  "D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_最终清洗数据.csv", 
  row.names = FALSE
)

cat("\n===== 最终变量缺失值核查 =====\n")
print(colSums(is.na(analysis_data)))

# ========== 7. 基线特征表（优化统计方法，适配偏态数据） ==========
baseline_table <- analysis_data %>%
  select(-SEQN, -drink_group_simple, -drink_score, -daily_drink_cups, -age_group, -MCQ220) %>%
  tbl_summary(
    by = breast_cancer,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ c(1, 1), all_categorical() ~ c(0, 1)),
    label = list(
      RIDAGEYR ~ "年龄（岁）",
      race ~ "种族",
      education ~ "教育程度",
      poverty_ratio ~ "收入贫困比",
      BMXBMI ~ "体重指数（kg/m²）",
      smoke_status ~ "吸烟状态",
      drink_group ~ "饮酒频次分组"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%       
  add_p(test = list(all_continuous() ~ "wilcox.test", all_categorical() ~ "chisq.test")) %>%
  modify_header(label = "特征") %>%
  modify_caption("表1 研究对象基线特征表") %>%
  modify_footnote(all_stat_cols() ~ "数据以中位数（四分位数）或例数（百分比）表示")

baseline_table

# 导出Word
baseline_table %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表1_基线特征表.docx")

# ========== 8. 组间比较统计检验 ==========
# 卡方检验：饮酒分组与乳腺癌
chisq_result <- chisq.test(table(analysis_data$drink_group, analysis_data$breast_cancer))
print("===== 饮酒分组与乳腺癌 卡方检验结果 =====")
print(chisq_result)

# Wilcoxon秩和检验：年龄、BMI（适配偏态分布）
wilcox_age <- wilcox.test(RIDAGEYR ~ breast_cancer, data = analysis_data)
print("===== 年龄组间比较 Wilcoxon秩和检验 =====")
print(wilcox_age)

wilcox_bmi <- wilcox.test(BMXBMI ~ breast_cancer, data = analysis_data)
print("===== BMI组间比较 Wilcoxon秩和检验 =====")
print(wilcox_bmi)

# ========== 9. 基础统计图表 ==========
common_theme <- theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

# 图1：年龄分布密度图
p1 <- ggplot(analysis_data, aes(x = RIDAGEYR, fill = breast_cancer)) +
  geom_density(alpha = 0.6) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的年龄分布",
    x = "年龄（岁）",
    y = "密度",
    fill = "患病状态"
  ) +
  common_theme
print(p1)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图1_年龄分布密度图.png",
  plot = p1, width = 8, height = 5, dpi = 300
)

# 图2：饮酒频次构成比
p2 <- ggplot(analysis_data %>% filter(!is.na(drink_group)), 
             aes(x = breast_cancer, fill = drink_group)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的饮酒频次构成",
    x = "患病状态",
    y = "组内占比",
    fill = "饮酒频次分组"
  ) +
  common_theme
print(p2)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图2_饮酒频次构成比.png",
  plot = p2, width = 8, height = 6, dpi = 300
)

# 图3：BMI分布箱线图
p3 <- ggplot(analysis_data, aes(x = breast_cancer, y = BMXBMI, fill = breast_cancer)) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.color = "red") +
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的BMI分布",
    x = "患病状态",
    y = "体重指数 BMI (kg/m²)"
  ) +
  common_theme +
  theme(legend.position = "none") 
print(p3)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图3_BMI分布箱线图.png",
  plot = p3, width = 7, height = 5, dpi = 300
)

# ========== 10. 核心logistic回归（单因素+多因素合并表） ==========
# 10.1 单因素回归
uni_reg_table <- analysis_data %>%
  select(-SEQN, -drink_group_simple, -drink_score, -daily_drink_cups, -age_group, -MCQ220) %>%
  tbl_uvregression(
    method = glm,
    y = breast_cancer,
    method.args = list(family = binomial(link = "logit")),
    exponentiate = TRUE,  
    label = list(
      drink_group ~ "饮酒频次分组",
      RIDAGEYR ~ "年龄（岁）",
      race ~ "种族",
      education ~ "教育程度",
      poverty_ratio ~ "收入贫困比",
      BMXBMI ~ "体重指数（kg/m²）",
      smoke_status ~ "吸烟状态"
    )
  ) %>%
  modify_header(label = "特征")

# 10.2 多因素调整回归（基准核心模型）
multi_model <- glm(
  breast_cancer ~ drink_group + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = analysis_data
)

multi_reg_table <- tbl_regression(
  multi_model,
  exponentiate = TRUE,
  label = list(
    drink_group ~ "饮酒频次分组",
    RIDAGEYR ~ "年龄（岁）",
    race ~ "种族",
    education ~ "教育程度",
    poverty_ratio ~ "收入贫困比",
    BMXBMI ~ "体重指数（kg/m²）",
    smoke_status ~ "吸烟状态"
  )
) %>%
  modify_header(label = "特征")

# 10.3 单因素+多因素合并表（学术论文标准格式）
reg_combined <- tbl_merge(
  list(uni_reg_table, multi_reg_table),
  tab_spanner = c("**单因素回归**", "**多因素调整回归**")
) %>%
  modify_caption("表2 饮酒频次与乳腺癌患病的logistic回归分析结果")

reg_combined

# 导出Word
reg_combined %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表2_回归分析合并表.docx")

# ========== 11. 多因素回归森林图（中文标签优化版） ==========
# 提取回归结果
res <- tidy(multi_model, exponentiate = TRUE, conf.int = TRUE)

# 自定义中文标签映射
label_map <- c(
  "drink_group少量饮酒" = "  少量饮酒",
  "drink_group中等饮酒" = "  中等饮酒",
  "drink_group频繁饮酒" = "  频繁饮酒",
  "RIDAGEYR" = "年龄（岁）",
  "race墨西哥裔" = "  墨西哥裔",
  "race其他西班牙裔" = "  其他西班牙裔",
  "race非西裔黑人" = "  非西裔黑人",
  "race其他种族" = "  其他种族",
  "race多民族" = "  多民族",
  "education初中及以下" = "  初中及以下",
  "education高中" = "  高中",
  "education大专" = "  大专",
  "education研究生及以上" = "  研究生及以上",
  "poverty_ratio" = "收入贫困比",
  "BMXBMI" = "体重指数（kg/m²）",
  "smoke_status曾吸烟" = "  曾吸烟"
)

res$label <- factor(label_map[res$term], levels = rev(label_map))

# 绘制森林图
ggplot(res, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#1f4e79") +
  geom_point(size = 3, color = "#1f4e79") +
  scale_x_log10() +
  labs(
    title = "图4 多因素logistic回归森林图",
    x = "OR值 (95%置信区间)",
    y = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 11)
  )

# 导出高清图片
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图4_多因素回归森林图.png",
  width = 10, height = 7.5, dpi = 300, bg = "white"
)

cat("\n✅ 森林图导出完成\n")

# ========== 12. 分层分析 ==========
# 12.1 按年龄分层（50岁为界，对应绝经状态）
model_under50 <- glm(
  breast_cancer ~ drink_group + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = filter(female_clean, age_group == "<50岁")
)

model_over50 <- glm(
  breast_cancer ~ drink_group + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = filter(female_clean, age_group == "≥50岁")
)

strat_age_table <- tbl_merge(
  list(
    tbl_regression(model_under50, exponentiate = TRUE, label = list(drink_group ~ "饮酒频次分组")),
    tbl_regression(model_over50, exponentiate = TRUE, label = list(drink_group ~ "饮酒频次分组"))
  ),
  tab_spanner = c("<50岁女性", "≥50岁女性")
) %>%
  modify_caption("表3 按年龄分层的logistic回归分析结果")

strat_age_table

# 导出Word
strat_age_table %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表3_年龄分层回归表.docx")

# 12.2 按吸烟状态分层（备用）
model_nosmoke <- glm(
  breast_cancer ~ drink_group + RIDAGEYR + race + education + poverty_ratio + BMXBMI,
  family = binomial(link = "logit"),
  data = filter(female_clean, smoke_status == "从不吸烟")
)

# ========== 13. 三项敏感性分析（验证结果稳健性） ==========
# 敏感性1：调整暴露分组方式（4组→3组）
sens_model1 <- glm(
  breast_cancer ~ drink_group_simple + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = female_clean
)

sens_table1 <- tbl_regression(
  sens_model1, exponentiate = TRUE,
  label = list(drink_group_simple ~ "饮酒频次（3分组）")
) %>%
  modify_caption("敏感性分析1：3分组暴露变量的回归结果")

# 敏感性2：排除患其他癌症的人群，消除反向因果干扰
sens_data2 <- female_clean %>%
  filter(!(MCQ220 == 1 & breast_cancer == "未患乳腺癌")) 

sens_model2 <- glm(
  breast_cancer ~ drink_group + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = sens_data2
)

sens_table2 <- tbl_regression(
  sens_model2, exponentiate = TRUE,
  label = list(drink_group ~ "饮酒频次分组")
) %>%
  modify_caption("敏感性分析2：排除其他癌症人群后的回归结果")

# 敏感性3：额外调整日均饮酒杯数，控制同频率下饮用量差异
sens_model3 <- glm(
  breast_cancer ~ drink_group + daily_drink_cups + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = female_clean
)

sens_table3 <- tbl_regression(
  sens_model3, exponentiate = TRUE,
  label = list(drink_group ~ "饮酒频次分组", daily_drink_cups ~ "日均饮酒杯数")
) %>%
  modify_caption("敏感性分析3：调整日均饮酒量后的回归结果")

# 合并敏感性分析结果表（汇报用）
sens_combined <- tbl_merge(
  list(sens_table1, sens_table2, sens_table3),
  tab_spanner = c("**3分组暴露**", "**排除其他癌症**", "**调整饮酒量**")
) %>%
  modify_caption("表4 敏感性分析结果汇总")

sens_combined %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/表4_敏感性分析汇总.docx")

# ========== 14. 完整模型诊断 ==========
cat("\n===== 模型诊断结果 =====\n")

# 14.1 多重共线性检验（VIF < 5 无严重共线性）
cat("1. 多重共线性VIF检验：\n")
print(vif(multi_model))

# 14.2 Hosmer-Lemeshow拟合优度检验（P > 0.05 拟合良好）
hl_test <- hoslem.test(multi_model$y, fitted(multi_model), g = 10)
cat("\n2. Hosmer-Lemeshow拟合优度检验：\n")
print(hl_test)

# 14.3 ROC曲线与AUC值（模型区分度评价）
roc_obj <- roc(female_clean$breast_cancer, fitted(multi_model), quiet = TRUE)
auc_value <- as.numeric(auc(roc_obj))

cat("\n3. 模型AUC值：", round(auc_value, 4), "\n")
cat("   AUC>0.7提示模型有较好的区分能力\n")

# 绘制并导出ROC曲线
png("D:/JUMP-R-2026/students/niujiayi/doc/图5_ROC曲线.png", 
    width = 700, height = 600, res = 300)
plot(roc_obj, 
     main = "图5 多因素logistic回归ROC曲线",
     col = "#1f4e79", lwd = 2,
     print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
abline(a = 0, b = 1, lty = 2, col = "gray50")
dev.off()

cat("\n✅ ROC曲线导出完成\n")

# ========== 15. 进阶补充分析（汇报加分项） ==========
# 15.1 饮酒×年龄 交互作用检验（似然比检验法，规范整体检验）
# 无交互项的嵌套基准模型
base_inter_model <- glm(
  breast_cancer ~ drink_group + age_group + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = female_clean
)

# 含交互项的完整模型
inter_model <- glm(
  breast_cancer ~ drink_group * age_group + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = female_clean
)

# 似然比检验
lr_test <- anova(base_inter_model, inter_model, test = "Chisq")
inter_p <- lr_test$`Pr(>Chi)`[2]

cat("\n===== 饮酒×年龄 交互作用检验（似然比法） =====\n")
cat("交互作用整体P值：", round(inter_p, 4), "\n")
cat("P<0.05提示年龄与饮酒的分层差异有统计学意义\n")

# 15.2 饮酒频率 剂量反应趋势检验
trend_model <- glm(
  breast_cancer ~ drink_score + RIDAGEYR + race + education + poverty_ratio + BMXBMI + smoke_status,
  family = binomial(link = "logit"),
  data = female_clean
)

trend_p <- broom::tidy(trend_model) %>%
  filter(term == "drink_score") %>%
  pull(p.value)

trend_or <- broom::tidy(trend_model, exponentiate = TRUE) %>%
  filter(term == "drink_score") %>%
  pull(estimate)

cat("\n===== 饮酒频率 剂量反应趋势检验 =====\n")
cat("每升高一个饮酒等级，OR值为：", round(trend_or, 4), "\n")
cat("趋势检验P值：", round(trend_p, 4), "\n")
cat("P<0.05提示乳腺癌风险随饮酒频率升高呈显著上升趋势\n")

cat("\n🎉 全流程分析全部运行完成，所有表格与图片已导出至指定文件夹")