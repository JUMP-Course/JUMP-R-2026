
# ==================== 第1部分：加载R包 ====================

library(dplyr)


# ==================== 第2部分：数据读取 ====================

# 读取2017-2020周期数据
df_2017 <- read.csv("D:/R-data/2017-2020合并数据.csv", 
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE)

# 读取2021-2023周期数据
df_2021 <- read.csv("D:/R-data/2021-2023合并数据.csv", 
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE)

# 标记数据来源时期
df_2017$period <- "2017-2020"
df_2021$period <- "2021-2023"

# 合并两个数据集
df_all <- bind_rows(df_2017, df_2021)

cat("========== 数据读取完成 ==========\n")
cat("2017-2020周期原始记录数：", nrow(df_2017), "\n")
cat("2021-2023周期原始记录数：", nrow(df_2021), "\n")
cat("合并后总记录数：", nrow(df_all), "\n\n")


# ==================== 第3部分：数据清洗 ====================

cat("========== 开始数据清洗 ==========\n")

# ----- 步骤1：保留年龄 >= 18岁的成年人 -----
df_step1 <- df_all %>% filter(RIDAGEYR >= 18)
cat("步骤1：排除年龄 < 18岁 → 保留", nrow(df_step1), "条\n")

# ----- 步骤2：保留甲肝抗体检测结果有效（1=阳性，2=阴性）-----
df_step2 <- df_step1 %>% filter(!is.na(LBXHA) & LBXHA %in% c(1, 2))
cat("步骤2：排除抗体缺失或无效 → 保留", nrow(df_step2), "条\n")

# ----- 步骤3：保留性别变量有效 -----
df_step3 <- df_step2 %>% filter(!is.na(RIAGENDR) & RIAGENDR != "")
cat("步骤3：排除性别缺失 → 保留", nrow(df_step3), "条\n")

# ----- 步骤4：保留教育程度变量有效（1-5为有效值）-----
df_step4 <- df_step3 %>% filter(!is.na(DMDEDUC2) & DMDEDUC2 %in% 1:5)
cat("步骤4：排除教育程度缺失或无效 → 保留", nrow(df_step4), "条\n")

# ----- 步骤5：保留种族变量有效 -----
df_clean <- df_step4 %>% filter(!is.na(RIDRETH3) & RIDRETH3 != "")
cat("步骤5：排除种族缺失 → 保留", nrow(df_clean), "条\n")

cat("\n========== 数据清洗结果汇总 ==========\n")
cat("原始总记录数：", nrow(df_all), "条\n")
cat("最终有效样本：", nrow(df_clean), "条\n")
cat("总排除样本：", nrow(df_all) - nrow(df_clean), "条\n")
cat("保留率：", round(nrow(df_clean) / nrow(df_all) * 100, 2), "%\n\n")


# ==================== 第4部分：变量编码 ====================

cat("========== 变量编码 ==========\n")

df_clean <- df_clean %>%
  mutate(
    # 年龄分组（三组：青年、中年、老年）
    age_group = cut(RIDAGEYR, 
                    breaks = c(18, 45, 65, Inf), 
                    labels = c("18-44", "45-64", "65+"), 
                    right = FALSE),
    
    # 种族重编码（NHANES特定：6→5, 7→6）
    RIDRETH3 = case_when(
      RIDRETH3 == 6 ~ 5,
      RIDRETH3 == 7 ~ 6,
      TRUE ~ RIDRETH3
    ),
    
    # 时期变量（转为因子）
    period = factor(period, levels = c("2017-2020", "2021-2023")),
    
    # 性别（转为因子）
    RIAGENDR = factor(RIAGENDR, 
                      levels = c(1, 2), 
                      labels = c("Male", "Female")),
    
    # 教育程度（转为因子）
    DMDEDUC2 = factor(DMDEDUC2, 
                      levels = 1:5, 
                      labels = c("Primary", "Middle", "High", "College", "University")),
    
    # 种族（转为因子）
    RIDRETH3 = factor(RIDRETH3, 
                      levels = 1:6, 
                      labels = c("Mexican", "Other Hispanic", "White", 
                                 "Black", "Asian", "Other")),
    
    # 甲肝抗体结果（转为因子）
    LBXHA = factor(LBXHA, 
                   levels = c(1, 2), 
                   labels = c("Positive", "Negative"))
  )

cat("✅ 变量编码完成\n\n")


# ==================== 第5部分：表1 研究对象基线特征 ====================

cat("========================================\n")
cat("        表1 研究对象基线人口学特征\n")
cat("========================================\n\n")

# 分时期样本量
n_2017 <- sum(df_clean$period == "2017-2020")
n_2021 <- sum(df_clean$period == "2021-2023")

cat("指标                     2017-2020 (n=", n_2017, ")    2021-2023 (n=", n_2021, ")\n")
cat("--------------------------------------------------------\n")

# ---- 年龄 ----
age_mean_2017 <- round(mean(df_clean$RIDAGEYR[df_clean$period == "2017-2020"], na.rm = TRUE), 1)
age_sd_2017 <- round(sd(df_clean$RIDAGEYR[df_clean$period == "2017-2020"], na.rm = TRUE), 1)
age_mean_2021 <- round(mean(df_clean$RIDAGEYR[df_clean$period == "2021-2023"], na.rm = TRUE), 1)
age_sd_2021 <- round(sd(df_clean$RIDAGEYR[df_clean$period == "2021-2023"], na.rm = TRUE), 1)
age_median_2017 <- median(df_clean$RIDAGEYR[df_clean$period == "2017-2020"], na.rm = TRUE)
age_median_2021 <- median(df_clean$RIDAGEYR[df_clean$period == "2021-2023"], na.rm = TRUE)
age_min_2017 <- min(df_clean$RIDAGEYR[df_clean$period == "2017-2020"], na.rm = TRUE)
age_max_2017 <- max(df_clean$RIDAGEYR[df_clean$period == "2017-2020"], na.rm = TRUE)
age_min_2021 <- min(df_clean$RIDAGEYR[df_clean$period == "2021-2023"], na.rm = TRUE)
age_max_2021 <- max(df_clean$RIDAGEYR[df_clean$period == "2021-2023"], na.rm = TRUE)

cat("年龄\n")
cat("  均数 ± 标准差       ", age_mean_2017, "±", age_sd_2017, "             ", age_mean_2021, "±", age_sd_2021, "\n")
cat("  中位数（范围）       ", age_median_2017, "(", age_min_2017, "-", age_max_2017, ")             ", age_median_2021, "(", age_min_2021, "-", age_max_2021, ")\n\n")

# ---- 性别 ----
male_2017 <- sum(df_clean$RIAGENDR == "Male" & df_clean$period == "2017-2020")
female_2017 <- n_2017 - male_2017
male_2021 <- sum(df_clean$RIAGENDR == "Male" & df_clean$period == "2021-2023")
female_2021 <- n_2021 - male_2021

cat("性别，n（%）\n")
cat("  男性                ", male_2017, "(", round(male_2017/n_2017*100, 1), "%)              ", male_2021, "(", round(male_2021/n_2021*100, 1), "%)\n")
cat("  女性                ", female_2017, "(", round(female_2017/n_2017*100, 1), "%)              ", female_2021, "(", round(female_2021/n_2021*100, 1), "%)\n\n")

# ---- 年龄分组 ----
age18_2017 <- sum(df_clean$age_group == "18-44" & df_clean$period == "2017-2020")
age45_2017 <- sum(df_clean$age_group == "45-64" & df_clean$period == "2017-2020")
age65_2017 <- sum(df_clean$age_group == "65+" & df_clean$period == "2017-2020")
age18_2021 <- sum(df_clean$age_group == "18-44" & df_clean$period == "2021-2023")
age45_2021 <- sum(df_clean$age_group == "45-64" & df_clean$period == "2021-2023")
age65_2021 <- sum(df_clean$age_group == "65+" & df_clean$period == "2021-2023")

cat("年龄分组，n（%）\n")
cat("  18-44岁             ", age18_2017, "(", round(age18_2017/n_2017*100, 1), "%)              ", age18_2021, "(", round(age18_2021/n_2021*100, 1), "%)\n")
cat("  45-64岁             ", age45_2017, "(", round(age45_2017/n_2017*100, 1), "%)              ", age45_2021, "(", round(age45_2021/n_2021*100, 1), "%)\n")
cat("  65岁及以上          ", age65_2017, "(", round(age65_2017/n_2017*100, 1), "%)              ", age65_2021, "(", round(age65_2021/n_2021*100, 1), "%)\n\n")

# ---- 抗体结果 ----
pos_2017 <- sum(df_clean$LBXHA == "Positive" & df_clean$period == "2017-2020")
neg_2017 <- n_2017 - pos_2017
pos_2021 <- sum(df_clean$LBXHA == "Positive" & df_clean$period == "2021-2023")
neg_2021 <- n_2021 - pos_2021

cat("甲肝抗体结果，n（%）\n")
cat("  阳性                ", pos_2017, "(", round(pos_2017/n_2017*100, 1), "%)              ", pos_2021, "(", round(pos_2021/n_2021*100, 1), "%)\n")
cat("  阴性                ", neg_2017, "(", round(neg_2017/n_2017*100, 1), "%)              ", neg_2021, "(", round(neg_2021/n_2021*100, 1), "%)\n")

cat("\n========================================\n")


# ==================== 第6部分：表2 单因素阳性率分析 ====================

cat("\n\n")
cat("========================================\n")
cat("      表2 不同人口学特征人群甲肝抗体单因素阳性率\n")
cat("========================================\n\n")

cat("影响因素           分组                    阳性率 (%)\n")
cat("--------------------------------------------------------\n")

# ---- 性别 ----
rate_male <- round(mean(df_clean$LBXHA[df_clean$RIAGENDR == "Male"] == "Positive") * 100, 1)
rate_female <- round(mean(df_clean$LBXHA[df_clean$RIAGENDR == "Female"] == "Positive") * 100, 1)
cat("性别                男性                    ", rate_male, "\n")
cat("                    女性                    ", rate_female, "\n\n")

# ---- 年龄组 ----
rate_age18 <- round(mean(df_clean$LBXHA[df_clean$age_group == "18-44"] == "Positive") * 100, 1)
rate_age45 <- round(mean(df_clean$LBXHA[df_clean$age_group == "45-64"] == "Positive") * 100, 1)
rate_age65 <- round(mean(df_clean$LBXHA[df_clean$age_group == "65+"] == "Positive") * 100, 1)
cat("年龄组              18-44岁                 ", rate_age18, "\n")
cat("                    45-64岁                 ", rate_age45, "\n")
cat("                    65岁及以上              ", rate_age65, "\n\n")

# ---- 种族 ----
race_levels <- levels(df_clean$RIDRETH3)
for (i in 1:length(race_levels)) {
  race <- race_levels[i]
  rate <- round(mean(df_clean$LBXHA[df_clean$RIDRETH3 == race] == "Positive") * 100, 1)
  if (i == 1) {
    cat("种族                ", race, "     ", rate, "\n")
  } else {
    cat("                    ", race, "     ", rate, "\n")
  }
}
cat("\n")

# ---- 教育程度 ----
edu_levels <- levels(df_clean$DMDEDUC2)
for (i in 1:length(edu_levels)) {
  edu <- edu_levels[i]
  rate <- round(mean(df_clean$LBXHA[df_clean$DMDEDUC2 == edu] == "Positive") * 100, 1)
  if (i == 1) {
    cat("教育水平            ", edu, "                    ", rate, "\n")
  } else {
    cat("                    ", edu, "                    ", rate, "\n")
  }
}

cat("\n========================================\n")


# ==================== 第7部分：表3 粗率与标准化率 ====================

cat("\n\n")
cat("========================================\n")
cat("      表3 粗阳性率与年龄标准化阳性率\n")
cat("========================================\n\n")

# 计算粗率
raw_2017 <- round(mean(df_clean$LBXHA[df_clean$period == "2017-2020"] == "Positive") * 100, 2)
raw_2021 <- round(mean(df_clean$LBXHA[df_clean$period == "2021-2023"] == "Positive") * 100, 2)

# 计算标准化率
df_temp <- df_clean %>%
  mutate(period = as.character(period),
         age_group = as.character(age_group))

age_rates <- df_temp %>%
  group_by(period, age_group) %>%
  summarise(n = n(), pos = sum(LBXHA == "Positive"), rate = pos / n, .groups = "drop")

std_pop <- df_temp %>%
  group_by(age_group) %>%
  summarise(std_n = n(), .groups = "drop")

age_std <- age_rates %>%
  left_join(std_pop, by = "age_group") %>%
  group_by(period) %>%
  summarise(adj = round(sum(rate * std_n) / sum(std_n) * 100, 2), .groups = "drop")

cat("周期              粗阳性率 (%)     年龄标准化阳性率 (%)\n")
cat("--------------------------------------------------------\n")
cat("2017-2020         ", raw_2017, "              ", age_std$adj[age_std$period == "2017-2020"], "\n")
cat("2021-2023         ", raw_2021, "              ", age_std$adj[age_std$period == "2021-2023"], "\n")

cat("\n========================================\n")


# ==================== 第8部分：表4 卡方检验 ====================

cat("\n\n")
cat("========================================\n")
cat("      表4 两周期抗体阳性率卡方检验\n")
cat("========================================\n\n")

# 四格表
tab <- table(df_clean$period, df_clean$LBXHA)

cat("四格表：\n")
print(tab)
cat("\n")

# 卡方检验
chi_result <- chisq.test(tab)

cat("χ² =", round(chi_result$statistic, 2), "\n")
cat("df =", chi_result$parameter, "\n")
cat("P =", format(chi_result$p.value, scientific = TRUE, digits = 3), "\n")

cat("\n========================================\n")


# ==================== 第9部分：完成 ====================

cat("\n========== 描述性统计分析完成 ==========\n")
cat("共生成4张描述性统计表：\n")
cat("  表1 研究对象基线人口学特征\n")
cat("  表2 不同人口学特征人群单因素阳性率\n")
cat("  表3 粗阳性率与年龄标准化阳性率\n")
cat("  表4 两周期抗体阳性率卡方检验\n")
cat("\n最终数据集：df_clean（", nrow(df_clean), "行，", ncol(df_clean), "列）\n")