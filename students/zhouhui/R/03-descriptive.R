if (!require("dplyr")) install.packages("dplyr")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("flextable")) install.packages("flextable")
library(dplyr)
library(gtsummary)
setwd("D:/ukb-jump/gdm-chd-ukb")
df_clean <- readRDS("./data/ukb_cleaned_analysis_final.rds")
cat("===== 数据集基本信息 =====\n")
cat("总样本量：", nrow(df_clean), "\n")
cat("变量个数：", ncol(df_clean), "\n")
str(df_clean)  # 查看变量类型，核对分类/连续变量

# ========== 第一步：数值层面初步判断 ==========
cat("===== 年龄分布统计 =====\n")
summary(df_clean$age)
cat("均值 =", mean(df_clean$age), "\n")
cat("中位数 =", median(df_clean$age), "\n")
cat("标准差 =", sd(df_clean$age), "\n")
cat("四分位数：", quantile(df_clean$age, c(0.25, 0.75)), "\n\n")
cat("===== BMI分布统计 =====\n")
summary(df_clean$bmi)
cat("均值 =", mean(df_clean$bmi), "\n")
cat("中位数 =", median(df_clean$bmi), "\n")
cat("标准差 =", sd(df_clean$bmi), "\n")
cat("四分位数：", quantile(df_clean$bmi, c(0.25, 0.75)), "\n")
# ========== 第二步：画直方图，直观看形状 ==========
# 年龄直方图
p_age_hist <- ggplot(df_clean, aes(x = age)) +
  geom_histogram(bins = 30, fill = "#4DBBD5FF", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(age)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(age)), 
             color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    title = "年龄分布直方图",
    x = "年龄(岁)", y = "人数",
    caption = "红色虚线=均值，蓝色虚线=中位数"
  ) +
  theme_bw(base_size = 12)

print(p_age_hist)
ggsave("./figures/年龄分布直方图.png", p_age_hist, width = 6, height = 4, dpi = 300)

# BMI直方图
p_bmi_hist <- ggplot(df_clean, aes(x = bmi)) +
  geom_histogram(bins = 30, fill = "#E64B35FF", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(bmi)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(bmi)), 
             color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    title = "BMI分布直方图",
    x = "BMI (kg/m²)", y = "人数",
    caption = "红色虚线=均值，蓝色虚线=中位数"
  ) +
  theme_bw(base_size = 12)

print(p_bmi_hist)
ggsave("./figures/BMI分布直方图.png", p_bmi_hist, width = 6, height = 4, dpi = 300)


# ========== 第三步：画QQ图，判断正态性 ==========
# 年龄QQ图
p_age_qq <- ggplot(df_clean, aes(sample = age)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "年龄正态QQ图", x = "理论分位数", y = "实际分位数") +
  theme_bw(base_size = 12)

print(p_age_qq)
ggsave("./figures/年龄正态QQ图.png", p_age_qq, width = 6, height = 4, dpi = 300)
# bmiQQ图
p_bmi_qq <- ggplot(df_clean, aes(sample = bmi)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "BMI正态QQ图", x = "理论分位数", y = "实际分位数") +
  theme_bw(base_size = 12)

print(p_bmi_qq)
ggsave("./figures/BMI正态QQ图.png", p_bmi_qq, width = 6, height = 4, dpi = 300)

cat("\n\n===== 全体人群描述性统计结果 =====\n")
# ========== 1. 连续变量 ==========
# 1.1 年龄：近似正态分布 → 均数 ± 标准差
age_mean <- round(mean(df_clean$age), 1)
age_sd <- round(sd(df_clean$age), 1)
cat("年龄(岁)：", age_mean, " ± ", age_sd, "\n", sep = "")
# 1.2 BMI：右偏态分布 → 中位数 (25分位数, 75分位数)
bmi_quant <- quantile(df_clean$bmi, c(0.5, 0.25, 0.75))
bmi_median <- round(bmi_quant[1], 1)
bmi_q25 <- round(bmi_quant[2], 1)
bmi_q75 <- round(bmi_quant[3], 1)
cat("BMI(kg/m²)：", bmi_median, " (", bmi_q25, ", ", bmi_q75, ")\n", sep = "")
# ========== 2. 二分类变量 ==========
cat("\n--- 2. 二分类变量 ---\n")
# 2.1 妊娠期糖尿病(GDM)
gdm_n <- table(df_clean$gdm)
gdm_p <- round(prop.table(gdm_n) * 100, 1)
cat("妊娠期糖尿病病史：\n")
cat("  ", names(gdm_n)[1], "：", gdm_n[1], "例 (", gdm_p[1], "%)\n", sep = "")
cat("  ", names(gdm_n)[2], "：", gdm_n[2], "例 (", gdm_p[2], "%)\n", sep = "")
# 2.2 冠心病(CHD)
chd_n <- table(df_clean$chd)
chd_p <- round(prop.table(chd_n) * 100, 1)
cat("冠心病发病情况：\n")
cat("  ", names(chd_n)[1], "：", chd_n[1], "例 (", chd_p[1], "%)\n", sep = "")
cat("  ", names(chd_n)[2], "：", chd_n[2], "例 (", chd_p[2], "%)\n", sep = "")

# ========== 3. 无序多分类变量 ==========
cat("\n--- 3. 无序多分类变量 ---\n")

# 3.1 吸烟状态
smk_n <- table(df_clean$smoking)
smk_p <- round(prop.table(smk_n) * 100, 1)
cat("吸烟状态：\n")
for (i in seq_along(smk_n)) {
  cat("  ", names(smk_n)[i], "：", smk_n[i], "例 (", smk_p[i], "%)\n", sep = "")
}
# 3.2 饮酒状态
alc_n <- table(df_clean$alcohol)
alc_p <- round(prop.table(alc_n) * 100, 1)
cat("饮酒状态：\n")
for (i in seq_along(alc_n)) {
  cat("  ", names(alc_n)[i], "：", alc_n[i], "例 (", alc_p[i], "%)\n", sep = "")
}
# ========== 4. 有序多分类变量 ==========
cat("\n--- 4. 有序多分类变量 ---\n")

# 4.1 教育程度
edu_n <- table(df_clean$education)
edu_p <- round(prop.table(edu_n) * 100, 1)
cat("教育程度：\n")
for (i in seq_along(edu_n)) {
  cat("  ", names(edu_n)[i], "：", edu_n[i], "例 (", edu_p[i], "%)\n", sep = "")
}
# 4.2 年龄分组
ageg_n <- table(df_clean$age_group)
ageg_p <- round(prop.table(ageg_n) * 100, 1)
cat("年龄分组：\n")
for (i in seq_along(ageg_n)) {
  cat("  ", names(ageg_n)[i], "：", ageg_n[i], "例 (", ageg_p[i], "%)\n", sep = "")
}
# 4.3 BMI分组
bmig_n <- table(df_clean$bmi_group)
bmig_p <- round(prop.table(bmig_n) * 100, 1)
cat("BMI分组：\n")
for (i in seq_along(bmig_n)) {
  cat("  ", names(bmig_n)[i], "：", bmig_n[i], "例 (", bmig_p[i], "%)\n", sep = "")
}

# ==============================================
cat("\n===== 正在生成GDM分组基线对比表 =====\n")
tbl_baseline <- df_clean %>%
  select(chd, age, bmi, age_group, bmi_group, smoking, education, alcohol, gdm) %>%
  
  tbl_summary(
    by = gdm,
    statistic = list(
      age ~ "{mean} ± {sd}",
      bmi ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      age ~ 1,
      bmi ~ 1,
      all_categorical() ~ 1
    ),
    label = list(
      chd ~ "冠心病发病情况",
      age ~ "年龄(岁)",
      bmi ~ "身体质量指数(kg/m²)",
      age_group ~ "年龄分组",
      bmi_group ~ "BMI分组",
      smoking ~ "吸烟状态",
      education ~ "教育程度",
      alcohol ~ "饮酒状态"
    )
  ) %>%
  
  add_n() %>%
  modify_header(label = "**变量**") %>%
  modify_caption("**表2 GDM组与非GDM组研究对象基线特征对比**")
print(tbl_baseline)
# 导出为Word文档
tbl_baseline %>%
  as_flex_table() %>%
  save_as_docx(path = "./output/表2_GDM分组基线对比_无P值.docx")
library(flextable)
tbl_baseline %>%
  as_flex_table() %>%
  save_as_docx(path = "./output/表2_GDM分组基线对比_无P值.docx")
cat("\n✅ 基线表生成并导出完成，文件已保存至output文件夹\n")
