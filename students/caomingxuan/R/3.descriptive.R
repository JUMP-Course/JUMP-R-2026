## 4.统计性描述
# 4.1 连续变量描述
# 年龄
cat("年龄 (RIDAGEYR):\n")
cat("  均值 ± 标准差: ", 
    round(mean(analysis_data$RIDAGEYR, na.rm = TRUE), 1), " ± ",  #round(..., 1)四舍五入保留一位小数
    round(sd(analysis_data$RIDAGEYR, na.rm = TRUE), 1), " 岁\n", sep = "")
cat("  中位数 (Q1, Q3): ", 
    round(median(analysis_data$RIDAGEYR, na.rm = TRUE), 1), " (",
    round(quantile(analysis_data$RIDAGEYR, 0.25, na.rm = TRUE), 1), ", ",
    round(quantile(analysis_data$RIDAGEYR, 0.75, na.rm = TRUE), 1), ") 岁\n", sep = "")

# BMI
cat("\nBMI:\n")
cat("  均值 ± 标准差: ", 
    round(mean(analysis_data$BMI, na.rm = TRUE), 1), " ± ",
    round(sd(analysis_data$BMI, na.rm = TRUE), 1), "\n", sep = "")
cat("  中位数 (Q1, Q3): ", 
    round(median(analysis_data$BMI, na.rm = TRUE), 1), " (",
    round(quantile(analysis_data$BMI, 0.25, na.rm = TRUE), 1), ", ",
    round(quantile(analysis_data$BMI, 0.75, na.rm = TRUE), 1), ")\n", sep = "")

# 4.2分类变量描述

# 性别
cat("性别分布:\n")
gender_tab <- table(analysis_data$gender)
gender_prop <- prop.table(gender_tab) * 100  #计算比例（百分数比）
cat("  男性: ", gender_tab[1], " (", round(gender_prop[1], 1), "%)\n", sep = "")
cat("  女性: ", gender_tab[2], " (", round(gender_prop[2], 1), "%)\n", sep = "")

# 种族
cat("\n种族分布:\n")
race_tab <- table(analysis_data$race)
race_prop <- prop.table(race_tab) * 100
race_labels <- c("墨西哥裔美国人", "其他西班牙裔", "非西班牙裔白人", "非西班牙裔黑人", "其他")
for (i in 1:length(race_tab)) {
  cat("  ", race_labels[i], ": ", race_tab[i], " (", round(race_prop[i], 1), "%)\n", sep = "")
}

# 教育水平
cat("\n教育水平分布:\n")
edu_tab <- table(analysis_data$education)
edu_prop <- prop.table(edu_tab) * 100
edu_labels <- c("低于9年级", "9-11年级", "高中毕业", "部分大学", "大学及以上")
for (i in 1:length(edu_tab)) {
  cat("  ", edu_labels[i], ": ", edu_tab[i], " (", round(edu_prop[i], 1), "%)\n", sep = "")
}

# BMI分组
cat("\nBMI分组分布:\n")
bmi_tab <- table(analysis_data$bmi_cat)
bmi_prop <- prop.table(bmi_tab) * 100
for (i in 1:length(bmi_tab)) {
  cat("  ", names(bmi_tab)[i], ": ", bmi_tab[i], " (", round(bmi_prop[i], 1), "%)\n", sep = "")
}

### 4.3 最终高血压患病率
hypertension_rate_final <- mean(analysis_data$hypertension == 1, na.rm = TRUE) * 100  #analysis_data$hypertension == 1 逐一判断是不是等于1，mean（），TRUE=1,FLSE=0

cat("\n————————最终样本量————————\n")
cat("总样本量：", nrow(analysis_data), "人\n")
cat("高血压患病率：", round(hypertension_rate_final, 1), "%\n\n")

cat("————————各BMI组样本量及患病率 ————————\n")
bmi_summary <- analysis_data %>%
  dplyr::group_by(bmi_cat) %>%
  dplyr::summarise(
    n = n(),
    prevalence = round(mean(hypertension == 1) * 100, 1)
  )
print(bmi_summary)

cat("\n———————— 各协变量分布 ————————\n")
cat("性别 (1=男, 2=女):\n")
print(table(analysis_data$gender))
cat("\n种族 (1=墨裔, 2=其他西裔, 3=非西裔白人, 4=非西裔黑人, 5=其他):\n")
print(table(analysis_data$race))
cat("\n教育水平 (1=低于9年级, 2=9-11年级, 3=高中, 4=部分大学, 5=大学及以上):\n")
print(table(analysis_data$education))

### 4.4 基线特征表
library(tableone)
library(kableExtra)

table_vars <- c("bmi_cat", "hypertension", "gender", "education", "race")  #在表格里展示这5个变量
cat_vars <- c("bmi_cat", "hypertension", "gender", "education", "race")  #分类变量
group_var <- "bmi_cat"  #分组变量

table_one <- CreateTableOne(  #CreateTableOne会自动对每个变量进行组间比较检验
  vars = table_vars,  #要分析的变量
  strata = group_var,  #分组依据
  data = analysis_data,  #数据来源
  factorVars = cat_vars  #分类变量
)

table_one_matrix <- print(table_one, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)  #打印成矩阵格式，不要引号，不要多余的空格

kable(table_one_matrix, 
      caption = "表1. 不同BMI分组的研究对象基线特征比较",  #给表格加标题
      booktabs = TRUE) %>%  #使用专业的三线表格式（顶部线、中间线、底部线）
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 12)  #添加斑马纹（隔行变色），固定表格位置，字体大小12