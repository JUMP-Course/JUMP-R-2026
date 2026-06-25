library(tidyverse)
library(data.table)
library(skimr)
library(naniar)
setwd("D:/ukb-jump/gdm-chd-ukb") 
df <- read_rds("./data/test.rds")
dim(df)
names(df)
df$gdm <- ifelse(!is.na(df$GDM_epistart), 1, 0)
df$chd <- ifelse(!is.na(df$CHD_epistart), 1, 0)
table(df$gdm, useNA = "ifany")
table(df$chd, useNA = "ifany")
cat("===== 性别变量分布 =====\n")
table(df$sex, useNA = "ifany")
cat("===== 变量确认 =====\n")
cat("总变量数：", ncol(df), "个\n")
cat("核心变量存在情况：\n")
cat("- gdm：", exists("gdm", df), "\n")
cat("- chd：", exists("chd", df), "\n")
cat("- age：", exists("age", df), "\n")
cat("- bmi：", exists("bmi", df), "\n")
cat("\n===== 数据清洗前初始样本量 =====\n")
cat("总样本量：", nrow(df), "人（全部为女性）\n")
cat("===== 1. 原始数据核心变量缺失情况 =====\n")
core_vars <- c("gdm", "chd", "age", "bmi", "smoking", "education", "alcohol")
core_missing_raw <- sapply(df[, core_vars], function(x) sum(is.na(x)))
print(core_missing_raw)
cat("\n原始数据总缺失值数量：", sum(core_missing_raw), "个\n")
cat("\n===== 2. 纳入排除标准执行 =====\n")
df_clean <- df[!is.na(df$gdm), ]
cat("排除gdm缺失后：", nrow(df_clean), "人\n")
df_clean <- df_clean[!is.na(df_clean$chd), ]
cat("排除chd缺失后：", nrow(df_clean), "人\n")
df_clean <- df_clean[df_clean$age >= 18 & df_clean$age <= 80, ]
cat("排除极端年龄后：", nrow(df_clean), "人\n")
df_clean <- df_clean[!is.na(df_clean$bmi), ]
cat("排除bmi缺失后：", nrow(df_clean), "人\n")
cat("\n===== 3. 过滤后弱混杂因素缺失情况 =====\n")
weak_vars <- c("smoking", "education", "alcohol")
weak_missing <- sapply(df_clean[, weak_vars], function(x) sum(is.na(x)))
print(weak_missing)
cat("\n弱混杂因素总缺失值数量：", sum(weak_missing), "个\n")
smoking_mode <- names(which.max(table(df_clean$smoking)))
education_mode <- names(which.max(table(df_clean$education)))
alcohol_mode <- names(which.max(table(df_clean$alcohol)))
cat("\n===== 弱混杂因素众数 =====\n")
cat("- smoking众数：", smoking_mode, "\n")
cat("- education众数：", education_mode, "\n")
cat("- alcohol众数：", alcohol_mode, "\n")
df_clean$smoking[is.na(df_clean$smoking)] <- as.integer(smoking_mode)
df_clean$education[is.na(df_clean$education)] <- as.integer(education_mode)
df_clean$alcohol[is.na(df_clean$alcohol)] <- as.integer(alcohol_mode)
weak_missing_after <- sapply(df_clean[, weak_vars], function(x) sum(is.na(x)))
cat("\n✅ 处理后弱混杂因素缺失情况：\n")
print(weak_missing_after)
cat("\n===== 4. 修正变量标签 =====\n")
df_clean$gdm <- factor(df_clean$gdm,
                       levels = c(0, 1),
                       labels = c("无GDM病史", "有GDM病史"))
df_clean$chd <- factor(df_clean$chd,
                       levels = c(0, 1),
                       labels = c("未发生冠心病", "发生冠心病"))
df_clean$smoking <- factor(df_clean$smoking,
                           levels = c(-3, 0, 1, 2),
                           labels = c("不愿作答", "从不吸烟", "既往吸烟", "目前吸烟"))
df_clean$education <- factor(df_clean$education,
                             levels = c(1, 2, 3, 4, 5, 6, -7, -3),
                             labels = c("本科/大学学历", "A水准/同等学力", "GCSE/同等学力",
                                        "CSE/同等学力", "NVQ/HND/同等学力", "其他专业资质",
                                        "以上学历均无", "不愿作答"))
df_clean$alcohol <- factor(df_clean$alcohol,
                           levels = c(-3, 0, 1, 2),
                           labels = c("不愿作答", "从不饮酒", "既往饮酒", "目前饮酒"))
df_clean$age_group <- cut(df_clean$age,
                          breaks = c(18, 30, 40, 50, 60, 70, 80),
                          labels = c("18-29岁", "30-39岁", "40-49岁", "50-59岁", "60-69岁", "70-80岁"),
                          include.lowest = TRUE)
df_clean$bmi_group <- cut(df_clean$bmi,
                          breaks = c(0, 18.5, 24, 28, Inf),
                          labels = c("偏瘦", "正常", "超重", "肥胖"),
                          include.lowest = TRUE)
cat("✅ 变量标签修正完成！\n")
cat("\n===== 5. 保存最终分析数据集 =====\n")
saveRDS(df_clean, "./data/ukb_cleaned_analysis_final.rds")
write.csv(df_clean, "./data/ukb_cleaned_analysis_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("✅ 最终数据集已保存！\n")
cat("📂 R格式路径：./data/ukb_cleaned_analysis_final.rds\n")
cat("📂 CSV格式路径：./data/ukb_cleaned_analysis_final.csv\n")
install.packages(c("dplyr", "tidyr", "openxlsx"))
library(dplyr)
library(openxlsx)
data_dictionary <- tibble(
  变量名 = c("gdm", "chd", "age", "bmi", "smoking", "education", "alcohol", "age_group", "bmi_group"),
  变量全称 = c(
    "妊娠期糖尿病病史",
    "冠心病发病情况",
    "年龄",
    "身体质量指数",
    "吸烟状态",
    "教育程度",
    "饮酒状态",
    "年龄分组",
    "BMI分组"
  ),
  变量类型 = c(
    "二分类分类变量",
    "二分类分类变量",
    "连续变量",
    "连续变量",
    "多分类分类变量",
    "多分类分类变量",
    "多分类分类变量",
    "有序分类变量",
    "有序分类变量"
  ),
  赋值说明 = c(
    "0=无GDM病史；1=有GDM病史",
    "0=未发生冠心病；1=发生冠心病",
    "单位：岁，原始连续数值",
    "单位：kg/m²，原始连续数值",
    "-3=不愿作答；0=从不吸烟；1=既往吸烟；2=目前吸烟",
    "1=本科/大学学历；2=A水准/同等学力；3=GCSE/同等学力；4=CSE/同等学力；5=NVQ/HND/同等学力；6=其他专业资质；-7=以上学历均无；-3=不愿作答",
    "-3=不愿作答；0=从不饮酒；1=既往饮酒；2=目前饮酒",
    "18-29岁、30-39岁、40-49岁、50-59岁、60-69岁、70-80岁",
    "偏瘦、正常、超重、肥胖"
  ),
  缺失处理 = c(
    "原始缺失样本直接剔除",
    "原始缺失样本直接剔除",
    "原始缺失样本直接剔除",
    "原始缺失样本直接剔除",
    "原始缺失采用众数插补",
    "原始缺失采用众数插补",
    "原始缺失采用众数插补",
    "由age变量分组生成，无缺失",
    "由bmi变量分组生成，无缺失"
  ),
  角色定位 = c(
    "暴露变量",
    "结局变量",
    "混杂因素",
    "混杂因素",
    "混杂因素",
    "混杂因素",
    "混杂因素",
    "分组变量/亚组分析用",
    "分组变量/亚组分析用"
  )
)
print(data_dictionary)
write.xlsx(data_dictionary, file = "./data/数据字典.xlsx", rowNames = FALSE)
cat("✅ 数据字典已成功导出为 Excel 文件！\n路径：./data/数据字典.xlsx")
