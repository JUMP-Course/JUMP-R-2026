library(tidyverse)
library(readxl)
library(skimr)
library(naniar)
library(visdat)
file_path<-"D:/Flu.xlsx"
df_raw <- read_excel(file_path, 
                     sheet = 1)
cat("数据来源：中国疾控中心国家流感中心《全国流感监测周报》\n")
cat("研究对象：全国哨点医院流感样病例（ILI）\n")
cat("数据维度：", nrow(df_raw), "行 ×", ncol(df_raw), "列\n")

var_info <- df_raw %>%
  summarise(across(everything(), ~ class(.))) %>%
  pivot_longer(everything(), names_to = "变量名", values_to = "数据类型")
print(var_info, n = 20)
core_vars <- data.frame(
  变量名 = c("年份", "周次", "南方ILI(%)", "北方ILI(%)", "总检测标本数", 
          "总阳性标本数", "甲流占阳性比(%)", "乙流占阳性比(%)"),
  含义 = c("年份", "周次（1-53）", "南方流感样病例占比", "北方流感样病例占比",
         "每周检测标本总数", "流感阳性标本数", "甲型流感占阳性标本比例",
         "乙型流感占阳性标本比例"),
  类型 = c("整数", "整数", "数值", "数值", "整数", "整数", "数值", "数值")
)
print(core_vars)

miss_detail <- df_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "变量", values_to = "缺失数量") %>%
  mutate(缺失比例 = round(缺失数量 / nrow(df_raw), 4)) %>%
  filter(缺失数量 > 0) %>%
  arrange(desc(缺失数量))
print(miss_detail)

cat("\n缺失具体位置：\n")
df_raw %>%
  filter(is.na(`南方ILI(%)`) | is.na(`北方ILI(%)`)) %>%
  select(年份, 周次, `南方ILI(%)`, `北方ILI(%)`) %>%
  print()

cat("\n结论：仅2017年第41-42周缺失南方/北方ILI(%)，共2行，其余变量无缺失。\n")
# IQR异常检测
numeric_vars <- c("南方ILI(%)", "北方ILI(%)", "总检测标本数", 
                  "总阳性标本数", "甲流占阳性比(%)", "乙流占阳性比(%)")

outlier_report <- data.frame()
for (var in numeric_vars) {
  x <- df_raw[[var]] %>% na.omit()
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outliers <- x[x < lower | x > upper]
  
  outlier_report <- rbind(outlier_report, data.frame(
    变量 = var,
    下界 = round(lower, 2),
    上界 = round(upper, 2),
    异常值个数 = length(outliers),
    异常值比例 = round(length(outliers) / length(x), 4)
  ))
}
print(outlier_report)

cat("\n异常值判断：\n")
cat("- 南方ILI(%)：上限6.1%，实际最大值9.7%（2024年初），属正常高峰期波动\n")
cat("- 北方ILI(%)：上限5.9%，实际最大值6.8%（2024年初），属正常\n")
cat("- 阳性标本数：高峰期数值偏高，符合流行病学规律\n")
cat("结论：所有异常值均为真实流行病学现象，予以保留，不做删除。\n")
cat("\n关键变量分布摘要：\n")
df_raw %>%
  summarise(
    南方ILI_均值 = mean(`南方ILI(%)`, na.rm = TRUE),
    南方ILI_中位数 = median(`南方ILI(%)`, na.rm = TRUE),
    南方ILI_范围 = paste(range(`南方ILI(%)`, na.rm = TRUE), collapse = " - "),
    北方ILI_均值 = mean(`北方ILI(%)`, na.rm = TRUE),
    北方ILI_中位数 = median(`北方ILI(%)`, na.rm = TRUE),
    北方ILI_范围 = paste(range(`北方ILI(%)`, na.rm = TRUE), collapse = " - "),
    阳性数_均值 = mean(总阳性标本数, na.rm = TRUE),
    阳性数_中位数 = median(总阳性标本数, na.rm = TRUE),
    阳性数_范围 = paste(range(总阳性标本数, na.rm = TRUE), collapse = " - ")
  ) %>%
  print()

# 分布直方图
p1 <- ggplot(df_raw, aes(x = `南方ILI(%)`)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "南方ILI(%)分布", x = "南方ILI(%)", y = "频数") +
  theme_minimal()

p2 <- ggplot(df_raw, aes(x = `北方ILI(%)`)) +
  geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
  labs(title = "北方ILI(%)分布", x = "北方ILI(%)", y = "频数") +
  theme_minimal()

p3 <- ggplot(df_raw, aes(x = 总阳性标本数)) +
  geom_histogram(bins = 30, fill = "seagreen", alpha = 0.7) +
  labs(title = "总阳性标本数分布", x = "阳性标本数", y = "频数") +
  theme_minimal()

print(p1)
print(p2)
print(p3)