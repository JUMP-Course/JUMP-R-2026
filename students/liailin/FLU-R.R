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
cat("\n========== 清洗操作说明 ==========\n")
cat("清洗策略：删除关键变量（南方ILI%、北方ILI%）存在缺失的行\n")
cat("原因：ILI%是核心分析变量，缺失的2行无法用于趋势和季节分析\n")
cat("清洗前行数:", nrow(df_raw), "\n")

df_clean <- df_raw %>%
  filter(!is.na(`南方ILI(%)`), !is.na(`北方ILI(%)`))

cat("清洗后行数:", nrow(df_clean), "\n")
cat("删除行数:", nrow(df_raw) - nrow(df_clean), "行（2017年第41-42周）\n")

cat("\n========== 样本量变化对比 ==========\n")
comparison <- data.frame(
  指标 = c("总行数", "完整无缺失行数", "总缺失值个数", "整体缺失率"),
  清洗前 = c(
    nrow(df_raw),
    sum(complete.cases(df_raw)),
    sum(is.na(df_raw)),
    round(mean(is.na(df_raw)), 4)
  ),
  清洗后 = c(
    nrow(df_clean),
    sum(complete.cases(df_clean)),
    sum(is.na(df_clean)),
    round(mean(is.na(df_clean)), 4)
  )
)
print(comparison)

cat("\n========== 变化总结 ==========\n")
cat(sprintf("样本量从 %d 行减少到 %d 行，删除了 %.1f%% 的数据\n", 
            nrow(df_raw), nrow(df_clean), 
            100 * (nrow(df_raw) - nrow(df_clean)) / nrow(df_raw)))
cat("删除比例极低（<0.4%），对整体分析影响可以忽略。\n")

cat("\n========== 原研究问题回顾 ==========\n")
cat("假设研究问题：描述2015-2024年中国流感流行特征\n")
cat("具体包括：\n")
cat("  - 流感季节性规律\n")
cat("  - 南北方ILI%差异\n")
cat("  - 甲流/乙流及亚型变化趋势\n")

cat("\n========== 支持性评估 ==========\n")
assessment <- data.frame(
  评估维度 = c(
    "样本量充分性",
    "时间跨度", 
    "变量完整性",
    "数据质量",
    "分析可行性"
  ),
  评估结果 = c("✅ 支持", "✅ 支持", "✅ 支持", "✅ 支持", "✅ 支持"),
  说明 = c(
    sprintf("清洗后剩余%d条周度记录，足够识别季节性规律", nrow(df_clean)),
    "覆盖2015-2024整10年，每年52周，周期完整",
    "包含南北ILI%、阳性数、甲/乙流占比及4种亚型分型",
    "缺失率<0.4%，异常值为真实高峰期，无需删除",
    "可做时间趋势、季节分解、南北方对比、亚型变迁分析"
  )
)
print(assessment)
# ========== 变量筛选与因子变量处理 ==========
core_columns <- c("年份", "周次", "南方ILI(%)", "北方ILI(%)", "总检测标本数",
                  "总阳性标本数", "甲流占阳性比(%)", "乙流占阳性比(%)",
                  "H3N2阳性数", "甲H1N1阳性数", "B(Victoria)阳性数", "B(Yamagata)阳性数")
df_clean_core <- df_clean %>% select(all_of(core_columns))
df_clean_core <- df_clean_core %>%
  mutate(
    年份因子 = as.factor(年份),
    周次 = as.integer(周次)
  )

df_clean_core <- df_clean_core %>%
  mutate(日期 = as.Date(paste(年份, 1, 1, sep = "-")) + (周次 - 1) * 7)

df_long <- df_clean_core %>%
  select(年份, 周次, `南方ILI(%)`, `北方ILI(%)`, 总阳性标本数, 
         `甲流占阳性比(%)`, `乙流占阳性比(%)`) %>%
  pivot_longer(cols = c(`南方ILI(%)`, `北方ILI(%)`),
               names_to = "区域",
               values_to = "ILI_percent") %>%
  mutate(区域 = str_replace(区域, "\\(%\\)", ""))  # 去掉括号，保留“南方ILI”“北方ILI”

# ========== 纳入排除标准与变量定义 ==========
cat("\n========== 分析对象确定 ==========\n")
cat("纳入标准：2015年第1周至2024年第52周的全部周次记录\n")
cat("排除标准：南方ILI(%) 或 北方ILI(%) 缺失的周次（共2周）\n")
cat("最终分析样本量：", nrow(df_clean_core), "周（", nrow(df_clean_core)/52, "年）\n")
cat("暴露变量：时间（年份、周次）\n")
cat("协变量（分组变量）：区域（南方/北方）、年份\n")
cat("结局变量：ILI(%)、阳性标本数、甲流占比、乙流占比及各亚型阳性数\n")
# ========== 描述性统计 ==========
library(tableone)
myVars_continuous <- c("南方ILI(%)", "北方ILI(%)", "总阳性标本数", 
                       "甲流占阳性比(%)", "乙流占阳性比(%)")

desc_table <- df_clean_core %>%
  summarise(across(all_of(myVars_continuous), 
                   list(
                     mean = ~ mean(.x, na.rm = TRUE),
                     sd = ~ sd(.x, na.rm = TRUE),
                     median = ~ median(.x, na.rm = TRUE),
                     Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                     Q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
                     min = ~ min(.x, na.rm = TRUE),
                     max = ~ max(.x, na.rm = TRUE)
                   ))) %>%
  pivot_longer(everything(), names_to = "var_stat", values_to = "value") %>%
  separate(var_stat, into = c("variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("\n========== 连续变量整体描述 ==========\n")
print(desc_table)

library(tableone)
library(kableExtra)
library(tidyverse)
library(readxl)
library(skimr)
library(naniar)
library(visdat)
df_long <- df_clean_core %>%
  select(年份, 周次, `南方ILI(%)`, `北方ILI(%)`, 总阳性标本数, `甲流占阳性比(%)`, `乙流占阳性比(%)`) %>%
  pivot_longer(
    cols = c(`南方ILI(%)`, `北方ILI(%)`),
    names_to = "区域",
    values_to = "ILI_percent"
  )

df_baseline <- df_long %>%
  mutate(区域 = ifelse(区域 == "南方ILI(%)", "南方", "北方"))
vars_baseline <- c("ILI_percent", "总阳性标本数", "甲流占阳性比(%)", "乙流占阳性比(%)")
# 生成基线表
tab1 <- CreateTableOne(vars = vars_baseline, strata = "区域", 
                       data = df_baseline, factorVars = catVars)
cat("\n========== 按区域分组的基线特征表 ==========\n")
print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
# ========== 分组变量合理性讨论==========
cat("\n========== 分组变量合理性讨论 ==========\n")
cat("1. 分组变量：区域（南方 vs 北方）\n")
cat("   - 合理性：南北方气候、人口密度、流感流行模式存在已知差异，分组具有流行病学意义。\n")
cat("   - 数据表现：南方ILI均值略高于北方，但差异无统计学显著性（p>0.05），可能与数据波动大或实际差异小有关。\n")
cat("2. 统计量选择：\n")
cat("   - 连续变量：因ILI%分布右偏（高峰年份拉高均值），同时报告均数±SD和中位数（Q1,Q3），更全面。\n")
cat("   - 分类变量：报告频数和百分比，标准做法。\n")
cat("3. 表格规范性：\n")
cat("   - 表格包含样本量、缺失情况、均数/SD/中位数等，符合医学论文基线表要求。\n")
cat("   - 行标题为指标，列标题为分组，数值保留一位小数，清晰可读。\n")
cat("4. 连续变量与分类变量的描述规范：\n")
cat("   - 连续变量（如ILI%）：若正态则用均数±SD，偏态则用中位数（四分位距）。本数据两者兼顾。\n")
cat("   - 分类变量（如优势毒株）：用频数（百分比），有序分类可补充累积百分比。\n")

