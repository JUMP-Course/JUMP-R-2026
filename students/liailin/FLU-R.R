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

# ============================================================
# 核心图形绘制（变量分布、组间比较、趋势、构成）
# ============================================================

# 加载绘图包
install.packages(c("patchwork"))
library(ggplot2)
library(patchwork)  # 拼图
library(RColorBrewer)  # 配色

# 设置图形主题（统一风格）
theme_set(theme_minimal(base_size = 12) +
            theme(legend.position = "top",
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  panel.grid.minor = element_blank()))

# 图1. 变量分布图（直方图 + 密度曲线）
# 1.1 南方ILI分布
p1_hist <- ggplot(df_clean_core, aes(x = `南方ILI(%)`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "#E74C3C", alpha = 0.6, color = "white") +
  geom_density(color = "#C0392B", linewidth = 1.2) +
  labs(title = "A. 南方ILI(%)分布",
       x = "南方ILI(%)", y = "密度") +
  annotate("text", x = 8, y = 0.6, 
           label = sprintf("均值=%.2f\n中位数=%.2f", 
                           mean(df_clean_core$`南方ILI(%)`, na.rm=TRUE),
                           median(df_clean_core$`南方ILI(%)`, na.rm=TRUE)),
           hjust = 0, size = 3.5)
# 1.2 北方ILI分布
p2_hist <- ggplot(df_clean_core, aes(x = `北方ILI(%)`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "#3498DB", alpha = 0.6, color = "white") +
  geom_density(color = "#2980B9", linewidth = 1.2) +
  labs(title = "B. 北方ILI(%)分布",
       x = "北方ILI(%)", y = "密度") +
  annotate("text", x = 8, y = 0.6, 
           label = sprintf("均值=%.2f\n中位数=%.2f", 
                           mean(df_clean_core$`北方ILI(%)`, na.rm=TRUE),
                           median(df_clean_core$`北方ILI(%)`, na.rm=TRUE)),
           hjust = 0, size = 3.5)
# 1.3 总阳性标本数分布（对数变换展示）
p3_hist <- ggplot(df_clean_core, aes(x = 总阳性标本数)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "#2ECC71", alpha = 0.6, color = "white") +
  geom_density(color = "#27AE60", linewidth = 1.2) +
  labs(title = "C. 总阳性标本数分布",
       x = "总阳性标本数", y = "密度") +
  annotate("text", x = 3500, y = 0.0015, 
           label = sprintf("均值=%.0f\n中位数=%.0f", 
                           mean(df_clean_core$总阳性标本数, na.rm=TRUE),
                           median(df_clean_core$总阳性标本数, na.rm=TRUE)),
           hjust = 0, size = 3.5)
# 拼图
combined_hist <- (p1_hist | p2_hist) / p3_hist
print(combined_hist)

# 图2. 组间比较图（南北方ILI%箱线图）
# 创建长格式数据用于组间比较
df_compare <- df_clean_core %>%
  select(`南方ILI(%)`, `北方ILI(%)`) %>%
  pivot_longer(everything(), names_to = "区域", values_to = "ILI_percent") %>%
  mutate(区域 = str_replace(区域, "\\(%\\)", ""))

p_box <- ggplot(df_compare, aes(x = 区域, y = ILI_percent, fill = 区域)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 0.5) +
  scale_fill_manual(values = c("南方ILI" = "#E74C3C", "北方ILI" = "#3498DB")) +
  labs(title = "南北方ILI(%)比较",
       x = "区域", y = "ILI(%)") +
  theme(legend.position = "none") +
  # 添加统计标注
  annotate("text", x = 1, y = 10.5, 
           label = sprintf("n=518\n均值=%.2f", 
                           mean(df_clean_core$`南方ILI(%)`, na.rm=TRUE)),
           hjust = 0.5, size = 3.5) +
  annotate("text", x = 2, y = 10.5, 
           label = sprintf("n=518\n均值=%.2f", 
                           mean(df_clean_core$`北方ILI(%)`, na.rm=TRUE)),
           hjust = 0.5, size = 3.5)

print(p_box)
# 图3. 时间趋势图（2015-2024年ILI%变化）
# 3.1 南北方ILI%时间趋势（双线图）
p_trend <- df_clean_core %>%
  ggplot(aes(x = 日期)) +
  geom_line(aes(y = `南方ILI(%)`, color = "南方"), linewidth = 0.8) +
  geom_line(aes(y = `北方ILI(%)`, color = "北方"), linewidth = 0.8) +
  scale_color_manual(values = c("南方" = "#E74C3C", "北方" = "#3498DB")) +
  labs(title = "2015-2024年南北方ILI(%)变化趋势",
       x = "年份", y = "ILI(%)",
       color = "区域") +
  theme(legend.position = "top") +
  # 标注2024年高峰
  annotate("text", x = as.Date("2024-01-01"), y = 10.5, 
           label = "2024年高峰", size = 3.5, color = "#C0392B") +
  annotate("point", x = as.Date("2024-01-01"), 
           y = max(df_clean_core$`南方ILI(%)`, na.rm=TRUE), 
           color = "#C0392B", size = 3)

print(p_trend)
# 3.2 各亚型阳性数趋势（堆积面积图）
p_subtype <- df_clean_core %>%
  select(日期, H3N2阳性数, `甲H1N1阳性数`, `B(Victoria)阳性数`, `B(Yamagata)阳性数`) %>%
  pivot_longer(-日期, names_to = "亚型", values_to = "阳性数") %>%
  ggplot(aes(x = 日期, y = 阳性数, fill = 亚型)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "各亚型阳性数变化趋势（堆积面积图）",
       x = "年份", y = "阳性数",
       fill = "亚型") +
  theme(legend.position = "top")

print(p_subtype)
# 图4. 构成图（甲流/乙流占比变化）
# 4.1 甲流/乙流占比堆积条形图（按年份汇总）
p_annual <- df_clean_core %>%
  group_by(年份) %>%
  summarise(
    甲流占比 = mean(`甲流占阳性比(%)`, na.rm = TRUE),
    乙流占比 = mean(`乙流占阳性比(%)`, na.rm = TRUE)
  ) %>%
  pivot_longer(-年份, names_to = "型别", values_to = "占比") %>%
  ggplot(aes(x = as.factor(年份), y = 占比, fill = 型别)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("甲流占比" = "#E74C3C", "乙流占比" = "#3498DB")) +
  labs(title = "各年度甲流/乙流占比变化",
       x = "年份", y = "构成比(%)",
       fill = "型别") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent_format())

print(p_annual)
# 4.2 甲流/乙流占比时间趋势折线图
p_ab_line <- df_clean_core %>%
  select(日期, `甲流占阳性比(%)`, `乙流占阳性比(%)`) %>%
  pivot_longer(-日期, names_to = "型别", values_to = "占比") %>%
  ggplot(aes(x = 日期, y = 占比, color = 型别)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("甲流占阳性比(%)" = "#E74C3C", 
                                "乙流占阳性比(%)" = "#3498DB")) +
  labs(title = "甲流/乙流占阳性比时间趋势",
       x = "年份", y = "占比(%)",
       color = "型别") +
  theme(legend.position = "top")

print(p_ab_line)
# 加载统计包
library(tidyverse)
library(broom)  # 整洁模型输出

# ============================================================
# 一、组间比较：南方 vs 北方 ILI%
# ============================================================

cat("\n========== 1. 南北方 ILI% 组间比较 ==========\n")

# 创建长格式数据（用于比较）
df_compare <- df_clean_core %>%
  select(`南方ILI(%)`, `北方ILI(%)`) %>%
  pivot_longer(everything(), names_to = "区域", values_to = "ILI_percent") %>%
  mutate(区域 = str_replace(区域, "\\(%\\)", ""))

# 1.1 正态性检验（决定用 t检验还是 Wilcoxon 检验）
cat("\n--- 正态性检验（Shapiro-Wilk）---\n")
south_data <- df_clean_core$`南方ILI(%)` %>% na.omit()
north_data <- df_clean_core$`北方ILI(%)` %>% na.omit()

shapiro_south <- shapiro.test(south_data)
shapiro_north <- shapiro.test(north_data)

cat("南方 ILI% W =", round(shapiro_south$statistic, 4), 
    "p =", format(shapiro_south$p.value, scientific = TRUE, digits = 4), "\n")
cat("北方 ILI% W =", round(shapiro_north$statistic, 4), 
    "p =", format(shapiro_north$p.value, scientific = TRUE, digits = 4), "\n")

# 正态性判断：p < 0.05 表示不服从正态分布
if (shapiro_south$p.value < 0.05 | shapiro_north$p.value < 0.05) {
  cat("数据不服从正态分布（p < 0.05），使用 Wilcoxon 秩和检验\n")
  
  # Wilcoxon 秩和检验
  test_result <- wilcox.test(south_data, north_data, paired = FALSE)
  cat("\n--- Wilcoxon 秩和检验结果 ---\n")
  cat("W =", test_result$statistic, "\n")
  cat("p =", format(test_result$p.value, scientific = TRUE, digits = 4), "\n")
  cat("结论：")
  if (test_result$p.value < 0.05) {
    cat("南北方 ILI% 差异具有统计学显著性（p < 0.05）\n")
  } else {
    cat("南北方 ILI% 差异无统计学显著性（p ≥ 0.05）\n")
  }
} else {
  # 方差齐性检验
  var_test <- var.test(south_data, north_data)
  cat("方差齐性检验 p =", format(var_test$p.value, scientific = TRUE, digits = 4), "\n")
  
  # t检验（根据方差齐性选择）
  t_test <- t.test(south_data, north_data, var.equal = (var_test$p.value > 0.05))
  cat("\n--- t检验结果 ---\n")
  cat("t =", round(t_test$statistic, 4), "\n")
  cat("df =", round(t_test$parameter, 2), "\n")
  cat("p =", format(t_test$p.value, scientific = TRUE, digits = 4), "\n")
  cat("95% CI = [", round(t_test$conf.int[1], 4), ", ", round(t_test$conf.int[2], 4), "]\n")
}

# 1.2 效应量计算（Cohen's d 或 r）
cat("\n--- 效应量（Effect Size）---\n")
# 使用 Z 值计算 r (适用于 Wilcoxon)
# 此处使用秩和检验的 Z 值计算效应量 r = Z / sqrt(N)
# 因无法直接从 wilcox.test 获取 Z，使用近似方法
n_total <- length(south_data) + length(north_data)
# 使用均数差/合并标准差作为效应量近似
pooled_sd <- sqrt(((length(south_data)-1)*var(south_data) + 
                     (length(north_data)-1)*var(north_data)) / 
                    (length(south_data) + length(north_data) - 2))
cohens_d <- abs(mean(south_data) - mean(north_data)) / pooled_sd
cat("Cohen's d =", round(cohens_d, 4), "\n")
cat("效应量解读：d < 0.2 为极小，0.2-0.5 为小，0.5-0.8 为中，>0.8 为大\n")

# ============================================================
# 二、时间趋势分析：ILI% 随年份的变化趋势
# ============================================================

cat("\n========== 2. 时间趋势分析 ==========\n")

# 2.1 南方 ILI% 时间趋势（线性回归）
cat("\n--- 南方 ILI% 时间趋势 ---\n")
model_south <- lm(`南方ILI(%)` ~ 年份, data = df_clean_core)
summary_south <- summary(model_south)
cat("回归方程：南方ILI% =", round(coef(model_south)[1], 4), 
    "+", round(coef(model_south)[2], 4), "× 年份\n")
cat("R² =", round(summary_south$r.squared, 4), "\n")
cat("年份系数 =", round(coef(model_south)[2], 4), 
    "，p =", format(summary_south$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")
if (summary_south$coefficients[2, 4] < 0.05) {
  cat("结论：南方 ILI% 随时间有显著的线性趋势（p < 0.05）\n")
} else {
  cat("结论：南方 ILI% 随时间无显著线性趋势（p ≥ 0.05）\n")
}

# 2.2 北方 ILI% 时间趋势（线性回归）
cat("\n--- 北方 ILI% 时间趋势 ---\n")
model_north <- lm(`北方ILI(%)` ~ 年份, data = df_clean_core)
summary_north <- summary(model_north)
cat("回归方程：北方ILI% =", round(coef(model_north)[1], 4), 
    "+", round(coef(model_north)[2], 4), "× 年份\n")
cat("R² =", round(summary_north$r.squared, 4), "\n")
cat("年份系数 =", round(coef(model_north)[2], 4), 
    "，p =", format(summary_north$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")
if (summary_north$coefficients[2, 4] < 0.05) {
  cat("结论：北方 ILI% 随时间有显著的线性趋势（p < 0.05）\n")
} else {
  cat("结论：北方 ILI% 随时间无显著线性趋势（p ≥ 0.05）\n")
}

# 2.3 模型诊断
cat("\n--- 模型诊断 ---\n")
par(mfrow = c(2, 2))
plot(model_south)
cat("南方模型 Q-Q 图显示残差是否近似正态分布\n")
par(mfrow = c(1, 1))

# ============================================================
# 三、季度效应分析（季节性模式）
# ============================================================

cat("\n========== 3. 季度效应分析 ==========\n")

# 创建季度变量
df_clean_core <- df_clean_core %>%
  mutate(
    季度 = case_when(
      周次 %in% c(1:13) ~ "Q1",
      周次 %in% c(14:26) ~ "Q2",
      周次 %in% c(27:39) ~ "Q3",
      周次 %in% c(40:53) ~ "Q4"
    ),
    季度 = factor(季度, levels = c("Q1", "Q2", "Q3", "Q4"))
  )

# 按季度汇总描述
cat("\n--- 各季度南方 ILI% 描述 ---\n")
df_clean_core %>%
  group_by(季度) %>%
  summarise(
    n = n(),
    均值 = round(mean(`南方ILI(%)`, na.rm = TRUE), 4),
    中位数 = round(median(`南方ILI(%)`, na.rm = TRUE), 4),
    标准差 = round(sd(`南方ILI(%)`, na.rm = TRUE), 4),
    最小值 = round(min(`南方ILI(%)`, na.rm = TRUE), 4),
    最大值 = round(max(`南方ILI(%)`, na.rm = TRUE), 4)
  ) %>%
  print()

# 方差分析（检验各季度间差异）
cat("\n--- 季度间差异 ANOVA 检验 ---\n")
aov_south <- aov(`南方ILI(%)` ~ 季度, data = df_clean_core)
summary_aov <- summary(aov_south)
print(summary_aov)
if (summary_aov[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("结论：不同季度间南方 ILI% 差异具有统计学显著性（p < 0.05）\n")
  cat("说明：流感活动存在明显的季节性模式\n")
} else {
  cat("结论：不同季度间南方 ILI% 差异无统计学显著性（p ≥ 0.05）\n")
}

# ============================================================
# 四、结果汇总表（用于报告）
# ============================================================

cat("\n========== 4. 主分析结果汇总 ==========\n")

# 创建汇总表
results_summary <- data.frame(
  分析项 = c(
    "南北方 ILI% 比较 (Wilcoxon)",
    "南方 ILI% 时间趋势 (线性回归)",
    "北方 ILI% 时间趋势 (线性回归)",
    "季度间差异 (ANOVA)"
  ),
  统计量 = c(
    paste0("W = ", round(wilcox.test(south_data, north_data)$statistic, 2)),
    paste0("β = ", round(coef(model_south)[2], 4)),
    paste0("β = ", round(coef(model_north)[2], 4)),
    paste0("F = ", round(summary_aov[[1]]$`F value`[1], 4))
  ),
  P值 = c(
    format(wilcox.test(south_data, north_data)$p.value, scientific = TRUE, digits = 4),
    format(summary_south$coefficients[2, 4], scientific = TRUE, digits = 4),
    format(summary_north$coefficients[2, 4], scientific = TRUE, digits = 4),
    format(summary_aov[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 4)
  ),
  结论 = c(
    ifelse(wilcox.test(south_data, north_data)$p.value < 0.05, "有显著差异", "无显著差异"),
    ifelse(summary_south$coefficients[2, 4] < 0.05, "趋势显著", "趋势不显著"),
    ifelse(summary_north$coefficients[2, 4] < 0.05, "趋势显著", "趋势不显著"),
    ifelse(summary_aov[[1]]$`Pr(>F)`[1] < 0.05, "有季节性差异", "无季节性差异")
  )
)
print(results_summary)

# 补充分析：稳健性 + 亚组差异 + 模型诊断
install.packages(c("car",type = "binary"))
library(boot)      # Bootstrap 重抽样
library(car)       # 模型诊断
library(lmtest)    # 异方差/自相关检验
library(broom)     # 整洁模型输出

# ============================================================
# 一、稳健性检验（Bootstrap 验证）
# ============================================================

cat("\n========== 1. 稳健性检验 ==========\n")
cat("【目的】验证时间趋势结果是否受抽样波动影响\n")

# 1.1 定义 Bootstrap 函数
set.seed(123)  # 确保结果可重复

# 南方 ILI% 的 Bootstrap 函数
boot_south <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(`南方ILI(%)` ~ 年份, data = d)
  return(coef(m)[2])  # 返回年份系数
}

# 北方 ILI% 的 Bootstrap 函数
boot_north <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(`北方ILI(%)` ~ 年份, data = d)
  return(coef(m)[2])
}

# 执行 Bootstrap（各1000次）
boot_south_res <- boot(df_clean_core, boot_south, R = 1000)
boot_north_res <- boot(df_clean_core, boot_north, R = 1000)

# 输出结果
cat("\n--- 南方 ILI% 时间趋势 Bootstrap ---\n")
cat("原始 β 系数:", round(boot_south_res$t0, 4), "\n")
cat("Bootstrap 标准误:", round(sd(boot_south_res$t), 4), "\n")
cat("Bootstrap 95% CI: [", 
    round(boot.ci(boot_south_res, type = "perc")$percent[4], 4), 
    ", ", 
    round(boot.ci(boot_south_res, type = "perc")$percent[5], 4), 
    "]\n")

cat("\n--- 北方 ILI% 时间趋势 Bootstrap ---\n")
cat("原始 β 系数:", round(boot_north_res$t0, 4), "\n")
cat("Bootstrap 标准误:", round(sd(boot_north_res$t), 4), "\n")
cat("Bootstrap 95% CI: [", 
    round(boot.ci(boot_north_res, type = "perc")$percent[4], 4), 
    ", ", 
    round(boot.ci(boot_north_res, type = "perc")$percent[5], 4), 
    "]\n")

# 判断稳健性
south_ci <- boot.ci(boot_south_res, type = "perc")$percent[4:5]
north_ci <- boot.ci(boot_north_res, type = "perc")$percent[4:5]

cat("\n【稳健性判断】\n")
cat("南方: 95% CI", ifelse(south_ci[1] * south_ci[2] > 0, "不包含0 ✓", "包含0 ✗"), 
    "→ 趋势", ifelse(south_ci[1] * south_ci[2] > 0, "稳健", "不稳定"), "\n")
cat("北方: 95% CI", ifelse(north_ci[1] * north_ci[2] > 0, "不包含0 ✓", "包含0 ✗"), 
    "→ 趋势", ifelse(north_ci[1] * north_ci[2] > 0, "稳健", "不稳定"), "\n")

# ============================================================
# 二、敏感性分析（排除异常年份2024年）
# ============================================================

cat("\n========== 2. 敏感性分析 ==========\n")
cat("【目的】排除2024年异常高峰后，趋势是否仍然存在？\n")

# 排除2024年
df_sens <- df_clean_core %>% filter(年份 != 2024)

cat("原始数据:", nrow(df_clean_core), "行\n")
cat("排除2024年后:", nrow(df_sens), "行（删除", 
    nrow(df_clean_core) - nrow(df_sens), "行）\n")

# 重新拟合模型
model_south_sens <- lm(`南方ILI(%)` ~ 年份, data = df_sens)
model_north_sens <- lm(`北方ILI(%)` ~ 年份, data = df_sens)

# 对比结果
cat("\n--- 南方 ILI% 趋势对比 ---\n")
cat("含2024年: β =", round(coef(model_south)[2], 4), 
    ", p =", format(summary(model_south)$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")
cat("排除2024年: β =", round(coef(model_south_sens)[2], 4), 
    ", p =", format(summary(model_south_sens)$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")

cat("\n--- 北方 ILI% 趋势对比 ---\n")
cat("含2024年: β =", round(coef(model_north)[2], 4), 
    ", p =", format(summary(model_north)$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")
cat("排除2024年: β =", round(coef(model_north_sens)[2], 4), 
    ", p =", format(summary(model_north_sens)$coefficients[2, 4], scientific = TRUE, digits = 4), "\n")

# 判断敏感性
south_change <- abs(coef(model_south)[2] - coef(model_south_sens)[2])
north_change <- abs(coef(model_north)[2] - coef(model_north_sens)[2])

cat("\n【敏感性判断】\n")
cat("南方 β 变化:", round(south_change, 4), 
    ifelse(south_change < 0.02, " → 变化小，结果稳健 ✓", " → 变化大，结果敏感 ⚠️"), "\n")
cat("北方 β 变化:", round(north_change, 4), 
    ifelse(north_change < 0.02, " → 变化小，结果稳健 ✓", " → 变化大，结果敏感 ⚠️"), "\n")

# ============================================================
# 三、亚组分析（高流行年 vs 低流行年）
# ============================================================

cat("\n========== 3. 亚组分析 ==========\n")
cat("【目的】不同流行强度年份，甲流占比是否有差异？\n")

# 计算各年份平均 ILI%
yearly_avg <- df_clean_core %>%
  group_by(年份) %>%
  summarise(年均ILI = mean(`南方ILI(%)`, na.rm = TRUE))

cat("\n各年份平均 ILI%:\n")
print(yearly_avg)

# 定义高流行年 vs 低流行年
high_years <- yearly_avg %>% arrange(desc(年均ILI)) %>% head(3) %>% pull(年份)
low_years <- yearly_avg %>% arrange(年均ILI) %>% head(3) %>% pull(年份)

cat("\n高流行年（前3）:", paste(high_years, collapse = ", "), "\n")
cat("低流行年（后3）:", paste(low_years, collapse = ", "), "\n")

# 标记流行强度
df_clean_core <- df_clean_core %>%
  mutate(
    流行强度 = case_when(
      年份 %in% high_years ~ "高流行",
      年份 %in% low_years ~ "低流行",
      TRUE ~ "中等"
    ),
    流行强度 = factor(流行强度, levels = c("低流行", "中等", "高流行"))
  )

# 比较不同流行强度的甲流占比
cat("\n--- 不同流行强度甲流占比描述 ---\n")
subgroup_summary <- df_clean_core %>%
  group_by(流行强度) %>%
  summarise(
    n_周次 = n(),
    甲流占比_均值 = round(mean(`甲流占阳性比(%)`, na.rm = TRUE), 2),
    甲流占比_中位数 = round(median(`甲流占阳性比(%)`, na.rm = TRUE), 2),
    甲流占比_SD = round(sd(`甲流占阳性比(%)`, na.rm = TRUE), 2)
  )
print(subgroup_summary)

# 统计检验（高 vs 低）
high_data <- df_clean_core %>% filter(流行强度 == "高流行") %>% pull(`甲流占阳性比(%)`)
low_data <- df_clean_core %>% filter(流行强度 == "低流行") %>% pull(`甲流占阳性比(%)`)

if (length(high_data) > 0 & length(low_data) > 0) {
  subgroup_test <- wilcox.test(high_data, low_data)
  cat("\n--- 高流行 vs 低流行 甲流占比比较 ---\n")
  cat("W =", subgroup_test$statistic, "\n")
  cat("p =", format(subgroup_test$p.value, scientific = TRUE, digits = 4), "\n")
  if (subgroup_test$p.value < 0.05) {
    cat("结论：高流行年甲流占比显著高于低流行年 ✓\n")
  } else {
    cat("结论：高流行年与低流行年甲流占比无显著差异\n")
  }
}

# ============================================================
# 四、模型诊断（残差检验）
# ============================================================

cat("\n========== 4. 模型诊断 ==========\n")
cat("【目的】检查线性回归模型是否满足基本假设\n")

# 4.1 Durbin-Watson 检验（自相关）
cat("\n--- Durbin-Watson 自相关检验 ---\n")
dw_south <- dwtest(model_south)
dw_north <- dwtest(model_north)

cat("南方模型: DW =", round(dw_south$statistic, 4), 
    ", p =", format(dw_south$p.value, scientific = TRUE, digits = 4), "\n")
cat("北方模型: DW =", round(dw_north$statistic, 4), 
    ", p =", format(dw_north$p.value, scientific = TRUE, digits = 4), "\n")

cat("\n【自相关判断】DW ≈ 2 表示无自相关\n")
cat("南方 DW =", round(dw_south$statistic, 4), 
    ifelse(abs(dw_south$statistic - 2) < 0.5, " → 可接受 ✓", " → 需注意 ⚠️"), "\n")
cat("北方 DW =", round(dw_north$statistic, 4), 
    ifelse(abs(dw_north$statistic - 2) < 0.5, " → 可接受 ✓", " → 需注意 ⚠️"), "\n")

# 4.2 Breusch-Pagan 检验（异方差）
cat("\n--- Breusch-Pagan 异方差检验 ---\n")
bp_south <- bptest(model_south)
bp_north <- bptest(model_north)

cat("南方模型: BP =", round(bp_south$statistic, 4), 
    ", p =", format(bp_south$p.value, scientific = TRUE, digits = 4), "\n")
cat("北方模型: BP =", round(bp_north$statistic, 4), 
    ", p =", format(bp_north$p.value, scientific = TRUE, digits = 4), "\n")

cat("\n【异方差判断】p > 0.05 表示方差齐性\n")
cat("南方: p =", format(bp_south$p.value, scientific = TRUE, digits = 4),
    ifelse(bp_south$p.value > 0.05, " → 方差齐性 ✓", " → 存在异方差 ⚠️"), "\n")
cat("北方: p =", format(bp_north$p.value, scientific = TRUE, digits = 4),
    ifelse(bp_north$p.value > 0.05, " → 方差齐性 ✓", " → 存在异方差 ⚠️"), "\n")

# 4.3 绘制模型诊断图
cat("\n--- 模型诊断图（南方模型）---\n")
par(mfrow = c(2, 2))
plot(model_south)
par(mfrow = c(1, 1))

# ============================================================
# 五、补充分析结果汇总表
# ============================================================

cat("\n========== 5. 补充分析结果汇总 ==========\n")

supplement_summary <- data.frame(
  分析类别 = c(
    "稳健性(Bootstrap)",
    "稳健性(Bootstrap)",
    "敏感性(排除2024)",
    "敏感性(排除2024)",
    "亚组分析(流行强度)",
    "模型诊断(自相关)",
    "模型诊断(异方差)"
  ),
  分析对象 = c(
    "南方趋势", "北方趋势",
    "南方趋势", "北方趋势",
    "高vs低流行年",
    "南方模型", "南方模型"
  ),
  关键结果 = c(
    paste0("95% CI [", round(south_ci[1], 4), ", ", round(south_ci[2], 4), "]"),
    paste0("95% CI [", round(north_ci[1], 4), ", ", round(north_ci[2], 4), "]"),
    paste0("β变化 = ", round(south_change, 4)),
    paste0("β变化 = ", round(north_change, 4)),
    paste0("p = ", format(subgroup_test$p.value, scientific = TRUE, digits = 4)),
    paste0("DW = ", round(dw_south$statistic, 4), ", p = ", format(dw_south$p.value, scientific = TRUE, digits = 4)),
    paste0("BP = ", round(bp_south$statistic, 4), ", p = ", format(bp_south$p.value, scientific = TRUE, digits = 4))
  ),
  结论 = c(
    ifelse(south_ci[1] * south_ci[2] > 0, "趋势稳健 ✓", "趋势不稳定 ✗"),
    ifelse(north_ci[1] * north_ci[2] > 0, "趋势稳健 ✓", "趋势不稳定 ✗"),
    ifelse(south_change < 0.02, "结果稳健 ✓", "结果敏感 ⚠️"),
    ifelse(north_change < 0.02, "结果稳健 ✓", "结果敏感 ⚠️"),
    ifelse(subgroup_test$p.value < 0.05, "有显著差异 ✓", "无显著差异"),
    ifelse(abs(dw_south$statistic - 2) < 0.5, "可接受 ✓", "需注意 ⚠️"),
    ifelse(bp_south$p.value > 0.05, "可接受 ✓", "需注意 ⚠️")
  )
)

print(supplement_summary)

# ============================================================
# 六、补充分析可视化
# ============================================================

# 6.1 Bootstrap 分布图
library(ggplot2)
boot_df <- data.frame(β系数 = boot_south_res$t)

p_boot <- ggplot(boot_df, aes(x = β系数)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6, color = "white") +
  geom_vline(xintercept = boot_south_res$t0, color = "red", linewidth = 1.2) +
  geom_vline(xintercept = south_ci[1], color = "blue", linetype = "dashed") +
  geom_vline(xintercept = south_ci[2], color = "blue", linetype = "dashed") +
  annotate("text", x = boot_south_res$t0 + 0.01, y = 80, 
           label = paste("原始 β =", round(boot_south_res$t0, 4)), 
           color = "red", size = 4, hjust = 0) +
  annotate("text", x = south_ci[1] - 0.01, y = 60, 
           label = "95% CI", color = "blue", size = 3.5, hjust = 1) +
  labs(title = "Bootstrap 抽样分布（南方 ILI% 趋势系数）",
       x = "年份系数 (β)", y = "频数",
       subtitle = "红色实线 = 原始估计，蓝色虚线 = 95% 置信区间") +
  theme_minimal()

print(p_boot)

cat("\n补充分析完成！\n")

