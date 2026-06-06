# 01_import.R（带权重版本）
library(haven)
library(dplyr)

# 读取数据
demo <- read_xpt("DEMO_J.XPT")
bmx  <- read_xpt("BMX_J.XPT")
bpq  <- read_xpt("BPQ_J.XPT")
smq  <- read_xpt("SMQ_J.XPT")

# 合并（保留所有变量）
merged_full <- demo %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(bpq, by = "SEQN") %>%
  left_join(smq, by = "SEQN")

# 挑选需要的变量（包括权重）
merged <- merged_full %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3,BMXBMI,BPQ020,SMQ020,WTMEC2YR, SDMVPSU, SDMVSTRA)   # ← 加上权重和抽样变量

# 保存
saveRDS(merged, "merged_raw.rds")

# 检查
dim(merged)
names(merged)

# 只看你需要的变量的缺失情况
need_vars <- c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "BMXBMI", "BPQ020", "SMQ020")

colSums(is.na(merged[, need_vars]))

# 年龄范围
range(merged$RIDAGEYR, na.rm = FALSE)

# BMI 范围
range(merged$BMXBMI, na.rm = TRUE)

# 基于原始数据 merged 计算 BMI 的 IQR 异常值边界

# 计算四分位数
Q1 <- quantile(merged$BMXBMI, 0.25, na.rm = TRUE)
Q3 <- quantile(merged$BMXBMI, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# 计算异常值边界（1.5倍 IQR 规则）
lower_bound<- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# 打印
cat("Q1 =", Q1, "\n")
cat("Q3 =", Q3, "\n")
cat("IQR =", IQR, "\n")
cat("异常值下界 =", lower_bound, "\n")
cat("异常值上界 =", upper_bound, "\n")

# 统计异常值数量
low_outliers <- sum(merged$BMXBMI < lower_bound, na.rm = TRUE)
high_outliers <- sum(merged$BMXBMI > upper_bound, na.rm = TRUE)

cat("低于下界的异常值数量：", low_outliers, "\n")
cat("高于上界的异常值数量：", high_outliers, "\n")

# 查看异常值的具体数值
high_bmi_values <- merged$BMXBMI[merged$BMXBMI > upper_bound]
high_bmi_values <- sort(high_bmi_values[!is.na(high_bmi_values)])

cat("\n高于上界的 BMI 值：\n")
print(high_bmi_values)

# 查看最极端的几个异常值（BMI > 64）的身高体重
cat("\n极端值核查（BMI > 64）：\n")
merged_full %>%
  filter(BMXBMI > 64) %>%
  select(SEQN, BMXBMI, RIDAGEYR, RIAGENDR, BMXWT, BMXHT) %>%
  print()

# 画箱线图
boxplot(merged$BMXBMI, main = "原始数据 BMI 分布箱线图", ylab = "BMI", col = "lightblue")

#BMI 分布概况
summary(merged$BMXBMI)
sd(merged$BMXBMI, na.rm = TRUE)

#年龄分布概况
summary(merged$RIDAGEYR)
sd(merged$RIDAGEYR, na.rm = TRUE)

#分类变量分布情况
table(merged$SMQ020, useNA = "ifany")
table(merged$BPQ020, useNA = "ifany")
table(merged$RIAGENDR, useNA = "ifany")
table(merged$RIDRETH3, useNA = "ifany")