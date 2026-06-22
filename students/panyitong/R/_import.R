library(haven)
library(dplyr)

setwd("~/GitHub/JUMP-R-2026/students/panyitong/data/raw")

# 读取数据
demo <- read_xpt("DEMO_J.XPT")
bmx  <- read_xpt("BMX_J.XPT")
bpq  <- read_xpt("BPQ_J.XPT")
smq  <- read_xpt("SMQ_J.XPT")
bpx  <- read_xpt("BPX_J.XPT")
# 合并（保留所有变量）
merged_full <- demo %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(bpq, by = "SEQN") %>%
  left_join(smq, by = "SEQN") %>%
  left_join(bpx, by = "SEQN")

# 挑选需要的变量
merged <- merged_full %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3,DMDEDUC2, INDFMPIR, BMXBMI, BPQ020, BPXSY1, BPXSY2, BPXSY3,BPXDI1,BPXDI2,BPXDI3, SMQ020, SMQ040, WTMEC2YR, SDMVPSU, SDMVSTRA)  

# 保存
saveRDS(merged, "merged_raw.rds")

# 检查
dim(merged)
names(merged)

# 所有核心变量的缺失情况
need_vars <- c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "BMXBMI", "BPQ020","BPXSY1","BPXSY2", "BPXSY3","BPXDI1","BPXDI2","BPXDI3","SMQ020","SMQ040" , "DMDEDUC2", "INDFMPIR")  

colSums(is.na(merged[, need_vars]))

# 年龄范围
range(merged$RIDAGEYR, na.rm = FALSE)

# BMI 范围
range(merged$BMXBMI, na.rm = TRUE)

# 基于原始数据 merged 检查BMI 是否存在异常值

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

# PIR 异常值检查

# 查看 PIR 基本情况
cat(" PIR 基本情况 \n")
summary(merged$INDFMPIR)

# 查看 PIR = 5 的有多少人（这是截断，不是异常）
cat("\n PIR = 5（高收入截断）的人数 \n")
sum(merged$INDFMPIR == 5, na.rm = TRUE)

# 查看 PIR < 0 是否存在
cat("\n PIR < 0 的人数 \n")
sum(merged$INDFMPIR < 0, na.rm = TRUE)

# 箱线图查看 PIR 分布
boxplot(merged$INDFMPIR,  main = "原始数据 PIR 分布箱线图", ylab = "PIR", col = "lightgreen")

# 直方图查看分布形态
hist(merged$INDFMPIR, main = "PIR 分布直方图", xlab = "PIR", col = "lightblue", breaks = 20)

# BPXSY1 范围
range(merged$BPXSY1, na.rm = TRUE)
# BPXSY2 范围
range(merged$BPXSY2, na.rm = TRUE)
# BPXSY3 范围
range(merged$BPXSY3, na.rm = TRUE)
# BPXDI1 范围
range(merged$BPXDI1, na.rm = TRUE)
# BPXDI2 范围
range(merged$BPXDI2, na.rm = TRUE)
# BPXDI3 范围
range(merged$BPXDI3, na.rm = TRUE)

# ============================================================
# 血压异常值检查（基于原始数据，3次测量全包含）
# ============================================================

# 舒张压 = 0 的异常值
cat("\n=== 舒张压 = 0 的异常值 ===\n")
cat("BPXDI1 = 0 的人数:", sum(merged$BPXDI1 == 0, na.rm = TRUE), "\n")
cat("BPXDI2 = 0 的人数:", sum(merged$BPXDI2 == 0, na.rm = TRUE), "\n")
cat("BPXDI3 = 0 的人数:", sum(merged$BPXDI3 == 0, na.rm = TRUE), "\n")

# 收缩压异常（< 50 或 > 250）
cat("\n=== 收缩压异常值（<50 或 >250）===\n")
cat("BPXSY1 < 50 的人数:", sum(merged$BPXSY1 < 50, na.rm = TRUE), "\n")
cat("BPXSY1 > 250 的人数:", sum(merged$BPXSY1 > 250, na.rm = TRUE), "\n")
cat("BPXSY2 < 50 的人数:", sum(merged$BPXSY2 < 50, na.rm = TRUE), "\n")
cat("BPXSY2 > 250 的人数:", sum(merged$BPXSY2 > 250, na.rm = TRUE), "\n")
cat("BPXSY3 < 50 的人数:", sum(merged$BPXSY3 < 50, na.rm = TRUE), "\n")
cat("BPXSY3 > 250 的人数:", sum(merged$BPXSY3 > 250, na.rm = TRUE), "\n")

# 舒张压异常（< 30 或 > 120）
cat("\n=== 舒张压异常值（<30 或 >120）===\n")
cat("BPXDI1 < 30 的人数:", sum(merged$BPXDI1 < 30 & merged$BPXDI1 > 0, na.rm = TRUE), "\n")
cat("BPXDI1 > 120 的人数:", sum(merged$BPXDI1 > 120, na.rm = TRUE), "\n")
cat("BPXDI2 < 30 的人数:", sum(merged$BPXDI2 < 30 & merged$BPXDI2 > 0, na.rm = TRUE), "\n")
cat("BPXDI2 > 120 的人数:", sum(merged$BPXDI2 > 120, na.rm = TRUE), "\n")
cat("BPXDI3 < 30 的人数:", sum(merged$BPXDI3 < 30 & merged$BPXDI3 > 0, na.rm = TRUE), "\n")
cat("BPXDI3 > 120 的人数:", sum(merged$BPXDI3 > 120, na.rm = TRUE), "\n")

# 血压范围概况
cat("\n=== 血压范围概况 ===\n")
cat("收缩压 (BPXSY1) 范围:", range(merged$BPXSY1, na.rm = TRUE), "\n")
cat("收缩压 (BPXSY2) 范围:", range(merged$BPXSY2, na.rm = TRUE), "\n")
cat("收缩压 (BPXSY3) 范围:", range(merged$BPXSY3, na.rm = TRUE), "\n")
cat("舒张压 (BPXDI1) 范围:", range(merged$BPXDI1, na.rm = TRUE), "\n")
cat("舒张压 (BPXDI2) 范围:", range(merged$BPXDI2, na.rm = TRUE), "\n")
cat("舒张压 (BPXDI3) 范围:", range(merged$BPXDI3, na.rm = TRUE), "\n")

#BMI 分布概况
summary(merged$BMXBMI)
sd(merged$BMXBMI, na.rm = TRUE)

#年龄分布概况
summary(merged$RIDAGEYR)
sd(merged$RIDAGEYR, na.rm = TRUE)

#PIR分布概况
summary(merged$INDFMPIR)
sd(merged$INDFMPIR, na.rm = TRUE)

#分类变量分布情况
table(merged$SMQ020, useNA = "ifany")
table(merged$SMQ040, useNA = "ifany")
table(merged$BPQ020, useNA = "ifany")
table(merged$RIAGENDR, useNA = "ifany")
table(merged$RIDRETH3, useNA = "ifany")
table(merged$DMDEDUC2, useNA = "ifany")

