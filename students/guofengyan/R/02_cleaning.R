# ============================================
# TCGA-PAAD胰腺癌预后预测模型
# ============================================

# ---- 加载工具包 ----
library(dplyr)       # 数据清洗
library(survival)    # 生存分析核心包：Cox回归、生存结局
library(survminer)   # 生存分析画图辅助
library(rms)         # 列线图、校准曲线、C-index
library(timeROC)     # 时间依赖ROC
library(broom)       # 整理回归结果为干净表格
library(ggplot2)     # 森林图、统计图

# ============================================================
# 第一部分：数据导入
# ============================================================

# ---- 1.1 设置工作目录 ----
setwd("C:/Users/86156/Desktop/")

# ---- 1.2 读取原始数据 ----
clin_raw <- read.delim("paad_tcga_gdc_clinical_data.tsv")  # 临床数据
mut_raw  <- read.delim("mutations.txt")                     # 基因突变数据

# ---- 1.3 查看原始数据情况 ----
cat("原始数据规模：\n")
cat("  临床数据：", nrow(clin_raw), "行 ×", ncol(clin_raw), "列\n")
cat("  突变数据：", nrow(mut_raw),  "行 ×", ncol(mut_raw),  "列\n")


# ============================================================
# 第二部分：数据清洗整理
# ============================================================

# ---- 2.1 临床数据整理 ----
df_clin <- clin_raw %>%
  mutate(
    ID     = substr(Patient.ID, 1, 12),
    time   = as.numeric(Overall.Survival..Months.),
    status = as.numeric(Overall.Survival.Status == "1:DECEASED"),
    Age    = as.numeric(Diagnosis.Age),
    Sex    = factor(Sex, levels = c("Male", "Female")),
    N      = AJCC.Pathologic.N.Stage
  ) %>%
  select(ID, time, status, Age, Sex, N)  # 保留分析所需列

# ---- 2.2 临床数据清洗 ----
df_clin_clean <- df_clin %>%
  filter(
    !is.na(time) & !is.na(Age) & !is.na(status) & !is.na(Sex) & !is.na(N),
    Age >= 18 & Age <= 100,
    time >= 0,
    N != "" & N != "NX"
  ) %>%
  mutate(
    # 合并N1b到N1
    N = factor(ifelse(N == "N1b", "N1", N), 
               levels = c("N0", "N1"))
  )

cat("\n临床数据清洗后：", nrow(df_clin_clean), "例\n")

# ---- 2.3 突变数据整理 ----
mut <- mut_raw %>%
  mutate(
    ID   = substr(SAMPLE_ID, 1, 12),
    KRAS = ifelse(KRAS == "WT", "Wild-type", "Mutant")
  )

# ---- 2.4 突变数据去重（同一患者只要有突变即标记为Mutant）----
kras <- aggregate(KRAS ~ ID, mut, FUN = function(x) {
  ifelse(any(x != "Wild-type"), "Mutant", "Wild-type")
})

# ---- 2.5 突变数据清洗 ----
kras_clean <- kras %>%
  filter(!is.na(KRAS)) %>%
  mutate(
    KRAS = factor(KRAS, levels = c("Wild-type", "Mutant"))
  )

cat("突变数据清洗后：", nrow(kras_clean), "例\n")
cat("\nKRAS突变分布：\n"); print(table(kras_clean$KRAS))

# ---- 2.6 合并两份数据（仅保留双方匹配的患者）----
df <- merge(df_clin_clean, kras_clean, by = "ID", all = FALSE)

cat("\n合并后最终数据集：", nrow(df), "例\n")
cat("N分期：");  print(table(df$N))
cat("KRAS：");   print(table(df$KRAS))
cat("性别：");   print(table(df$Sex))

