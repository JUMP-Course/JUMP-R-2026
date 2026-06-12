# ============================================
# TCGA-PAAD 胰腺癌预后预测模型
# ============================================

# ===================== 一、数据导入 =====================
# 加载分析所需R包
library(survival)    # 生存分析、Cox回归
library(survminer)   # 生存曲线可视化
library(rms)         # 列线图、校准曲线、C-index
library(timeROC)     # 时间依赖性ROC
library(broom)       # 回归结果提取
library(ggplot2)     # 绘图

## 1.1 导入临床原始数据（TSV格式）
file_path <- "C:/Users/86156/Desktop/paad_tcga_gdc_clinical_data.tsv"
paad_raw <- read.delim(
  file_path,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  quote = "",
  comment.char = "#"
)
cat("原始临床数据：", nrow(paad_raw), "行 ×", ncol(paad_raw), "列\n")

## 1.2 导入KRAS突变数据（TSV格式）
mut_file <- "C:/Users/86156/Desktop/mutations.txt"
mutations <- read.delim(
  mut_file, 
  sep = "\t", 
  stringsAsFactors = FALSE,
  comment.char = "#"
)
cat("原始突变数据：", nrow(mutations), "行 ×", ncol(mutations), "列\n")

# ===================== 二、数据整理 =====================
## 2.1 临床数据变量提取与格式转换
clinical <- paad_raw
# 截取前12位构建统一患者ID，用于后续数据匹配
clinical$Patient_ID_short <- substr(clinical$Patient.ID, 1, 12)

# 构建生存结局、临床自变量
clinical$OS_time   <- as.numeric(clinical$Overall.Survival..Months.)  # 总生存时间(月)
clinical$OS_status <- ifelse(clinical$Overall.Survival.Status == "1:DECEASED", 1, 0) # 生存状态：1=死亡，0=截尾
clinical$Age       <- as.numeric(clinical$Diagnosis.Age)             # 确诊年龄
clinical$Sex       <- factor(clinical$Sex, levels = c("Male", "Female")) # 性别，设置因子水平
clinical$N_Stage   <- clinical$AJCC.Pathologic.N.Stage               # 淋巴结N分期

# 筛选分析所需核心变量，生成初步临床数据集
df_clin_clean <- clinical[, c("Patient_ID_short", "OS_time", "OS_status", "Age", "Sex", "N_Stage")]

## 2.2 突变数据变量整理
mut <- mutations
mut$Patient_ID_short <- substr(mut$SAMPLE_ID, 1, 12)  # 统一患者ID
mut$KRAS_status <- ifelse(mut$KRAS == "WT", "Wild-type", "Mutant") # KRAS状态：野生型/突变型

# 同一患者多条突变记录去重，统一标记突变状态
kras_unique <- aggregate(
  KRAS_status ~ Patient_ID_short, 
  data = mut,
  FUN = function(x) {
    ifelse(any(x != "Wild-type"), "Mutant", "Wild-type")
  }
)
cat("\nKRAS突变分布（去重后）：\n")
print(table(kras_unique$KRAS_status))

# ===================== 三、数据清洗 =====================
## 3.1 临床数据异常值、缺失值清洗
df_clin_clean <- na.omit(df_clin_clean)                  # 删除含缺失值样本
df_clin_clean <- df_clin_clean[df_clin_clean$Age >= 18 & df_clin_clean$Age <= 100, ] # 筛选合理年龄范围
df_clin_clean <- df_clin_clean[df_clin_clean$OS_time >= 0, ] # 生存时间非负
df_clin_clean <- df_clin_clean[df_clin_clean$N_Stage != "" & !is.na(df_clin_clean$N_Stage), ] # 剔除N分期空值

## 3.2 分类变量合并与筛选
df_clin_clean <- df_clin_clean[df_clin_clean$N_Stage != "NX", ] # 剔除无法评估分期NX
df_clin_clean$N_Stage <- ifelse(df_clin_clean$N_Stage == "N1b", "N1", as.character(df_clin_clean$N_Stage)) # N1b合并为N1
df_clin_clean$N_Stage <- factor(df_clin_clean$N_Stage, levels = c("N0", "N1")) # 转为因子

cat("\n清洗后纯临床数据：", nrow(df_clin_clean), "例\n")
print(table(df_clin_clean$N_Stage))

## 3.3 临床数据与突变数据合并，剔除无匹配样本
df_clean <- merge(df_clin_clean, kras_unique, by = "Patient_ID_short", all.x = TRUE) # 左连接保留临床样本
df_clean <- df_clean[!is.na(df_clean$KRAS_status), ] # 剔除无KRAS信息样本
df_clean$KRAS_status <- factor(df_clean$KRAS_status, levels = c("Wild-type", "Mutant")) # 设置因子水平

## 3.4 查看最终清洗完成数据集基本信息
cat("\n========== 最终清洗后数据集 ==========\n")
cat("最终有效样本量：", nrow(df_clean), "\n")
cat("N分期分布："); print(table(df_clean$N_Stage))
cat("KRAS状态分布："); print(table(df_clean$KRAS_status))
cat("性别分布："); print(table(df_clean$Sex))
cat("\n数据集结构：\n")
str(df_clean)
