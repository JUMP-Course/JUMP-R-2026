# ============================================
# 第一步：数据导入
# ============================================

# 1.1 导入临床数据
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

# 1.2 导入KRAS突变数据
mut_file <- "C:/Users/86156/Desktop/mutations.txt"
mutations <- read.delim(mut_file, 
                        sep = "\t", 
                        stringsAsFactors = FALSE,
                        comment.char = "#")

cat("原始突变数据：", nrow(mutations), "行 ×", ncol(mutations), "列\n")

# ============================================
# 第二步：数据整理
# ============================================

# 2.1 整理临床数据
clinical <- paad_raw

# 创建统一ID（前12位）
clinical$Patient_ID_short <- substr(clinical$Patient.ID, 1, 12)

# 提取核心变量
clinical$OS_time   <- as.numeric(clinical$Overall.Survival..Months.)
clinical$OS_status <- ifelse(clinical$Overall.Survival.Status == "1:DECEASED", 1, 0)
clinical$Age       <- as.numeric(clinical$Diagnosis.Age)
clinical$Sex       <- clinical$Sex
clinical$N_Stage   <- clinical$AJCC.Pathologic.N.Stage

# 2.2 整理突变数据
mut <- mutations

# 创建统一ID（前12位）
mut$Patient_ID_short <- substr(mut$SAMPLE_ID, 1, 12)

# 创建KRAS状态
mut$KRAS_status <- ifelse(mut$KRAS == "WT", "Wild-type", "Mutant")

# 去重：一个患者多个样本，只要有突变就标记为Mutant
kras_unique <- aggregate(KRAS_status ~ Patient_ID_short, 
                         data = mut,
                         FUN = function(x) {
                             ifelse(any(x != "Wild-type"), "Mutant", "Wild-type")
                         })

cat("\nKRAS突变分布（去重后）：\n")
print(table(kras_unique$KRAS_status))

# 2.3 合并临床数据和KRAS数据
df_merged <- merge(clinical, kras_unique, 
                   by = "Patient_ID_short",
                   all.x = TRUE)

cat("\n合并后数据：", nrow(df_merged), "行\n")
cat("有KRAS数据的样本：", sum(!is.na(df_merged$KRAS_status)), "\n")
cat("无KRAS数据的样本：", sum(is.na(df_merged$KRAS_status)), "\n")

# ============================================
# 第三步：数据清洗
# ============================================

# 3.1 保留核心变量
df_clean <- df_merged[, c("Patient_ID_short", "OS_time", "OS_status", 
                           "Age", "Sex", "N_Stage", "KRAS_status")]

# 3.2 剔除缺失值
df_clean <- na.omit(df_clean)

# 3.3 剔除异常值
df_clean <- df_clean[df_clean$Age >= 18 & df_clean$Age <= 100, ]
df_clean <- df_clean[df_clean$OS_time >= 0, ]

# 3.4 处理N分期
# 剔除NX
df_clean <- df_clean[df_clean$N_Stage != "NX", ]

# N1b合并到N1
df_clean$N_Stage <- ifelse(df_clean$N_Stage == "N1b", "N1", 
                           as.character(df_clean$N_Stage))

# 3.5 因子化
df_clean$Sex       <- factor(df_clean$Sex, levels = c("Male", "Female"))
df_clean$N_Stage   <- factor(df_clean$N_Stage, levels = c("N0", "N1"))
df_clean$KRAS_status <- factor(df_clean$KRAS_status, 
                                levels = c("Wild-type", "Mutant"))

# ============================================
# 第四步：查看最终数据
# ============================================
cat("\n========== 最终清洗后数据 ==========\n")
cat("样本量：", nrow(df_clean), "\n")
cat("\n各变量分布：\n")
cat("N分期："); print(table(df_clean$N_Stage))
cat("KRAS："); print(table(df_clean$KRAS_status))
cat("Sex："); print(table(df_clean$Sex))
cat("\n数据结构：\n")
str(df_clean)
cat("\n前6行：\n")
head(df_clean)

