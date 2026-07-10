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