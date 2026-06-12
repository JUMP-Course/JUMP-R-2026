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

