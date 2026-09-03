rm(list = ls())
#设置工作目录
setwd("D:/gdc-client")
#加载R包
#install.packages("rjson")
library("rjson")
#读取metadata文件
json <- jsonlite::fromJSON("metadata.cart.2026-06-01.json")
#提取TCGA样本编号
#id <- json$associated_entities[[1]][,1]
sample_id <- sapply(json$associated_entities,function(x){x[,1]})
#将TCGA的样本编号和表达谱文件夹名称一一对应
file_sample <- data.frame(sample_id,file_name=json$file_name)

####----------------------构建tpm矩阵----------------------####
#获取cart文件夹下的所有TSV表达文件的 路径+文件名
tpm_file <- list.files('cart',pattern ='.tsv',recursive = T)
head(tpm_file)#查看数据结构
#在count_file中分割出文件名
file_name <- strsplit(tpm_file,split='/')#以/为分隔符
file_name <- sapply(file_name,function(x){x[2]})#选择/后的数据
head(file_name)#查看数据结构，是否与file name一致
#定义一个足够大的空矩阵
matrix <- data.frame(matrix(nrow=60660,ncol=0))
#提取每个样本的表达矩阵，tpm数据的提取，用于生存分析
for (i in 1:length(file_name)){ 
  path <- paste0('cart//',tpm_file[i])#读取工作路径
  data <- read.delim(path,fill = T,header = F,row.names = 1) #读取count.tsv文件
  colnames(data) <- data[2,]#第二行作为列名
  data <- data[-c(1:6),]#删除前6行
  data <- data[,6,drop=F]   #tpm选择6
  colnames(data) <- file_sample$sample_id[which(file_sample$file_name==file_name[i])] 
  matrix <- cbind(matrix,data)
}
matrix_tpm <- matrix
#保存表达矩阵
write.csv(matrix_tpm,'tpm_matrix.csv',row.names = T)

####----------------------ID与tpm合并----------------------####
##设置Gene Symbol作为列名
#重新读取原始表达矩阵
path = paste0('cart//',tpm_file[1])
data <- as.matrix(read.delim(path,fill = T,header = F,row.names = 1))
#提取出第1列的gene_name（symbol）
gene_name <- data[-c(1:6),1]#删除前6行，提取第1列
#将gene_name（symbol）与上述未注释的基因表达谱合并
matrix_tpm0 <- cbind(gene_name,matrix)
#将gene_name列去除重复的基因，保留每个基因最大表达量结果
matrix_tpm0 <- aggregate( . ~ gene_name,data=matrix_tpm0, max)
#将gene_name列设为行名
rownames(matrix_tpm0) <- matrix_tpm0[,1]
#删除多余的第一列
matrix_tpm0 <- matrix_tpm0[,-1]
matrix_tpm0 <- matrix_tpm0[,sample_id]
str(matrix_tpm0)#查看数据格式，是否为numeric
tpm_log2 <- log2(as.data.frame(lapply(matrix_tpm0, as.numeric)) + 1)
rownames(tpm_log2) <- rownames (matrix_tpm0)
colnames(tpm_log2) <- gsub('\\.','-',colnames(tpm_log2))
str(tpm_log2)
#保存最终整理好的tpm数据
write.csv(tpm_log2,'tpm-COAD.csv', row.names = T)
load('GDC_COAD_data.rda')
save(matrix,matrix0,clinical,clinical_OS.time,matrix_tpm,tpm_log2,file = 'GDC_COAD_count_tpm.rda')



