####----------------------整理count数据----------------------####
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

#获取cart文件夹下的所有TSV表达文件的 路径+文件名
count_file <- list.files('cart',pattern ='.tsv',recursive = T)
head(count_file)#查看数据结构

#在count_file中分割出文件名
file_name <- strsplit(count_file,split='/')#以/为分隔符
file_name <- sapply(file_name,function(x){x[2]})#选择/后的数据
head(file_name)#查看数据结构，是否与file name一致

#定义一个足够大的空矩阵
matrix <- data.frame(matrix(nrow=60660,ncol=0))
#提取每个样本的表达矩阵，counts数据的提取
for (i in 1:length(file_name)){ 
  path <- paste0('cart//',count_file[i])#读取工作路径
  data <- read.delim(path,fill = T,header = F,row.names = 1) #读取count.tsv文件
  colnames(data) <- data[2,]#第二行作为列名
  data <- data[-c(1:6),]#删除前6行
  data <- data[,3,drop=F]   #取出unstranded列（第3列），即count数据，对应其它数据可根据实际列号修改 
  colnames(data) <- file_sample$sample_id[which(file_sample$file_name==file_name[i])] 
  matrix <- cbind(matrix,data)
}
#保存表达矩阵
write.csv(matrix,'count_matrix.csv',row.names = T)

##设置Gene Symbol作为列名
#重新读取原始表达矩阵
path = paste0('cart//',count_file[1])
data <- as.matrix(read.delim(path,fill = T,header = F,row.names = 1))
#提取出第1列的gene_name（symbol）
gene_name <- data[-c(1:6),1]#删除前6行，提取第1列
#将gene_name（symbol）与上述未注释的基因表达谱合并
matrix0 <- cbind(gene_name,matrix)

#将gene_name列去除重复的基因，保留每个基因最大表达量结果
matrix0 <- aggregate( . ~ gene_name,data=matrix0, max)
#将gene_name列设为行名
rownames(matrix0) <- matrix0[,1]
#删除多余的第一列
matrix0 <- matrix0[,-1]
#保存最终整理好的counts数据
write.csv(matrix0,'counts-COAD.csv', row.names = T)

####----------------------整理临床数据----------------------####
#rm(list = ls())
#设置工作目录 
setwd("D:/gdc-client")
#此处读取metadata以及提取TCGA样本名
library(rjson)
json <- jsonlite::fromJSON("metadata.cart.2026-06-01.json")
entity_submitter_id <- sapply(json$associated_entities,function(x){x[,1]})
#提取case_id 
case_id <- sapply(json$associated_entities,function(x){x[,3]})
#将TCGA样本名与病人编号合并后取转置
sample_case <- t(rbind(entity_submitter_id,case_id))
#读取clinical.tsv文件
clinical <- read.delim('clinical.tsv',header = T)
colnames(clinical)[2] <- "case_id"#将clinical里的第2列重命名为case_id
unique(clinical$cases.submitter_id)
#识别并提取数据里存在重复的样本
clinical_unique <- clinical[!duplicated(clinical$case_id), ]

#clinical <- as.data.frame(clinical[duplicated(clinical$case_id),])
sum(duplicated(clinical$case_id))
#根据“case_id”这一列将TCGA样本名整合进来
clinical_matrix <- merge(sample_case,clinical_unique,by="case_id",all.x=T)
#此时病人的编号已经没什么用了，将其删除
clinical_matrix <- clinical_matrix[,-1]
#colnames(clinical_unique)
#unique(clinical_unique$case_id) #查看几个case
#unique(clinical_unique$cases.submitter_id) #查看几个case
library(tidyverse)
filter_clinical <- clinical_matrix %>% select("project.project_id","entity_submitter_id","demographic.age_at_index","demographic.gender",
                                       "demographic.vital_status","demographic.days_to_death","diagnoses.days_to_last_follow_up",
                                       "diagnoses.ajcc_pathologic_stage","diagnoses.ajcc_pathologic_t","diagnoses.ajcc_pathologic_m","diagnoses.ajcc_pathologic_n")
colnames(filter_clinical)#查看列名，便于复制
clinical_OS <- filter_clinical %>% rename(project=project.project_id,ID=entity_submitter_id,
                                          OS=demographic.vital_status,
                                          OS.time=diagnoses.days_to_last_follow_up,
                                          gender=demographic.gender,
                                          age=demographic.age_at_index,
                                          M=diagnoses.ajcc_pathologic_m,
                                          N=diagnoses.ajcc_pathologic_n,
                                          T=diagnoses.ajcc_pathologic_t,
                                          stage=diagnoses.ajcc_pathologic_stage,
)

#补齐OS.time
library(dplyr)
clinical_OS <- clinical_OS %>%
  mutate(
    OS.time = case_when(
      OS == "Dead" & is.na(OS.time) ~ demographic.days_to_death,
      TRUE ~ OS.time
    ),
    OS.time = as.numeric(OS.time)# 转为数值，便于生存分析
  )
clinical_OS$OS <- ifelse(str_detect(clinical_OS$OS,'Dead'),'1','0') #如果os列中为Dead是1, 其余是0
clinical_OS.time <- clinical_OS %>% select('ID', 'OS.time', 'OS',everything())#
view(clinical_OS.time)
#保存临床数据
write.csv(clinical,file = 'GDC_COAD_clinical.csv')
write.csv(clinical_OS.time,file = 'GDC_COAD_clinical_OS.time.csv')
save(matrix,matrix0,clinical,clinical_OS.time,file = 'GDC_COAD_data.rda')#保存为rda文件
