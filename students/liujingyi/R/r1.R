# 改成你的样本目录
root <- "D:/gdc-client/cart"

# 递归检索所有名叫logs的文件夹
all_dir <- list.dirs(root, recursive = TRUE, full.names = TRUE)
logs_folder <- all_dir[basename(all_dir) == "logs"]

# 预览要删除的文件夹，先运行这行看列表
logs_folder

# 确认无误再执行删除，recursive=T连里面文件一起删掉
if (length(logs_folder) > 0) {
  unlink(logs_folder, recursive = TRUE)
}

####整合TCGA数据库2.0数据####
#####整合基因表达谱#####
#设置工作目录
setwd("D:/gdc-client")
#加载R包
install.packages("jsonlite")
library("jsonlite")
#读取metadata文件
json <- jsonlite::fromJSON("metadata.cart.2026-06-01.json")
