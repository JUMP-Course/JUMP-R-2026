install.packages(c("haven","tidyverse"))
library(haven)
library(tidyverse)
#导入原始数据
charls <- read_dta("C:/Users/Dell/Desktop/整理完的-charls数据/charls.dta")
#查看数据基本情况
dim(charls)#行列数
names(charls)#变量名称
glimpse(charls)#查看变量类型
colSums(is.na(charls))#全数据集中每个变量缺失的数量
round(colSums(is.na(charls))/nrow(charls)*100,2)#缺失比例
