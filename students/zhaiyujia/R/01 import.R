library(haven)
library(dplyr)

#读取人口学数据
demog <- read_dta("Demographic_Background.dta")

#查看数据内部结构
str(demog)  # 行数、列数、每个变量的类型和前几个示例值。


#查看所有列名
names(demog)

#查看每个变量的缺失值数量和百分比
colMeans(is.na(demog))* 100

#查看年龄的缺失情况
mean(is.na(demog$xrage))

#年龄分布的统计摘要
summary(demog$xrage)  #最小值、第一四分位数、中位数、均值、第三四分位数、最大值

#导入健康数据
health <- read_dta("Health_Status_and_Functioning.dta")