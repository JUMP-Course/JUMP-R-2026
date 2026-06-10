if (!require("nhanesA")) install.packages("nhanesA")  #导入数据库
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("survey")) install.packages("survey")  #矫正NHANES数据库的抽样设计
if (!require("forestplot")) install.packages("forestplot")  #绘制森林图
if (!require("tableone")) install.packages("tableone")  #生成基线特征表
if (!require("kableExtra")) install.packages("kableExtra")  #美化表格
if (!require("ggplot2")) install.packages("ggplot2")  #绘制统计图
library(nhanesA)
library(tidyverse)
library(survey)
library(forestplot)
library(tableone)
library(kableExtra)
library(ggplot2)
#人口学数据
demo <- nhanes("DEMO_L")
cat("DEMO_L 下载完成，样本量:", nrow(demo), "\n")  #字符串拼接，“\n换行，\t空格”
#血压问卷数据
bpq <- nhanes("BPQ_L")
cat("BPQ_L 下载完成，样本量:", nrow(bpq), "\n")
# 体格测量数据
bmx <- nhanes("BMX_L")
cat("BMX_L 下载完成，样本量:", nrow(bmx), "\n")
#合并数据
nhanes_merged <- demo %>%
  left_join(bpq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN")
cat("合并后总样本量:", nrow(nhanes_merged), "\n")
#筛选年龄大于20岁的样本
analysis_data <- nhanes_merged %>%
  dplyr::filter(RIDAGEYR >= 20)
cat("年龄筛选后样本量:", nrow(analysis_data), "\n")
cat("年龄范围:", range(analysis_data$RIDAGEYR, na.rm = TRUE), "岁\n")
#行列数
nrow(analysis_data)
ncol(analysis_data)
#变量名
names(analysis_data)
colnames(analysis_data)
#数据类型查看
class(analysis_data)
typeof(analysis_data)
mode(analysis_data)
#获取文件路径
getwd()














