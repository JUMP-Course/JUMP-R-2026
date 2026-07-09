# BMI与高血压关联的Logistic回归分析
## 1.整理数据
### 1.1 加载R包--------------------------------------------------
if (!require("nhanesA")) install.packages("nhanesA")  #导入数据库
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("survey")) install.packages("survey")  #矫正NHANES数据库的抽样设计
if (!require("forestplot")) install.packages("forestplot")  #绘制森林图
if (!require("tableone")) install.packages("tableone")  #生成基线特征表
if (!require("kableExtra")) install.packages("kableExtra")  #美化表格
if (!require("ggplot2")) install.packages("ggplot2")  #绘制统计图
if (!require("car")) install.packages("car")  # 多重共线性检验
if (!require("pROC")) install.packages("pROC")  # ROC曲线
if (!require("sjPlot")) install.packages("sjPlot")  # 可视化模型
library(car)
library(pROC)
library(sjPlot)
library(nhanesA)
library(tidyverse)
library(survey)
library(forestplot)
library(tableone)
library(kableExtra)
library(ggplot2)

### 1.2 下载数据

# 人口学数据（2021-2023周期标识为"L"）
demo <- nhanes("DEMO_L")
cat("DEMO_L 下载完成，样本量:", nrow(demo), "\n")  #字符串拼接，“\n换行，\t空格”

# 血压问卷数据
bpq <- nhanes("BPQ_L")
cat("BPQ_L 下载完成，样本量:", nrow(bpq), "\n")

# 体格测量数据
bmx <- nhanes("BMX_L")
cat("BMX_L 下载完成，样本量:", nrow(bmx))

### 1.3 合并数据

nhanes_merged <- demo %>%
  left_join(bpq, by = "SEQN") %>%  #SEQN <- ID号
  #以 demo 表为基准，保留所有在 demo里的人，bpq里有血压数据就加上，没有就留空。
  left_join(bmx, by = "SEQN")
cat("合并后总样本量:", nrow(nhanes_merged), "\n")
analysis_data <- nhanes_merged

## 2.数据探索
### 2.1 行列数
nrow(analysis_data)
ncol(analysis_data)

### 2.2 变量名

names(analysis_data)
colnames(analysis_data)

### 2.3 数据类型查看

class(analysis_data)
typeof(analysis_data)
mode(analysis_data)

### 2.4 获取当前目录

getwd()
