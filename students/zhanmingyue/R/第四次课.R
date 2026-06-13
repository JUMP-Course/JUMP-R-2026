#加载包

library(tidyverse)#数据处理
library(gtsummary)#制作表格

#读取清洗后的数据

dat<-read.csv("D:/shuju/clean_dat1.csv")

#检验变量的正态性
shapiro.test(dat$waist)
shapiro.test(dat$age)
shapiro.test(dat$chol)

#使用gtsummary制作Table1(按糖尿病状态分组)

table1<-dat%>%
  select(waist,age,chol,gender,diabetes)%>%  #选择变量
  tbl_summary(
    by=diabetes,  #按糖尿病分组
    statistic=list(
      all_continuous()~"{median}({p25}-{p75})",  #连续变量显示中位数（第25百分位数-第75百分位数）
      all_categorical()~"{n}({p}%)"  #分类变量显示频数（百分比）
      ),
    digits=all_continuous()~1  #数值保留1位小数
  )%>%
  add_p()%>%  #添加组间比较的p值
  modify_header(label="**变量**")%>%  #修改第一列标题
  modify_spanning_header(all_stat_cols()~"**糖尿病状态**")  #添加跨列标题

#打印表格
table1

  


