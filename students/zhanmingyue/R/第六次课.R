#加载包
library(tidyverse)

#读取数据
dat<-readRDS("D:/shuju/dat1.rds")

#查看数据概览
str(dat)       
summary(dat)    
table(dat$diabetes)

#将性别转换为因子
dat$gender <- as.factor(dat$gender)

###连续变量的组间比较

#正态性检验

shapiro.test(dat$waist)   
shapiro.test(dat$age)    
shapiro.test(dat$chol)

#Wilcoxon秩和检验

wilcox.test(waist ~ diabetes, data = dat)
wilcox.test(age ~ diabetes, data = dat)
wilcox.test(chol ~ diabetes, data = dat)

###分类变量的组间比较

#创建列联表

gender_table <- table(dat$gender, dat$diabetes)
print(gender_table)

#卡方检验

chisq.test(gender_table)

#创建结果数据框

test_results <- data.frame(
  变量 = c("腰围", "年龄", "总胆固醇", "性别"),
  检验方法 = c("Wilcoxon秩和检验", "Wilcoxon秩和检验", "Wilcoxon秩和检验", "卡方检验"),
  统计量 = c(
    wilcox.test(waist ~ diabetes, data = dat)$statistic,
    wilcox.test(age ~ diabetes, data = dat)$statistic,
    wilcox.test(chol ~ diabetes, data = dat)$statistic,
    chisq.test(gender_table)$statistic
  ),
  p值 = c(
    wilcox.test(waist ~ diabetes, data = dat)$p.value,
    wilcox.test(age ~ diabetes, data = dat)$p.value,
    wilcox.test(chol ~ diabetes, data = dat)$p.value,
    chisq.test(gender_table)$p.value
  )
)
print(test_results)
