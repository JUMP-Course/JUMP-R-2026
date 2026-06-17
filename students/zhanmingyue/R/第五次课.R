###基础可视化与结果表达

#1.加载包

library(tidyverse)

#2.读取清洗后的数据

dat<-readRDS("D:/shuju/dat1.rds")

#图1：腰围分布直方图

p1<-ggplot(dat,aes(x=waist))+
  geom_histogram(binwidth = 5,fill="skyblue",color="black",alpha=0.7)+
  labs(title="腰围分布直方图",x="腰围(cm)",y="频数")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=14))
p1

#图2：腰围与糖尿病状态的箱线图

p2<-ggplot(dat,aes(x=diabetes,y=waist,fill=diabetes))+
  geom_boxplot(alpha=0.7,outlier.color="red")+
  scale_fill_manual(values=c("lightgreen","lightpink"))+
  labs(title="不同糖尿病状态的腰围比较",x="糖尿病状态",y="腰围(cm)")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),legend.position = "none")
p2

#图三：年龄分布直方图

p3<-ggplot(dat,aes(x=age))+
  geom_histogram(binwidth=5,fill="lightblue",color="black",alpha=0.7)+
  labs(title="年龄分布直方图",x="年龄(岁)",y="频数")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5,size=14))
p3

#图四：按性别分组的腰围箱线图

p4<-ggplot(dat,aes(x=gender,y=waist,fill=gender))+
  geom_boxplot(alpha=0.7,outlier.color="red")+
  scale_fill_manual(values=c("lightpink","lightblue"))+
  labs(title="不同性别的腰围分布",x="性别",y="腰围(cm)")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),legend.position = "none")
p4

#查看极端值

dat_diab<-subset(dat,diabetes=="糖尿病")
boxplot(dat_diab$waist,plot=FALSE)$out
dat_diab[dat_diab$waist%in%boxplot(dat_diab$waist,plot=FALSE)$out,]

dat_male<-subset(dat,gender=="male")
boxplot(dat_male$waist,plot=FALSE)$out
dat_male[dat_male$waist%in%boxplot(dat_male$waist,plot=FALSE)$out,]

#查看原始数据中腰围极端值人群的bmi

dat1<-read.csv("D:/shuju/diabetes.csv")
dat1$waist<-dat1$waist*2.54  
dat1$height_cm<-dat1$height*2.54
dat1$height_m<-dat1$height_cm/100
dat1$weight_kg<-dat1$weight*0.4536
dat1$bmi<-dat1$weight_kg/(dat1$height_m)^2

outlier<-which(dat1$waist>129)
print(outlier)
dat1[outlier,]

