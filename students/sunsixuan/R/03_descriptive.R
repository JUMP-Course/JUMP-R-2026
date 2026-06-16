install.packages(c("haven","tidyverse","dplyr"))
library(haven)
library(tidyverse)
library(dplyr)
#描述性统计
summary(df_analy$age)
age_freq <- df_analy %>%
  count(age_group, name = "频数") %>%
  mutate(
    频率= round(频数 / sum(频数) * 100, 2),
  )
print(age_freq)#年龄分布

gender_freq <- df_analy %>%
  count(gender, name = "频数") %>%
  mutate(
    频率 = round(频数 / sum(频数) * 100, 2)
  )
print(gender_freq)#性别分布

marry_freq <- df_analy %>%
  count(marry, name = "频数") %>%
  mutate(
    频率 = round(频数 / sum(频数) * 100, 2)
  )
print(marry_freq)#婚姻状况

rural_freq <- df_analy %>%
  count(rural, name = "频数") %>%
  mutate(
    频率 = round(频数 / sum(频数) * 100, 2)
  )
print(rural_freq)#城乡居住地

df_analy<- df_analy %>%
  mutate(
    edu_label = case_when(
      edu == 1 ~ "小学及以下",
      edu == 2 ~ "小学",
      edu == 3 ~ "中学",
      edu == 4 ~ "高中及以上",
    )
  )
edu_freq<- df_analy %>%
  count(edu_label, name = "频数") %>%
  mutate(
    频率= round(频数 / sum(频数) * 100, 2),
  )
print(edu_freq)#教育水平分布

ins_freq <- df_analy %>%
  count(ins, name = "频数") %>%
  mutate(
    频率= round(频数 / sum(频数) * 100, 2),
  )
print(ins_freq)#医疗保险分布

qqnorm(df_analy$hhcperc, main = "家庭年人均消费正态Q-Q图")
qqline(df_analy$hhcperc, col = "red", lwd = 2)
quantile(df_analy$hhcperc,c(0.25,0.5,0.75))#家庭年人均消费分布

smoken_freq <- df_analy %>%
  count(smoken, name = "频数") %>%
  mutate(
    频率= round(频数 / sum(频数) * 100, 2),
  )
print(smoken_freq)#吸烟史分布

chronic_freq <- df_analy %>%
  count(chronic_group, name = "频数") %>%
  mutate(
    频率 = round(频数 / sum(频数) * 100, 2)
  )
print(chronic_freq)#慢性病数量分布

df_analy<- df_analy %>%
  mutate(
    srh_label = case_when(
      srh == 1 ~ "很差",
      srh == 2 ~ "较差",
      srh == 3 ~ "一般",
      srh == 4 ~ "较好",
      srh == 5 ~ "很好"
    ),
    srh_label = factor(srh_label,
                       levels = c("很差","较差","一般","较好","很好"))
  )
srh_freq<- df_analy %>%
  count(srh_label, name = "频数") %>%
  mutate(
    频率= round(频数 / sum(频数) * 100, 2),
  )
print(srh_freq)#自评健康状况分布

doctor_rate <- df_analy %>%
  count(doctor, name = "例数") %>%
  mutate(百分比 = round(例数 / sum(例数) * 100, 2))
hospital_rate <- df_analy %>%
  count(hospital, name = "例数") %>%
  mutate(百分比 = round(例数 / sum(例数) * 100, 2))#卫生服务利用情况


#绘制基线特征表
install.packages(c("gtsummary","flextable"))
library(gtsummary)
install.packages("zip")
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("zip", dependencies = TRUE)
install.packages("flextable")
library(flextable)
df_analy <- df_analy %>%
  mutate(
    age_group = factor(age_group, levels = c("65-74岁","75-84岁","85岁及以上")),
    chronic_group = factor(chronic_group, levels = c("0种","1种","2种","3种","4种","5种及以上")),
    srh_label = factor(srh_label, levels = c("很差","较差","一般","较好","很好"))
  )
table_baseline <- df_analy %>%
  tbl_summary(
    include = c(age_group,edu_label,hhcperc,srh_label,chronic_group,gender,marry,rural,ins,smoken),
    statistic = list(
      all_categorical() ~ "{n}({p}%)",
      all_continuous() ~ "{median}({p25}, {p75})"
    ),
    digits = all_categorical() ~ c(0,2), 
    missing="no"
  ) %>%
  modify_header(label = "基线特征", stat_0 = paste0("总人群(n=",nrow(df_analy),")")) %>%
  modify_caption("研究对象总体基线特征")
table_baseline
#导出基线特征表
table_baseline%>%
  as_flex_table() %>%
  save_as_docx(path = "总体基线特征表.docx")

install.packages ("labelled")
library(labelled)
df_analy<-unlabelled(df_analy)