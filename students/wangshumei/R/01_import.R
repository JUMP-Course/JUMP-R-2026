##==============数据基本情况=================================================
# 1. 安装和加载包并读取数据
install.packages("tidyverse", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
install.packages("readr")
install.packages("survminer")
install.packages("survival")

library(tidyverse)  #数据处理包，包含dplyr和ggplot2
library(survival)   #生存分析专用包
library(survminer)  #画图专用包
library(readr)      #读取tsv/txt文件更快

# 设置文件路径并读取数据
file_path <- "D:/JUMP/clinical.tsv" 
df <- read_tsv(file_path)

#  查看数据结构 
# glimpse 会列出每一列的名字和数据类型
glimpse(df)

#  查看前几行数据
head(df)

#  查看所有的列名 
colnames(df)
#-----------------------------------------------------------------------------

# 2. 提取我所需要的列
library(dplyr)
df_selected <- df %>%
  select(cases.submitter_id,
         diagnoses.tumor_grade,
         demographic.vital_status,
         demographic.days_to_death,
         cases.lost_to_followup,
         diagnoses.days_to_last_follow_up,
         demographic.age_at_index,
         diagnoses.figo_stage,
         treatments.treatment_type
  )

# 看看 df_selected 有哪些列
colnames(df_selected)

# 看看 df_selected 的前 10 行
head(df_selected, 10)
#-------------------------------------------------------------------------------------

# 3.为每位患者合并治疗信息，但保留唯一的临床信息
df_final <- df_selected %>%
  group_by(cases.submitter_id) %>%
  summarise(
    tumor_grade = first(na.omit(diagnoses.tumor_grade)),
    vital_status = first(na.omit(demographic.vital_status)),
    days_to_death = first(na.omit(demographic.days_to_death)),
    lost_to_followup = first(na.omit( cases.lost_to_followup)),
    days_to_last_follow_up = first(na.omit(diagnoses.days_to_last_follow_up)),
    age_at_index = first(na.omit(demographic.age_at_index)),
    figo_stage = first(na.omit(diagnoses.figo_stage)),
    all_treatments = paste(unique(na.omit(treatments.treatment_type)), collapse = "; "),
    .groups = 'drop'
  )

# 查看结果
head(df_final)
nrow(df_final)
glimpse(df_final)


# 查看肿瘤分级分布
table(df_final$tumor_grade, useNA = "ifany")

# 查看生存状态分布
table(df_final$vital_status, useNA = "ifany")

#  查看FIGO分期分布
table(df_final$figo_stage, useNA = "ifany")


# 查看年龄的概括性统计（最小值、中位数、平均值、最大值等）
summary(df_final$age_at_index)

# 将“天数”列从文本型转换为数值型
df_final$days_to_death <- as.numeric(df_final$days_to_death)
# 将“末次随访时间”列从文本型转换为数值型
df_final$days_to_last_follow_up <- as.numeric(df_final$days_to_last_follow_up)

# 创建生存时间变量：如果死亡，用死亡天数；如果存活，用最后随访天数
df_final$time <- ifelse(df_final$vital_status == "Dead", 
                        as.numeric(df_final$days_to_death), 
                        as.numeric(df_final$days_to_last_follow_up))

# 创建生存状态变量：1=死亡，0=删失（存活或失访）
df_final$status <- ifelse(df_final$vital_status == "Dead", 1, 0)
table(df_final$vital_status, useNA = "ifany")

ggplot(df_final, aes(x = tumor_grade)) +
  geom_bar(fill = "steelblue", width = 0.6) +
  labs(title = "Distribution of Tumor Grade",
       x = "Tumor Grade", y = "Number of Patients") +
  theme_minimal()
#-----------------------------------------------------------------------------------
