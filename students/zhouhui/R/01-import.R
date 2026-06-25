#install.packages(c("tidyverse", "skimr", "naniar"))
library(tidyverse)
library(skimr)
library(naniar)
df <- read_rds("./data/test.rds")
dim(df)
names(df)
#-----------------------核心变量定义--------------------------------------
df$gdm <- ifelse(!is.na(df$GDM_epistart), 1, 0)
df$chd <- ifelse(!is.na(df$CHD_epistart), 1, 0)
#-----------查看人群基线患病情况以及初步观察暴露与结局关系---------------------------------------
table(df$gdm)
prop.table(table(df$gdm)) * 100
table(df$chd)
prop.table(table(df$chd)) * 100
table(df$chd, df$gdm)
prop.table(table(df$chd, df$gdm), margin = 2) * 100
#-------------------查看核心变量缺失值--------------------------------------------
miss_var_summary(df %>% select(gdm, chd, age, sex, bmi))