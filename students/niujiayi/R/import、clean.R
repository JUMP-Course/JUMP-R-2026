install.packages(c("nhanesA","tidyverse","haven"))
library(nhanesA)
library(tidyverse)
library(haven)


library(tidyverse)
library(haven)

demo <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_DEMO.xpt")
alq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_ALQ.xpt")
mcq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_MCQ.xpt")
bmx  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_BMX.xpt")

all_data <- demo %>%
  left_join(alq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN")

female <- all_data %>% filter(RIAGENDR == 2, RIDAGEYR >= 18)

female <- female %>%
  mutate(breast_cancer = case_when(
    MCQ230A ==14 | MCQ230B ==14 | MCQ230C ==14 ~ 1,
    TRUE ~ 0
  ))

write.csv(female,"D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_整理后数据.csv",row.names = F)

dim(female)
table(female$breast_cancer)
table(female$ALQ120)

names(alq)

table(female$ALQ121)




