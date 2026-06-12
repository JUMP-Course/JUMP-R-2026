library(dplyr)
library(haven)

demo <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_DEMO.xpt")
alq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_ALQ.xpt")
mcq  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_MCQ.xpt")
bmx  <- read_xpt("D:/JUMP-R-2026/students/niujiayi/doc/P_BMX.xpt")

all_data <- demo %>%
  left_join(alq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN")

cat("===== 数据清洗样本量变化 =====\n")
cat("① 原始合并总样本量：", nrow(all_data), "人\n")

female <- all_data %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 18)
cat("② 筛选成年女性后样本量：", nrow(female), "人\n")

female <- female %>% 
  mutate(
    breast_cancer = case_when(
      MCQ230A == 14 | MCQ230B == 14 | MCQ230C == 14 ~ 1,
      TRUE ~ 0
    )
  )

female$ALQ121[female$ALQ121 == 99] <- NA

female_clean <- female %>% 
  filter(!is.na(ALQ121), !is.na(breast_cancer))
cat("③ 删除核心变量缺失后最终样本量：", nrow(female_clean), "人\n")
female_clean <- female_clean %>% 
  mutate(
    breast_cancer = factor(
      breast_cancer, 
      levels = c(0, 1), 
      labels = c("未患乳腺癌", "患乳腺癌")
    ),
    ALQ121 = factor(
      ALQ121,
      levels = c(0,1,2,3,4,5,6,7,8,9,10),
      labels = c("从不饮酒","少于每月1次","每月1次","每月2-3次",
                 "每周1次","每周2-3次","每周4-5次","每天1次",
                 "每天2-3次","每天4-5次","每天≥6次")
    )
  )
analysis_data <- female_clean %>% 
  select(SEQN, RIDAGEYR, ALQ121, breast_cancer, BMXBMI)
write.csv(analysis_data, "D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_最终清洗数据.csv", row.names = FALSE)
cat("\n===== 最终数据集缺失值统计 =====\n")
print(colSums(is.na(analysis_data)))