library(dplyr)
library(mice)

# 逐步清洗
step1 <- merged_full %>% filter(RIDAGEYR >= 20)
nrow(step1)  

step2 <- step1 %>% filter(!is.na(SMQ020))
nrow(step2)  

step3 <- step2 %>% filter(BPQ020 %in% c(1, 2))
nrow(step3) 

# 在这个step3子集中，检查 BMI 缺失的人的身高体重情况
step3 %>%
  filter(is.na(BMXBMI)) %>%
  summarise(总缺失 = n(),身高缺失 = sum(is.na(BMXHT)),体重缺失 = sum(is.na(BMXWT)),
            身高体重都完整 = sum(!is.na(BMXHT) & !is.na(BMXWT)) )

# BMI缺失值无法进行填补，删除处理
step4 <- step3 %>% filter(!is.na(BMXBMI))
nrow(step4) 

# PIR 多重插补(进行缺失值处理)
impute_data <- step4 %>%
  select(SEQN, INDFMPIR, DMDEDUC2, RIDAGEYR, RIAGENDR, RIDRETH3, DMDMARTL, DMDFMSIZ)

imp <- mice(impute_data, m = 5, method = "pmm", seed = 123, printFlag = FALSE)
completed <- complete(imp, 1)

step4 <- step4 %>%
  select(-INDFMPIR) %>%
  left_join(completed %>% select(SEQN, INDFMPIR), by = "SEQN")

summary(step4$INDFMPIR, na.rm = FALSE)

step5 <- step4 %>% filter(DMDEDUC2 %in% c(1, 2, 3, 4, 5))
nrow(step5)

# 挑选需要的变量（包括权重）
clean_data <- step5 %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3,DMDEDUC2, INDFMPIR, BMXBMI, BPQ020, SMQ020, WTMEC2YR, SDMVPSU, SDMVSTRA)  
dim(clean_data)

# 保存
saveRDS(clean_data, "clean_data.rds")

# 变量转换
clean_data <- clean_data %>%
  mutate(hypertension = case_when(BPQ020 == 1 ~ 1,BPQ020 == 2 ~ 0,TRUE ~ NA_real_),
    smoking = case_when(SMQ020 == 1 ~ 1,SMQ020 == 2 ~ 0,TRUE ~ NA_real_),
    gender = case_when(RIAGENDR == 1 ~ "Male",RIAGENDR == 2 ~ "Female",TRUE ~ NA_character_),
    age_group = case_when(RIDAGEYR < 40 ~ "20-39",RIDAGEYR < 60 ~ "40-59",TRUE ~ "60-80"),
    race = case_when(RIDRETH3 == 1 ~ "Mexican American",
                     RIDRETH3 == 2 ~ "Other Hispanic",
                     RIDRETH3 == 3 ~ "Non-Hispanic White",
                     RIDRETH3 == 4 ~ "Non-Hispanic Black",
                     RIDRETH3 == 6 ~ "Non-Hispanic Asian",TRUE ~ "Other"),
    bmi_group = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI < 25 ~ "Normal",
      BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese",
      TRUE ~ NA_character_),
    education = case_when(
      DMDEDUC2 %in% c(1, 2) ~ "Below high school",
      DMDEDUC2 %in% c(3, 4, 5) ~ "High school or above",
      TRUE ~ NA_character_),
    pir_group = case_when(
      INDFMPIR < 2 ~ "Low",
      INDFMPIR >= 2 & INDFMPIR <= 4 ~ "Middle",
      INDFMPIR > 4 ~ "High",
      TRUE ~ NA_character_))

# 查看转换结果
glimpse(clean_data)

# 检查各变量分布
table(clean_data$smoking, useNA = "ifany")
table(clean_data$hypertension, useNA = "ifany")
table(clean_data$gender, useNA = "ifany")
table(clean_data$age_group, useNA = "ifany")
table(clean_data$race, useNA = "ifany")
table(clean_data$bmi_group, useNA = "ifany")
table(clean_data$education, useNA = "ifany")
table(clean_data$pir_group, useNA = "ifany")

# 保存
saveRDS(clean_data, "clean_data.rds")

dim(clean_data)
table(clean_data$smoking, clean_data$hypertension)












