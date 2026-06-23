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


# 处理血压异常值

# 将舒张压 < 30 或 > 120，收缩压 < 50 或 > 250定义为缺失
step4 <- step4 %>%
  mutate(
    BPXSY1 = if_else(BPXSY1 < 50 | BPXSY1 > 250, NA_real_, BPXSY1),
    BPXSY2 = if_else(BPXSY2 < 50 | BPXSY2 > 250, NA_real_, BPXSY2),
    BPXSY3 = if_else(BPXSY3 < 50 | BPXSY3 > 250, NA_real_, BPXSY3),
    BPXDI1 = if_else(BPXDI1 < 30 | BPXDI1 > 120, NA_real_, BPXDI1),
    BPXDI2 = if_else(BPXDI2 < 30 | BPXDI2 > 120, NA_real_, BPXDI2),
    BPXDI3 = if_else(BPXDI3 < 30 | BPXDI3 > 120, NA_real_, BPXDI3))
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

# 挑选需要的变量
clean_data <- step5 %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3,DMDEDUC2, INDFMPIR, BMXBMI, BPQ020, BPXSY1, BPXSY2, BPXSY3,BPXDI1,BPXDI2,BPXDI3, SMQ020, SMQ040, WTMEC2YR, SDMVPSU, SDMVSTRA)  
dim(clean_data)

# 保存
saveRDS(clean_data, "clean_data.rds")

#计算实测血压平均值
clean_data <- clean_data %>%
  mutate(
    SBP_avg = rowMeans(across(c(BPXSY1, BPXSY2, BPXSY3)), na.rm = TRUE),
    DBP_avg = rowMeans(across(c(BPXDI1, BPXDI2, BPXDI3)), na.rm = TRUE))
#进行变量转换
clean_data <- clean_data %>%
  mutate(hypertension = case_when((SBP_avg >= 140 | DBP_avg >= 90) ~ 1,BPQ020 == 1 ~ 1,
                                  (SBP_avg < 140 & DBP_avg < 90) & BPQ020 == 2 ~ 0, 
                                  (!is.finite(SBP_avg) | !is.finite(DBP_avg)) & BPQ020 == 2 ~ 0,
                                   TRUE ~ NA_real_),
         smoking = case_when(
           SMQ020 == 2 ~ "Never",
           SMQ020 == 1 & SMQ040 == 3 ~ "Former",
           SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ "Current",
           TRUE ~ NA_character_),
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
           DMDEDUC2 == 1 ~  "Lower than grade 9",
           DMDEDUC2 == 2~ "9–11 grade",
           DMDEDUC2 == 3~ "Highschool graduate",
           DMDEDUC2 == 4~ "Some college or AA degree",
           DMDEDUC2 == 5~ "College graduate or higher",
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












