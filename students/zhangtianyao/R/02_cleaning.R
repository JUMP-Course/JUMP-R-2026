#===========数据清洗==========
#查看变量的缺失值数量和比例
# 查看每个变量的缺失值数量和比例
missing_info <- data.frame(
  变量 = names(final_data),
  缺失数 = sapply(final_data, function(x) sum(is.na(x))),
  缺失比例 = sapply(final_data, function(x) mean(is.na(x)))
)
# 按缺失比例从高到低排序
missing_info <- missing_info[order(-missing_info$缺失比例), ]
print(missing_info)
#========重新合并协变量=========
#饮酒
library(dplyr)
alq_1516_std <- alq_1516 %>%
  transmute(
    SEQN,
    Alcohol = case_when(
      ALQ101 == 1 ~ 1,
      ALQ101 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
alq_1718_std <- alq_1718 %>%
  transmute(
    SEQN,
    Alcohol = case_when(
      ALQ111 == 1 ~ 1,
      ALQ111 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
alq_all <- bind_rows(alq_1516_std,alq_1718_std)
table(alq_all$Alcohol,useNA = "always")
#吸烟
smq_all <- bind_rows(smq_1516,smq_1718) %>%
  transmute(
    SEQN,
    Smoking = case_when(
      SMQ020 == 1 ~ 1,
      SMQ020 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(smq_all$Smoking,useNA = "always")
#体力活动
paq_all <- bind_rows(paq_1516,paq_1718) %>%
  transmute(
    SEQN,
    PhysicalActivity = case_when(
      PAQ650 == 1 | PAQ665 == 1~ 1,
      PAQ650 == 2 & PAQ665 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(paq_all$PhysicalActivity,useNA = "always")
#高血压
bpq_all <- bind_rows(bpq_1516,bpq_1718) %>%
  transmute(
    SEQN,
    Hypertension = case_when(
      BPQ020 == 1 ~ 1,
      BPQ020 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(bpq_all$Hypertension,useNA = "always")
#糖尿病
diq_all <- bind_rows(diq_1516,diq_1718) %>%
  transmute(
    SEQN,
    Diabetes = case_when(
      DIQ010 == 1 ~ 1,
      DIQ010 == 2 ~ 0,
      DIQ010 == 3 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(diq_all$Diabetes,useNA = "always")
#高胆固醇
tchol_all <- bind_rows(tchol_1516,tchol_1718) %>%
  transmute(
    SEQN,
    Dyslipidemia = case_when(
      LBXTC >= 240~ 1,
      LBXTC < 240~ 0,
      TRUE~ NA_real_
    )
  )
table(tchol_all$Dyslipidemia,useNA = "always")
#BMI
bmx_all <- bind_rows(bmx_1516,bmx_1718) %>%
  transmute(
    SEQN,
    BMI = BMXBMI
  )
table(bmx_all$BMI,useNA = "always")
#人口学
demo_all <- bind_rows(demo_1516,demo_1718) %>%
  transmute(
    SEQN,
    Age = RIDAGEYR,
    Sex = factor(
      RIAGENDR,
      levels = c(1,2),
      labels = c(
        "Male",
        "Female"
      )
    ),
    Race = RIDRETH3,
    Education = DMDEDUC2,
    INDFMPIR,
    SDMVPSU,
    SDMVSTRA,
    WTMEC2YR
  )
#心衰
library(haven)
mcq_1516 <- read_xpt("D:/NHANES数据库/MCQ_J（2017-2018）.xpt")
mcq_1718 <- read_xpt("D:/NHANES数据库/2015-2016/MCQ_I.xpt")
mcq_all <- bind_rows(mcq_1516,mcq_1718)
hf <- mcq_all %>%
  transmute(
    SEQN,
    HF = case_when(
      MCQ160B == 1 ~ 1,
      MCQ160B == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
sum(hf == 0, na.rm = TRUE)
sum(hf == 1, na.rm = TRUE)
sum(is.na(hf))
#合并
analysis_data <- eld_with_scores %>%
  left_join(hf, by="SEQN") %>%
  left_join(demo_all, by="SEQN") %>%
  left_join(smq_all, by="SEQN") %>%
  left_join(alq_all, by="SEQN") %>%
  left_join(paq_all, by="SEQN") %>%
  left_join(bmx_all, by="SEQN") %>%
  left_join(bpq_all, by="SEQN") %>%
  left_join(diq_all, by="SEQN") %>%
  left_join(tchol_all, by="SEQN")
#构建四年权重
analysis_data <- analysis_data %>%
  mutate(
    WT4YR =WTMEC2YR / 2
  )
#建立survey对象
library(survey)
design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = analysis_data
)
#检查缺失，字符向量列出后续分析的所有关键变量
vars <- c(
  "HF","ELD_total_score",
  "Age","Sex","Race","Education","INDFMPIR",
  "Smoking","Alcohol","PhysicalActivity",
  "BMI","Hypertension","Diabetes","Dyslipidemia",
  "Energy"
)
missing_info <- data.frame(
  变量 = vars,
  缺失数 = sapply(
    analysis_data[vars],
    function(x)
      sum(is.na(x))
  ),
  缺失比例 = sapply(
    analysis_data[vars],
    function(x)
      mean(is.na(x))
  )
)
missing_info[
  order(
    -missing_info$缺失比例
  ),
]

#===========删除信息不全的变量==========
# 创建一个记录每步样本量变化的数据框
flow <- data.frame(
  Step = character(),
  Start_n = integer(),
  Excluded_n = integer(),
  End_n = integer(),
  stringsAsFactors = FALSE
)

# 初始化
current_data <- analysis_data
start_n <- nrow(current_data)

# 删除年龄 < 20 岁
excluded <- sum(current_data$Age < 20, na.rm=TRUE)
current_data <- current_data %>% filter(Age >= 20)
flow <- flow %>% add_row(
  Step = "Age >= 20",
  Start_n = start_n,
  Excluded_n = excluded,
  End_n = nrow(current_data)
)
start_n <- nrow(current_data)

#删除 ELD_total 缺失者
excluded <- sum(is.na(current_data$ELD_total_score))
current_data <- current_data %>% filter(!is.na(ELD_total_score))

flow <- flow %>% add_row(
  Step = "ELD_total_score not missing",
  Start_n = start_n,
  Excluded_n = excluded,
  End_n = nrow(current_data)
)
start_n <- nrow(current_data)

# 删除 HF 缺失者
excluded <- sum(is.na(current_data$HF))
current_data <- current_data %>% filter(!is.na(HF))

flow <- flow %>% add_row(
  Step = "HF not missing",
  Start_n = start_n,
  Excluded_n = excluded,
  End_n = nrow(current_data)
)

start_n <- nrow(current_data)

#删除基线资料缺失者（包括：年龄、性别、种族、教育水平、PIR、吸烟、饮酒、体力活动、BMI、高血压、糖尿病、血脂）

baseline_vars <- c(
  "Age","Sex","Race","Education","INDFMPIR",
  "Smoking","Alcohol","PhysicalActivity",
  "BMI","Hypertension","Diabetes","Dyslipidemia"
)

excluded <- current_data %>%
  filter(
    !complete.cases(select(., all_of(baseline_vars))) |
      Education %in% c(7, 9)
  ) %>%
  nrow()
current_data <- current_data %>%
  filter(
    complete.cases(select(., all_of(baseline_vars))) & !Education %in% c(7, 9))

flow <- flow %>% add_row(
  Step = "Complete baseline covariates & Education not 7/9",
  Start_n = start_n,
  Excluded_n = excluded,
  End_n = nrow(current_data)
)

start_n <- nrow(current_data)


# ⑤ 删除日均能量摄入异常（men: <800 or >4200 kcal/d, women: <600 or >3500 kcal/d）
excluded <- sum(
  (current_data$Sex == "Male" & (current_data$Energy < 800 | current_data$Energy > 4200)) |
    (current_data$Sex == "Female" & (current_data$Energy < 600 | current_data$Energy > 3500)),
  na.rm=TRUE
)

current_data <- current_data %>% filter(
  (Sex == "Male" & Energy >= 800 & Energy <= 4200) |
    (Sex == "Female" & Energy >= 600 & Energy <= 3500)
)

flow <- flow %>% add_row(
  Step = "Energy intake within normal range",
  Start_n = start_n,
  Excluded_n = excluded,
  End_n = nrow(current_data)
)
print(flow)

# 最终清理后的数据集
cleaned_data <- current_data
library(survey)
head(cleaned_data$HF)

# 创建NHANES survey设计对象
design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = cleaned_data 
)

# 检查 HF 在 survey 对象中是否存在
names(design$variables) 

# 测试加权频数
svytable(~HF, design)