install.packages("nhanesA")
install.packages("dplyr")
library(nhanesA)
library(dplyr)
demo <- nhanes("DEMO_L")
dim(demo)
names(demo)
bmx <- nhanes("BMX_L")
dim(bmx)
names(bmx)
sleep <- nhanes("SLQ_L")
dim(sleep)
names(sleep)
smoke <- nhanes("SMQ_L")
dim(smoke)
names(smoke)
alcohol <- nhanes("ALQ_L")
dim(alcohol)
names(alcohol)
all_data <- demo %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(sleep, by = "SEQN") %>%
  left_join(smoke, by = "SEQN") %>%
  left_join(alcohol, by = "SEQN")
dim(all_data)
names(all_data)
core_data <- all_data %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2,RIDEXPRG,
         BMXBMI, SLD012, SMQ020, SMQ040, ALQ111, ALQ121)
dim(core_data)
core_data <- core_data %>%
  rename(
    id = SEQN,
    age = RIDAGEYR,
    sex = RIAGENDR,
    race = RIDRETH3,
    education = DMDEDUC2,
    pregnancy = RIDEXPRG,
    bmi = BMXBMI,
    sleep_weekday = SLD012,
    smoke_ever = SMQ020,
    smoke_now = SMQ040,
    drink_ever = ALQ111,
    drink_recent = ALQ121
  )
study_data <- core_data %>%
  filter(age >= 20) %>%
  filter(is.na(pregnancy) | pregnancy != 1)
dim(study_data)
names(study_data)
str(study_data)
colSums(is.na(study_data))
summary(study_data$age)
summary(study_data$bmi)
summary(study_data$sleep_weekday)
table(study_data$sex, useNA = "ifany")
table(study_data$race, useNA = "ifany")
table(study_data$education, useNA = "ifany")
table(study_data$smoke_ever, useNA = "ifany")
table(study_data$drink_ever, useNA = "ifany")
table(study_data$pregnancy, useNA = "ifany")
table(study_data$smoke_now, useNA = "ifany")
table(study_data$drink_recent, useNA = "ifany")
sum(duplicated(study_data$id))






