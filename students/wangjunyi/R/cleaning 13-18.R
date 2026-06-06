library(haven)
library(dplyr)
library(survey)

# ========== 1. 人口学数据 ==========
demo_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Demographics/demo_h.xpt") %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTL,
         WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  mutate(cycle = "2013-2014")

demo_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Demographics/demo_i.xpt") %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTL,
         WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  mutate(cycle = "2015-2016")

demo_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Demographics/demo_j.xpt") %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTL,
         WTMEC2YR, SDMVPSU, SDMVSTRA) %>%
  mutate(cycle = "2017-2018")

demo_combined <- bind_rows(demo_h, demo_i, demo_j)

nrow(demo_combined)

demo_combined <- demo_combined %>%
  mutate(final_weight = WTMEC2YR / 3)

demo_male <- demo_combined %>%
  filter(RIAGENDR == 1, RIDAGEYR >= 20) %>%
  select(SEQN, cycle, RIDAGEYR, RIDRETH3, DMDEDUC2, DMDMARTL,
         SDMVPSU, SDMVSTRA, final_weight)

nrow(demo_male)

demo_male <- demo_male %>%
  mutate(DMDEDUC2 = ifelse(DMDEDUC2 %in% c(7, 9), NA, DMDEDUC2),
         DMDMARTL = ifelse(DMDMARTL == 77, NA, DMDMARTL))

# ========== 2. 吸烟状态 ==========
smq_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Questionnaire/smq_h.xpt")
smq_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Questionnaire/smq_i.xpt")
smq_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Questionnaire/smq_j.xpt")
smq_combined <- bind_rows(smq_h, smq_i, smq_j)

smq_clean <- smq_combined %>%
  mutate(
    SMQ020_clean = ifelse(SMQ020 %in% c(1,2), SMQ020, NA),
    SMQ040_clean = ifelse(SMQ040 %in% c(1,2,3), SMQ040, NA),
    smoking_status = case_when(
      SMQ020_clean == 2 ~ "Never",
      SMQ020_clean == 1 & SMQ040_clean == 3 ~ "Former",
      SMQ020_clean == 1 & SMQ040_clean %in% c(1,2) ~ "Current",
      TRUE ~ NA_character_
    )
  ) %>%
  select(SEQN, smoking_status)

demo_male <- demo_male %>% left_join(smq_clean, by = "SEQN")

table(demo_male$smoking_status, useNA = "ifany")

# ========== 3. 饮酒状态 ==========
alq_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Questionnaire/alq_h.xpt") %>%
  mutate(cycle = "2013-2014", ALQ111 = NA_real_) %>%
  select(SEQN, cycle, ALQ101, ALQ111)

alq_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Questionnaire/alq_i.xpt") %>%
  mutate(cycle = "2015-2016", ALQ111 = NA_real_) %>%
  select(SEQN, cycle, ALQ101, ALQ111)

alq_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Questionnaire/alq_j.xpt") %>%
  mutate(cycle = "2017-2018", ALQ101 = NA_real_) %>%
  select(SEQN, cycle, ALQ101, ALQ111)

alq_combined <- bind_rows(alq_h, alq_i, alq_j)

alq_clean <- alq_combined %>%
  mutate(
    ever_drinker_raw = case_when(
      cycle %in% c("2013-2014", "2015-2016") & ALQ101 %in% c(1,2) ~ ALQ101,
      cycle == "2017-2018" & ALQ111 %in% c(1,2) ~ ALQ111,
      TRUE ~ NA_real_
    ),
    ever_drinker = case_when(
      ever_drinker_raw == 1 ~ 1,
      ever_drinker_raw == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(SEQN, cycle, ever_drinker)

demo_male <- demo_male %>% left_join(alq_clean, by = c("SEQN", "cycle"))

table(demo_male$ever_drinker, useNA = "ifany")

# ========== 4. 血常规数据 ==========
cbc_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Laboratory/cbc_h.xpt") %>%
  mutate(cycle = "2013-2014")
cbc_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Laboratory/cbc_i.xpt") %>%
  mutate(cycle = "2015-2016")
cbc_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Laboratory/cbc_j.xpt") %>%
  mutate(cycle = "2017-2018")

cbc_combined <- bind_rows(cbc_h, cbc_i, cbc_j) %>%
  select(SEQN, cycle, LBDNENO, LBDLYMNO)

demo_male <- demo_male %>% left_join(cbc_combined, by = c("SEQN", "cycle"))

summary(demo_male[, c("LBDNENO", "LBDLYMNO")])

# ========== 5. 白蛋白数据 ==========
biopro_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Laboratory/biopro_h.xpt") %>%
  mutate(cycle = "2013-2014")
biopro_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Laboratory/biopro_i.xpt") %>%
  mutate(cycle = "2015-2016")
biopro_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Laboratory/biopro_j.xpt") %>%
  mutate(cycle = "2017-2018")

biopro_combined <- bind_rows(biopro_h, biopro_i, biopro_j) %>%
  select(SEQN, cycle, LBXSAL)

demo_male <- demo_male %>% left_join(biopro_combined, by = c("SEQN", "cycle"))

# ========== 6. 前列腺癌数据 ==========
mcq_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Questionnaire/mcq_h.xpt") %>%
  mutate(cycle = "2013-2014")
mcq_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Questionnaire/mcq_i.xpt") %>%
  mutate(cycle = "2015-2016")
mcq_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Questionnaire/mcq_j.xpt") %>%
  mutate(cycle = "2017-2018")

mcq_combined <- bind_rows(mcq_h, mcq_i, mcq_j) %>%
  select(SEQN, cycle, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D)

mcq_prostate <- mcq_combined %>%
  mutate(
    MCQ220_clean = ifelse(MCQ220 %in% c(1,2), MCQ220, NA),
    prostate_cancer = case_when(
      MCQ220_clean == 2 ~ 0,
      MCQ220_clean == 1 & (MCQ230A == 30 | MCQ230B == 30 | MCQ230C == 30 | MCQ230D == 30) ~ 1,
      MCQ220_clean == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(SEQN, cycle, prostate_cancer)

demo_male <- demo_male %>% left_join(mcq_prostate, by = c("SEQN", "cycle"))

table(demo_male$prostate_cancer, useNA = "ifany")


# ========== 7. BMI数据 ==========
bmx_h <- read_xpt("E:/4、研究牲/NHANES/2013-2014/Examination/bmx_h.xpt") %>%
  mutate(cycle = "2013-2014") %>%
  select(SEQN, cycle, BMXBMI)

bmx_i <- read_xpt("E:/4、研究牲/NHANES/2015-2016/Examination/bmx_i.xpt") %>%
  mutate(cycle = "2015-2016") %>%
  select(SEQN, cycle, BMXBMI)

bmx_j <- read_xpt("E:/4、研究牲/NHANES/2017-2018/Examination/bmx_j.xpt") %>%
  mutate(cycle = "2017-2018") %>%
  select(SEQN, cycle, BMXBMI)

bmx_combined <- bind_rows(bmx_h, bmx_i, bmx_j)

demo_male <- demo_male %>% left_join(bmx_combined, by = c("SEQN", "cycle"))

summary(demo_male$BMXBMI)

# ========== 8. 计算 ALI ==========
demo_male <- demo_male %>%
  mutate(ALI = LBXSAL * BMXBMI * LBDLYMNO / LBDNENO)

summary(demo_male$ALI)

# ========== 9. 删除所有含有缺失值的样本 ==========
demo_complete <- demo_male %>%
  filter(complete.cases(.))

# 查看删除后样本量
nrow(demo_complete)

table(demo_complete$prostate_cancer)

