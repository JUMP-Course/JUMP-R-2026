# =========================================================
# 02_cleaning re.R
# 项目：基于 NHANES 数据的成年人 BMI 与高血压患病风险关联分析
# 作者：王子铨
# 目的：
# 1. 完成6月10日课后数据清洗任务
# 2. 修订高血压变量定义
# 3. 处理吸烟变量和问卷跳题缺失
# 4. 比较纳入者和排除者
# 5. 生成清洗后数据和相关表格
# =========================================================


# ---------------------------------------------------------
# 0. 加载R包
# ---------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)


# ---------------------------------------------------------
# 1. 创建输出文件夹
# ---------------------------------------------------------

dir.create("tables", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)


# ---------------------------------------------------------
# 2. 读取6月5日生成的初步数据
# ---------------------------------------------------------

data_adult <- read_csv("output/nhanes_bmi_hypertension_initial.csv")

# 查看数据基本情况
dim(data_adult)
glimpse(data_adult)


# ---------------------------------------------------------
# 3. 计算平均收缩压和平均舒张压
# ---------------------------------------------------------

data_adult$mean_sbp <- rowMeans(
  data_adult[, c("BPXSY1", "BPXSY2", "BPXSY3")],
  na.rm = TRUE
)

data_adult$mean_dbp <- rowMeans(
  data_adult[, c("BPXDI1", "BPXDI2", "BPXDI3")],
  na.rm = TRUE
)

# 如果三次血压都缺失，rowMeans会生成NaN，这里改成NA
data_adult$mean_sbp[is.nan(data_adult$mean_sbp)] <- NA
data_adult$mean_dbp[is.nan(data_adult$mean_dbp)] <- NA

summary(data_adult$mean_sbp)
summary(data_adult$mean_dbp)


# ---------------------------------------------------------
# 4. 整理性别、BMI分组和吸烟变量
# ---------------------------------------------------------

data_clean <- data_adult %>%
  mutate(
    # 性别变量
    sex_chr = as.character(sex),
    
    sex_clean = case_when(
      sex_chr == "1" ~ "Male",
      sex_chr == "2" ~ "Female",
      sex_chr == "Male" ~ "Male",
      sex_chr == "Female" ~ "Female",
      sex_chr == "male" ~ "Male",
      sex_chr == "female" ~ "Female",
      TRUE ~ NA_character_
    ),
 
    # BMI分组
    bmi_group_clean = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obesity",
      TRUE ~ NA_character_
    ),
    
    # 把正常体重设为参照组，方便后续Logistic回归
    bmi_group_clean = factor(
      bmi_group_clean,
      levels = c("Normal", "Underweight", "Overweight", "Obesity")
    ),
    
    # 吸烟变量：SMQ020 = 是否一生中至少吸过100支烟
    smoking_clean = case_when(
      SMQ020 == 1 ~ "Ever smoker",
      SMQ020 == 2 ~ "Never smoker",
      TRUE ~ NA_character_
    ),
    
    # 根据老师反馈：吸烟缺失可以暂时设为 Unknown
    smoking_model = replace_na(smoking_clean, "Unknown")
  )

# 检查整理结果
table(data_clean$sex_clean, useNA = "ifany")
table(data_clean$bmi_group_clean, useNA = "ifany")
table(data_clean$smoking_model, useNA = "ifany")


# ---------------------------------------------------------
# 5. 根据文献和问卷逻辑重新定义高血压
# ---------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    # BPQ020：是否曾被医生告知患有高血压
    bpq020_clean = case_when(
      BPQ020 == 1 ~ 1,
      BPQ020 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # BPQ040A：是否正在使用降压药 / 按医嘱用药
    bpq040a_clean = case_when(
      BPQ040A == 1 ~ 1,
      BPQ040A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # BPQ040A跳题缺失处理：
    # 如果明确用药，记为1
    # 如果明确未用药，记为0
    # 如果BPQ020=0且BPQ040A缺失，可能是跳题导致，暂时视为无降压药使用
    # 如果BPQ020=1但BPQ040A缺失，则保留为NA
    antihypertensive_med = case_when(
      bpq040a_clean == 1 ~ 1,
      bpq040a_clean == 0 ~ 0,
      bpq020_clean == 0 & is.na(bpq040a_clean) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 主分析血压定义：140/90标准
    measured_hbp_140 = case_when(
      mean_sbp >= 140 | mean_dbp >= 90 ~ 1,
      !is.na(mean_sbp) & !is.na(mean_dbp) &
        mean_sbp < 140 & mean_dbp < 90 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 敏感性分析血压定义：130/80标准
    measured_hbp_130 = case_when(
      mean_sbp >= 130 | mean_dbp >= 80 ~ 1,
      !is.na(mean_sbp) & !is.na(mean_dbp) &
        mean_sbp < 130 & mean_dbp < 80 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 主分析高血压定义：
    # 平均SBP>=140 或 平均DBP>=90 或 降压药使用
    hypertension_main = case_when(
      measured_hbp_140 == 1 | antihypertensive_med == 1 ~ 1,
      measured_hbp_140 == 0 & antihypertensive_med == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 扩展定义：
    # 在主定义基础上加入自报医生诊断高血压
    hypertension_extended = case_when(
      measured_hbp_140 == 1 |
        antihypertensive_med == 1 |
        bpq020_clean == 1 ~ 1,
      
      measured_hbp_140 == 0 &
        antihypertensive_med == 0 &
        bpq020_clean == 0 ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    # 敏感性分析定义：
    # 平均SBP>=130 或 平均DBP>=80 或 降压药使用
    hypertension_sens_130 = case_when(
      measured_hbp_130 == 1 | antihypertensive_med == 1 ~ 1,
      measured_hbp_130 == 0 & antihypertensive_med == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# 查看不同定义下的高血压人数
table(data_clean$hypertension_main, useNA = "ifany")
table(data_clean$hypertension_extended, useNA = "ifany")
table(data_clean$hypertension_sens_130, useNA = "ifany")


# ---------------------------------------------------------
# 6. 标记最终纳入者和排除者
# ---------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    include_analysis = case_when(
      age >= 18 &
        !is.na(sex_clean) &
        !is.na(bmi) &
        !is.na(bmi_group_clean) &
        !is.na(hypertension_main) ~ 1,
      TRUE ~ 0
    ),
    
    analysis_status = case_when(
      include_analysis == 1 ~ "Included",
      include_analysis == 0 ~ "Excluded"
    ),
    
    exclusion_reason = case_when(
      age < 18 ~ "Age <18",
      is.na(sex_clean) ~ "Missing sex",
      is.na(bmi) ~ "Missing BMI",
      is.na(bmi_group_clean) ~ "Missing BMI group",
      is.na(hypertension_main) ~ "Missing hypertension status",
      include_analysis == 1 ~ "Included",
      TRUE ~ "Other"
    )
  )

# 查看纳入和排除情况
table(data_clean$analysis_status)
table(data_clean$exclusion_reason, useNA = "ifany")


# ---------------------------------------------------------
# 7. 生成纳入者与排除者比较表
# ---------------------------------------------------------

excluded_vs_included <- data_clean %>%
  group_by(analysis_status) %>%
  summarise(
    n = n(),
    
    age_mean = round(mean(age, na.rm = TRUE), 2),
    age_sd = round(sd(age, na.rm = TRUE), 2),
    
    female_n = sum(sex_clean == "Female", na.rm = TRUE),
    female_pct = round(mean(sex_clean == "Female", na.rm = TRUE) * 100, 2),
    
    bmi_mean = round(mean(bmi, na.rm = TRUE), 2),
    bmi_sd = round(sd(bmi, na.rm = TRUE), 2),
    
    ever_smoker_n = sum(smoking_model == "Ever smoker", na.rm = TRUE),
    ever_smoker_pct = round(mean(smoking_model == "Ever smoker", na.rm = TRUE) * 100, 2),
    
    missing_bmi_n = sum(is.na(bmi)),
    missing_hypertension_n = sum(is.na(hypertension_main)),
    
    .groups = "drop"
  )

excluded_vs_included

write_csv(excluded_vs_included, "tables/excluded_vs_included.csv")


# ---------------------------------------------------------
# 8. 生成排除原因表
# ---------------------------------------------------------

exclusion_reason_summary <- data_clean %>%
  count(exclusion_reason, name = "n") %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  )

exclusion_reason_summary

write_csv(exclusion_reason_summary, "tables/exclusion_reason_summary.csv")


# ---------------------------------------------------------
# 9. 生成清洗后缺失情况表
# ---------------------------------------------------------

vars_to_check <- c(
  "age",
  "sex_clean",
  "bmi",
  "bmi_group_clean",
  "mean_sbp",
  "mean_dbp",
  "bpq020_clean",
  "antihypertensive_med",
  "hypertension_main",
  "hypertension_extended",
  "hypertension_sens_130",
  "smoking_clean",
  "smoking_model"
)

missing_summary_clean <- tibble(
  variable = vars_to_check,
  n_missing = map_int(vars_to_check, ~ sum(is.na(data_clean[[.x]]))),
  pct_missing = round(
    map_dbl(vars_to_check, ~ mean(is.na(data_clean[[.x]])) * 100),
    2
  )
)

missing_summary_clean

write_csv(missing_summary_clean, "tables/missing_summary_clean.csv")


# ---------------------------------------------------------
# 10. 生成样本量变化表
# ---------------------------------------------------------

step0 <- data_clean
step1 <- step0 %>% filter(age >= 18)
step2 <- step1 %>% filter(!is.na(sex_clean))
step3 <- step2 %>% filter(!is.na(bmi))
step4 <- step3 %>% filter(!is.na(bmi_group_clean))
step5 <- step4 %>% filter(!is.na(hypertension_main))

data_final <- step5

cleaning_flow <- tibble(
  step = c(
    "Initial dataset",
    "Adults aged >=18 years",
    "Exclude missing sex",
    "Exclude missing BMI",
    "Exclude missing BMI group",
    "Exclude missing hypertension status",
    "Final analytic sample"
  ),
  n = c(
    nrow(step0),
    nrow(step1),
    nrow(step2),
    nrow(step3),
    nrow(step4),
    nrow(step5),
    nrow(data_final)
  )
) %>%
  mutate(
    excluded_from_previous_step = lag(n) - n
  )

cleaning_flow

write_csv(cleaning_flow, "tables/cleaning_flow.csv")


# ---------------------------------------------------------
# 11. 保存清洗后的最终分析数据
# ---------------------------------------------------------

write_csv(data_final, "output/nhanes_bmi_hypertension_clean.csv")


# ---------------------------------------------------------
# 12. 自动生成变量定义与清洗规则依据说明文件
# ---------------------------------------------------------

definition_text <- c(
  "# 变量定义与清洗规则依据",
  "",
  "## 项目题目",
  "",
  "基于 NHANES 数据的成年人 BMI 与高血压患病风险关联分析",
  "",
  "## 1. 高血压定义",
  "",
  "本项目主分析将高血压定义为：平均收缩压 >= 140 mmHg，或平均舒张压 >= 90 mmHg，或正在使用降压药。",
  "",
  "同时保留两个补充定义：",
  "",
  "- extended definition：加入自报医生诊断高血压；",
  "- sensitivity definition：采用 SBP >= 130 mmHg 或 DBP >= 80 mmHg 或降压药使用。",
  "",
  "这样设置的原因是：传统流行病学研究中常使用140/90或降压药作为高血压定义；2017 ACC/AHA指南将高血压阈值下调至130/80，因此本项目把130/80作为敏感性分析定义。",
  "",
  "## 2. 血压测量值处理",
  "",
  "NHANES BPX_J 提供多次血压测量值。本项目初步使用多次测量值的平均值作为平均收缩压和平均舒张压。",
  "",
  "## 3. 吸烟变量定义",
  "",
  "SMQ020 = 1 定义为 Ever smoker；SMQ020 = 2 定义为 Never smoker；缺失或其他情况暂时定义为 Unknown。",
  "",
  "吸烟不是本研究的核心暴露或结局，因此暂时不因吸烟缺失直接删除样本。",
  "",
  "## 4. BPQ040A跳题缺失处理",
  "",
  "BPQ040A 与降压药使用有关，可能受 BPQ020 的前置问题影响。因此，BPQ040A 缺失需要结合 BPQ020 理解。",
  "",
  "如果 BPQ020 = 2 且 BPQ040A 缺失，初步认为可能是跳题导致，暂时视为无降压药使用；如果 BPQ020 = 1 但 BPQ040A 缺失，则用药信息不明确。",
  "",
  "## 5. 纳入者与排除者比较",
  "",
  "由于完整病例分析可能造成选择偏倚，本项目比较最终纳入者与排除者在年龄、性别、BMI和吸烟状态方面的差异，并将结果保存为 excluded_vs_included.csv。",
  "",
  "## 6. 当前局限",
  "",
  "目前清洗规则仍属于课程项目阶段的初步方案。后续需要根据老师反馈进一步确认高血压定义、血压平均值计算方式、跳题缺失处理和是否需要使用NHANES抽样权重。"
)

writeLines(
  definition_text,
  "tables/literature_based_variable_definition.md"
)


# ---------------------------------------------------------
# 13. 最后检查6月10日课后作业文件是否生成
# ---------------------------------------------------------

file.exists("tables/excluded_vs_included.csv")
file.exists("tables/exclusion_reason_summary.csv")
file.exists("tables/missing_summary_clean.csv")
file.exists("tables/cleaning_flow.csv")
file.exists("tables/literature_based_variable_definition.md")
file.exists("output/nhanes_bmi_hypertension_clean.csv")