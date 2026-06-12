#整理人口学变量（demog_clean）

##1 选择需要的列：ID、年龄、性别（ba001）、居住地（ba008）
demog_clean <- demog %>%
  select(ID, xrage, ba001, ba008)

##2创建性别因子变量（1=Male, 2=Female）
demog_clean <- demog_clean %>%
  mutate(gender = factor(ba001, levels = c(1,2), labels = c("Male", "Female")))

##3 创建居住地因子变量（1,2→Urban；3,4→Rural）
demog_clean <- demog_clean %>%
  mutate(residence = case_when(
    ba008 %in% c(1,2) ~ "Urban",
    ba008 %in% c(3,4) ~ "Rural",
    TRUE ~ NA_character_
  ))

##4 保留最终需要的四列：ID, 年龄, 性别, 居住地
demog_clean <- demog_clean %>%
  select(ID, xrage, gender, residence)


#整理健康相关数据（health_clean）

##1 提取需要的变量并处理缺失值
health_sub <- health %>%
  select(ID, da030, da003_1_, da047, da051) %>%
  mutate(across(everything(), ~ ifelse(. < 0, NA, .)))   #ID, 睡眠时间(da030), 高血压诊断(da003_1_), 吸烟状态(da047), 饮酒频率(da051)。

##2 创建睡眠时间变量
health_sub <- health_sub %>%
  mutate(sleep_hours = da030)

##3 创建高血压因子（1=Yes, 2=No）
health_sub <- health_sub %>%
  mutate(hypertension = case_when(
    da003_1_ == 1 ~ "Yes",
    da003_1_ == 2 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(hypertension = factor(hypertension, levels = c("No", "Yes")))

##4 创建吸烟状态因子（da047: 1=Still smoke, 2=Quit, 3=Never smoked）
health_sub <- health_sub %>%
  mutate(smoking = case_when(
    da047 == 3 ~ "Never",
    da047 == 2 ~ "Former",
    da047 == 1 ~ "Current",
    TRUE ~ NA_character_
  )) %>%
  mutate(smoking = factor(smoking, levels = c("Never", "Former", "Current")))

##5 创建饮酒频率因子（da051: 1=Regular, 2=Occasional, 3=None）
health_sub <- health_sub %>%
  mutate(drinking = case_when(
    da051 == 1 ~ "Regular",
    da051 == 2 ~ "Occasional",
    da051 == 3 ~ "Never",
    TRUE ~ NA_character_
  )) %>%
  mutate(drinking = factor(drinking, levels = c("Never", "Occasional", "Regular")))

##6 保留最终健康变量
health_clean <- health_sub %>%
  select(ID, sleep_hours, hypertension, smoking, drinking)


#合并人口学与健康数据

#1 合并数据
按 ID 进行整理（只保留两表共有的个体）。
final_data <- demog_clean %>%
  inner_join(health_clean, by = "ID")

##2 增加睡眠时间分组变量
data <- data%>%
  mutate(sleep_group = case_when(
    sleep_hours < 6                     ~ "<6",
    sleep_hours >= 6 & sleep_hours < 7  ~ "6-<7",
    sleep_hours >= 7 & sleep_hours < 8  ~ "7-<8",   
    sleep_hours >= 8 & sleep_hours < 9  ~ "8-<9",
    sleep_hours >= 9                    ~ ">=9",))

data$sleep_group <- factor(data$sleep_group, 
                           levels = c("7-<8", "<6", "6-<7", "8-<9", ">=9"))
