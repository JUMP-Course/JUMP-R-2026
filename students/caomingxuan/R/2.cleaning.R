#筛选年龄
analysis_data <- nhanes_merged %>%
  dplyr::filter(RIDAGEYR >= 20)  #筛序函数
cat("年龄筛选后样本量:", nrow(analysis_data), "\n")
cat("年龄范围:", range(analysis_data$RIDAGEYR, na.rm = TRUE), "岁\n")
#计算BMI
analysis_data <- analysis_data %>%
  dplyr::mutate(
    BMI = BMXWT / (BMXHT / 100)^2
  )
#查看BMI分布
cat("BMI统计量:\n")
cat("  最小值:", round(min(analysis_data$BMI, na.rm = TRUE), 1), "\n")
cat("  中位数:", round(median(analysis_data$BMI, na.rm = TRUE), 1), "\n")
cat("  均值:", round(mean(analysis_data$BMI, na.rm = TRUE), 1), "\n")
cat("  最大值:", round(max(analysis_data$BMI, na.rm = TRUE), 1), "\n")
#BMI分类
analysis_data <- analysis_data %>%
  dplyr::mutate(  #新增列
    bmi_cat = dplyr::case_when(
      BMI >= 18.5 & BMI < 25 ~ "Normal weight",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    )
  )
#查看各分类样本量
cat("BMI分类样本量:\n")
print(table(analysis_data$bmi_cat, useNA = "ifany"))  #如果有缺失值（NA），也把它数出来显示
#定义结局变量：高血压
analysis_data <- analysis_data %>%
  dplyr::mutate(
    hypertension = dplyr::case_when(  #case_when 是条件判断，从上往下看，满足任意一个就赋值
      BPQ020 == "Yes" ~ 1,      # 自报确诊
      BPQ150 == "Yes" ~ 1,
      TRUE ~ 0               # 不满足上述条件
    )
  )

#查看高血压患病情况
cat("高血压患病情况:\n")
print(table(analysis_data$hypertension, useNA = "ifany"))

#计算患病率
hypertension_rate <- mean(analysis_data$hypertension == 1, na.rm = TRUE) * 100
cat("\n高血压患病率:", round(hypertension_rate, 1), "%\n")
#定义协变量：性别
analysis_data <- analysis_data %>%
  dplyr::mutate(
    gender = dplyr::case_when(
      RIAGENDR == "Male"   ~ 1,
      RIAGENDR == "Female" ~ 2,
      TRUE ~ NA_real_
    )
  )

#查看转换结果
cat("性别数值转换结果：\n")
print(table(analysis_data$gender, useNA = "ifany"))
#定义协变量：教育水平
analysis_data <- analysis_data %>%
  dplyr::mutate(
    education = dplyr::case_when(
      DMDEDUC2 == "Less than 9th grade" ~ 1,
      DMDEDUC2 == "9-11th grade (Includes 12th grade with no diploma)" ~ 2,
      DMDEDUC2 == "High school graduate/GED or equivalent" ~ 3,
      DMDEDUC2 == "Some college or AA degree" ~ 4,
      DMDEDUC2 == "College graduate or above" ~ 5,
      TRUE ~ NA_real_
    )
  )
#查看教育分布
cat("教育水平分布:\n")
print(table(analysis_data$education, useNA = "ifany"))

#定义协变量：种族
analysis_data <- analysis_data %>%
  dplyr::mutate(
    race = dplyr::case_when(
      RIDRETH3 == "Mexican American" ~ 1,
      RIDRETH3 == "Other Hispanic" ~ 2,
      RIDRETH3 == "Non-Hispanic White" ~ 3,
      RIDRETH3 == "Non-Hispanic Black" ~ 4,
      RIDRETH3 == "Non-Hispanic Asian" ~ 5,
      RIDRETH3 == "Other Race" ~ 5,
      TRUE ~ NA_real_
    )
  )
#查看种族分布
cat("种族分布:\n")
print(table(analysis_data$race, useNA = "ifany"))

#删除缺失值
analysis_data <- analysis_data %>%
  dplyr::filter(
    !is.na(bmi_cat),
    !is.na(hypertension),
    !is.na(gender),
    !is.na(race),
    !is.na(education),
    BMI >= 18.5  # 剔除低体重者（BMI<18.5）
  )

#数据清洗结果
cat("【最终样本量】\n")
cat("  总样本量:", nrow(analysis_data), "人\n")
cat("\n")

cat("【高血压患病率】\n")
hypertension_rate_final <- mean(analysis_data$hypertension == 1) * 100
cat("  总患病率:", round(hypertension_rate_final, 1), "%\n")
cat("\n")

cat("【各BMI组样本量及患病率】\n")
bmi_summary <- analysis_data %>%
  dplyr::group_by(bmi_cat) %>%
  dplyr::summarise(
    n = n(),
    prevalence = round(mean(hypertension == 1) * 100, 1)
  )
print(bmi_summary)
cat("\n")

cat("【各协变量分布】\n")
cat("  性别:\n")
print(table(analysis_data$gender))
cat("\n  种族:\n")
print(table(analysis_data$race))
cat("\n  教育水平:\n")
print(table(analysis_data$education))