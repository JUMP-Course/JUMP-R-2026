library(dplyr)

# 定义结局、尿镉校正
data_clean <- data_clean %>%
  mutate(
    hashimoto = case_when(
      LBXTPO > 9 | LBXATG > 4 ~ 1,        # 任意一个阳性 = 1
      !is.na(LBXTPO) & !is.na(LBXATG) ~ 0,# 两个都有值且都阴性 = 0
      TRUE ~ NA_real_                     # 只有两个都缺失才 = NA
    ),
    URXUCD_cr = URXUCD / URXUCR * 1000
  )

# 转换分类协变量
data_clean <- data_clean %>%
  mutate(
    # 性别
    sex = factor(RIAGENDR, levels = c("Male", "Female"), labels = c("男", "女")),
    
    # 种族
    race = factor(
      case_when(
        RIDRETH1 %in% c("Mexican American", "Other Hispanic", 3, 4) ~ "西班牙裔",
        RIDRETH1 %in% c("Non-Hispanic White", 1) ~ "白人",
        RIDRETH1 %in% c("Non-Hispanic Black", 2) ~ "黑人",
        TRUE ~ "其他"
      ),
      levels = c("白人", "黑人", "西班牙裔", "其他")
    ),
    
    # 教育
    education = factor(
      case_when(
        grepl("Less than 9th|Less Than 9th|9-11th grade", DMDEDUC2, ignore.case = TRUE) ~ "低于高中",
        grepl("High school graduate|High School Grad|GED", DMDEDUC2, ignore.case = TRUE) ~ "高中",
        grepl("College graduate|Some college|AA degree", DMDEDUC2, ignore.case = TRUE) ~ "大学及以上",
        TRUE ~ NA_character_
      ),
      levels = c("低于高中", "高中", "大学及以上")
    ),
    
    # 吸烟
    smoke = factor(
      case_when(
        SMQ020 %in% c("No", 2) ~ "从不吸烟",
        SMQ020 %in% c("Yes", 1) ~ "吸烟",
        TRUE ~ NA_character_
      ),
      levels = c("从不吸烟", "吸烟")
    ),
    
    # 高血压
    hypertension = factor(
      case_when(
        BPQ020 == "Yes" ~ "有",
        BPQ020 == "No" ~ "无",
        TRUE ~ NA_character_
      ),
      levels = c("有", "无")
    )
  )

# 检查NA数量
cat("sex:", sum(is.na(data_clean$sex)), "\n")
cat("race:", sum(is.na(data_clean$race)), "\n")
cat("education:", sum(is.na(data_clean$education)), "\n")
cat("smoke:", sum(is.na(data_clean$smoke)), "\n")
cat("hypertension:", sum(is.na(data_clean$hypertension)), "\n")
cat("wt_6yr:", sum(is.na(data_clean$wt_6yr)), "\n")
cat("URXUCD_cr:", sum(is.na(data_clean$URXUCD_cr)), "\n")
cat("hashimoto:", sum(is.na(data_clean$hashimoto)), "\n")

# 删除缺失值
n_before <- nrow(data_clean)
data_final <- data_clean %>%
  drop_na(
    URXUCD_cr, hashimoto, wt_6yr,
    RIDAGEYR, sex, race, education, smoke, hypertension,
    BMXBMI, INDFMPIR          
  )
n_after <- nrow(data_final)

cat("清洗前样本量（年龄≥20岁且含缺失）:", n_before, "\n")
cat("清洗后样本量（完整病例）:", n_after, "\n")
cat("删除的样本量:", n_before - n_after, "\n")

# 最终数据分布
cat("结局分布:\n"); print(table(data_final$hashimoto, useNA = "ifany"))
cat("\n性别分布:\n"); print(table(data_final$sex, useNA = "ifany"))
cat("\n种族分布:\n"); print(table(data_final$race, useNA = "ifany"))
cat("\n教育分布:\n"); print(table(data_final$education, useNA = "ifany"))
cat("\n吸烟分布:\n"); print(table(data_final$smoke, useNA = "ifany"))
cat("\n高血压分布:\n"); print(table(data_final$hypertension, useNA = "ifany"))
cat("\n年龄描述:\n"); print(summary(data_final$RIDAGEYR))
cat("\nBMI描述:\n"); print(summary(data_final$BMXBMI))
cat("\n贫困收入比描述:\n"); print(summary(data_final$INDFMPIR))
cat("\n校正尿镉描述:\n"); print(summary(data_final$URXUCD_cr))

writexl::write_xlsx(data_final, "NHANES_2007_2012_urine_cadmium_final_clean.xlsx")
cat("文件已保存至:", getwd(), "/NHANES_2007_2012_urine_cadmium_final_clean.xlsx\n")