# 1. 加载包
library(readxl)
library(dplyr)
library(writexl)

# 2. 读取数据
df_2017 <- read.csv("D:/R-data/2017-2020合并数据.csv", fileEncoding = "UTF-8")
df_2021 <- read.csv("D:/R-data/2021-2023合并数据.csv", fileEncoding = "UTF-8")

# 3. 查看数据基本信息
dim(df1)
names(df1)
str(df1)

# 4. 查看缺失值
sum(is.na(df1$LBXHA))
sum(is.na(df1$RIDAGEYR))

# 5. 清洗：保留有效结局变量
df1_clean <- df1 %>% filter(LBXHA %in% c(1, 2))
df2_clean <- df2 %>% filter(LBXHA %in% c(1, 2))

# 6. 删除高缺失无关变量
df1_clean <- df1_clean %>% select(-DMDYRUSZ, -DMDYRUSR, -RIDEXPRG)
df2_clean <- df2_clean %>% select(-DMDYRUSZ, -DMDYRUSR, -RIDEXPRG)

# 7. 查看清洗后样本量
dim(df1_clean)
dim(df2_clean)

# 8. 分类变量转换
df1_clean$RIAGENDR <- factor(df1_clean$RIAGENDR,
                             levels = c(1,2),
                             labels = c("男性","女性"))

df1_clean$age_group <- cut(df1_clean$RIDAGEYR,
                           breaks = c(18,45,65,Inf),
                           labels = c("18-44岁","45-64岁","≥65岁"),
                           right = FALSE)

# 9. 导出清洗后数据
write_xlsx(df1_clean, "2017-2020_清洗完成.xlsx")
write_xlsx(df2_clean, "2021-2023_清洗完成.xlsx")