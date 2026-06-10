# 01_import_explore.R
# 目的：下载并初步查看 NHANES 2017-2018 数据

# 1. 安装并加载需要的包
# 如果已经安装过，可以不用重复运行 install.packages()
install.packages(c("tidyverse", "nhanesA", "janitor", "skimr"))

library(tidyverse)
library(nhanesA)
library(janitor)
library(skimr)

# 2. 下载 NHANES 2017-2018 数据表
demo <- nhanes("DEMO_J")  # 人口学信息
bmx  <- nhanes("BMX_J")   # 身体测量，包含BMI
bpx  <- nhanes("BPX_J")   # 血压测量
bpq  <- nhanes("BPQ_J")   # 血压相关问卷
smq  <- nhanes("SMQ_J")   # 吸烟问卷

# 3. 查看每个数据表的大小
dim(demo)
dim(bmx)
dim(bpx)
dim(bpq)
dim(smq)

# 4. 查看变量名
names(demo)
names(bmx)
names(bpx)
names(bpq)
names(smq)

# 5. 选取本项目需要的变量
demo_small <- demo %>%
  select(SEQN, age = RIDAGEYR, sex = RIAGENDR)

bmx_small <- bmx %>%
  select(SEQN, bmi = BMXBMI)

bpx_small <- bpx %>%
  select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXDI1, BPXDI2, BPXDI3)

bpq_small <- bpq %>%
  select(SEQN, BPQ020, BPQ040A)

smq_small <- smq %>%
  select(SEQN, SMQ020)

# 6. 合并数据
data_raw <- demo_small %>%
  left_join(bmx_small, by = "SEQN") %>%
  left_join(bpx_small, by = "SEQN") %>%
  left_join(bpq_small, by = "SEQN") %>%
  left_join(smq_small, by = "SEQN")

# 7. 只保留成年人
data_adult <- data_raw %>%
  filter(age >= 18)

# 8. 初步查看数据
glimpse(data_adult)
skim(data_adult)

# 9. 计算平均收缩压和舒张压
data_adult <- data_adult %>%
  mutate(
    mean_sbp = rowMeans(select(., BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE),
    mean_dbp = rowMeans(select(., BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE)
  )

# 10. 初步定义BMI分组
data_adult <- data_adult %>%
  mutate(
    bmi_group = case_when(
      bmi < 18.5 ~ "低体重",
      bmi >= 18.5 & bmi < 25 ~ "正常体重",
      bmi >= 25 & bmi < 30 ~ "超重",
      bmi >= 30 ~ "肥胖",
      TRUE ~ NA_character_
    )
  )

# 11. 初步定义高血压变量
data_adult <- data_adult %>%
  mutate(
    hypertension = case_when(
      mean_sbp >= 140 ~ 1,
      mean_dbp >= 90 ~ 1,
      BPQ020 == 1 ~ 1,
      BPQ040A == 1 ~ 1,
      TRUE ~ 0
    )
  )

# 12. 查看关键变量缺失情况
data_adult %>%
  summarise(
    n = n(),
    missing_age = sum(is.na(age)),
    missing_sex = sum(is.na(sex)),
    missing_bmi = sum(is.na(bmi)),
    missing_sbp = sum(is.na(mean_sbp)),
    missing_dbp = sum(is.na(mean_dbp))
  )

# 13. 绘制BMI分布图
p1 <- ggplot(data_adult, aes(x = bmi)) +
  geom_histogram(bins = 30) +
  labs(
    title = "BMI分布图",
    x = "BMI",
    y = "人数"
  )

p1

# 14. 绘制不同BMI组高血压患病率图
plot_data <- data_adult %>%
  filter(!is.na(bmi_group)) %>%
  group_by(bmi_group) %>%
  summarise(
    n = n(),
    hypertension_rate = mean(hypertension, na.rm = TRUE)
  )

p2 <- ggplot(plot_data, aes(x = bmi_group, y = hypertension_rate)) +
  geom_col() +
  labs(
    title = "不同BMI组高血压患病率",
    x = "BMI分组",
    y = "高血压患病率"
  )

p2

# 15. 保存图片
ggsave("figures/bmi_distribution.png", p1, width = 6, height = 4)
ggsave("figures/hypertension_rate_by_bmi.png", p2, width = 6, height = 4)

# 16. 保存初步整理后的数据
write_csv(data_adult, "output/nhanes_bmi_hypertension_initial.csv")

