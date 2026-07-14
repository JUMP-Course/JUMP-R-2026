install.packages(c("nhanesA","tidyverse","writexl","survey","naniar"))

library(nhanesA)
library(tidyverse)
library(writexl)
library(survey)
library(naniar)

#导入数据
# 2007-2008
thyroid_0708 <- nhanes("THYROD_E")
urine_cadmium_0708 <- nhanes("UHM_E")
demo_0708   <- nhanes("DEMO_E")
smoke_0708  <- nhanes("SMQ_E")
bmx_0708    <- nhanes("BMX_E")
bpq_0708    <- nhanes("BPQ_E")
# 2009-2010
thyroid_0910 <- nhanes("THYROD_F")
urine_cadmium_0910 <- nhanes("UHM_F")
demo_0910   <- nhanes("DEMO_F")
smoke_0910  <- nhanes("SMQ_F")
bmx_0910    <- nhanes("BMX_F")
bpq_0910    <- nhanes("BPQ_F")
# 2011-2012
thyroid_1112 <- nhanes("THYROD_G")
urine_cadmium_1112 <- nhanes("UHM_G")
demo_1112   <- nhanes("DEMO_G")
smoke_1112  <- nhanes("SMQ_G")
bmx_1112    <- nhanes("BMX_G")
bpq_1112    <- nhanes("BPQ_G")

# 检查所有需要的对象是否存在
objects <- c("demo_0708", "bmx_0708", "smoke_0708", "bpq_0708", "thyroid_0708", "urine_cadmium_0708",
             "demo_0910", "bmx_0910", "smoke_0910", "bpq_0910", "thyroid_0910", "urine_cadmium_0910",
             "demo_1112", "bmx_1112", "smoke_1112", "bpq_1112", "thyroid_1112", "urine_cadmium_1112")
exists <- sapply(objects, exists)
print(objects[!exists])

# 检查不同年份同类表列名是否完全一致
# 检查 DEMO 表
identical(names(demo_0708), names(demo_0910))
identical(names(demo_0910), names(demo_1112))
# 检查 BMX 表
identical(names(bmx_0708), names(bmx_0910))
identical(names(bmx_0910), names(bmx_1112))
# 检查 SMQ 表
identical(names(smoke_0708), names(smoke_0910))
identical(names(smoke_0910), names(smoke_1112))
# 检查 BPQ 表
identical(names(bpq_0708), names(bpq_0910))
identical(names(bpq_0910), names(bpq_1112))
# 检查 THYROID 表
identical(names(thyroid_0708), names(thyroid_0910))
identical(names(thyroid_0910), names(thyroid_1112))
# 检查 URINE CADMIUM 表
identical(names(urine_cadmium_0708), names(urine_cadmium_0910))
identical(names(urine_cadmium_0910), names(urine_cadmium_1112))

# 查看各周期所有列名
# DEMO 表
names(demo_0708)
names(demo_0910)
names(demo_1112)
# BMX 表
names(bmx_0708)
names(bmx_0910)
names(bmx_1112)
# SMQ 表
names(smoke_0708)
names(smoke_0910)
names(smoke_1112)
# BPQ 表
names(bpq_0708)
names(bpq_0910)
names(bpq_1112)
# THYROID 表
names(thyroid_0708)
names(thyroid_0910)
names(thyroid_1112)
# URINE CADMIUM 表
names(urine_cadmium_0708)
names(urine_cadmium_0910)
names(urine_cadmium_1112)

# 定义每个模块必须保留的变量
demo_required <- c("SEQN", "WTMEC2YR", "RIDAGEYR", "RIAGENDR", "RIDRETH1", 
                   "INDFMPIR", "DMDEDUC2", "SDMVSTRA", "SDMVPSU")
bmx_required <- c("SEQN", "BMXBMI")
smoke_required <- c("SEQN", "SMQ020")
bpq_required <- c("SEQN", "BPQ020")
thyroid_required <- c("SEQN", "LBXTPO", "LBXATG")
cadmium_required <- c("SEQN", "URXUCD", "URXUCR")   # 尿镉 + 尿肌酐

# 筛选每个周期所需的列（使用 all_of，缺失会报错）
demo_0708_sel <- demo_0708 %>% select(all_of(demo_required))
demo_0910_sel <- demo_0910 %>% select(all_of(demo_required))
demo_1112_sel <- demo_1112 %>% select(all_of(demo_required))
bmx_0708_sel <- bmx_0708 %>% select(all_of(bmx_required))
bmx_0910_sel <- bmx_0910 %>% select(all_of(bmx_required))
bmx_1112_sel <- bmx_1112 %>% select(all_of(bmx_required))
smoke_0708_sel <- smoke_0708 %>% select(all_of(smoke_required))
smoke_0910_sel <- smoke_0910 %>% select(all_of(smoke_required))
smoke_1112_sel <- smoke_1112 %>% select(all_of(smoke_required))
bpq_0708_sel <- bpq_0708 %>% select(all_of(bpq_required))
bpq_0910_sel <- bpq_0910 %>% select(all_of(bpq_required))
bpq_1112_sel <- bpq_1112 %>% select(all_of(bpq_required))
thyroid_0708_sel <- thyroid_0708 %>% select(all_of(thyroid_required))
thyroid_0910_sel <- thyroid_0910 %>% select(all_of(thyroid_required))
thyroid_1112_sel <- thyroid_1112 %>% select(all_of(thyroid_required))
cadmium_0708_sel <- urine_cadmium_0708 %>% select(all_of(cadmium_required))
cadmium_0910_sel <- urine_cadmium_0910 %>% select(all_of(cadmium_required))
cadmium_1112_sel <- urine_cadmium_1112 %>% select(all_of(cadmium_required))

# 纵向合并（将三个周期的同类型数据堆叠）
demo_all    <- bind_rows(demo_0708_sel, demo_0910_sel, demo_1112_sel)
bmx_all     <- bind_rows(bmx_0708_sel, bmx_0910_sel, bmx_1112_sel)
smoke_all   <- bind_rows(smoke_0708_sel, smoke_0910_sel, smoke_1112_sel)
bpq_all     <- bind_rows(bpq_0708_sel, bpq_0910_sel, bpq_1112_sel)
thyroid_all <- bind_rows(thyroid_0708_sel, thyroid_0910_sel, thyroid_1112_sel)
cadmium_all <- bind_rows(cadmium_0708_sel, cadmium_0910_sel, cadmium_1112_sel)

# DEMO 检查 
cat("样本数：", nrow(demo_all), "\n")
cat("重复ID：", demo_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(demo_all))
# BMX 检查
cat("样本数：", nrow(bmx_all), "\n")
cat("重复ID：", bmx_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(bmx_all))
#SMOKE 检查
cat("样本数：", nrow(smoke_all), "\n")
cat("重复ID：", smoke_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(smoke_all))
# BPQ 检查
cat("样本数：", nrow(bpq_all), "\n")
cat("重复ID：", bpq_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(bpq_all))
# THYROID 检查
cat("样本数：", nrow(thyroid_all), "\n")
cat("重复ID：", thyroid_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(thyroid_all))
# URINE CADMIUM 检查 
cat("样本数：", nrow(cadmium_all), "\n")
cat("重复ID：", cadmium_all %>% count(SEQN) %>% filter(n>1) %>% nrow(), "\n")
cat("变量名："); print(names(cadmium_all))

# 横向合并
data_merged <- cadmium_all %>%
  left_join(thyroid_all, by = "SEQN") %>%
  left_join(demo_all, by = "SEQN") %>%
  left_join(bmx_all, by = "SEQN") %>%
  left_join(smoke_all, by = "SEQN") %>%
  left_join(bpq_all, by = "SEQN")

# 检查总样本量
cat("cadmium_all 样本量:", nrow(cadmium_all), "\n")
cat("data_merged 样本量:", nrow(data_merged), "\n")
# 检查 SEQN 是否唯一
dup_merged <- data_merged %>% count(SEQN) %>% filter(n > 1)
cat("合并后重复 SEQN 数量:", nrow(dup_merged), "\n")
# 查看各来源表成功匹配的比例
cat("甲状腺匹配率:", sum(!is.na(data_merged$LBXTPO)) / nrow(data_merged), "\n")
cat("DEMO匹配率:", sum(!is.na(data_merged$WTMEC2YR)) / nrow(data_merged), "\n")
cat("BMX匹配率:", sum(!is.na(data_merged$BMXBMI)) / nrow(data_merged), "\n")
# 检查关键变量是否存在且类型正确
str(data_merged[, c("SEQN", "URXUCD", "LBXTPO", "WTMEC2YR")])

# 计算权重
data_merged <- data_merged %>%
  mutate(wt_6yr = WTMEC2YR/3)

#年龄筛选（≥20岁成人）
# 查看原始年龄分布
summary(data_merged$RIDAGEYR)
# 筛选 ≥20 岁研究人群
data_clean <- data_merged %>% filter(RIDAGEYR >= 20)
# 输出样本量变化
cat("年龄筛选前总样本量：", nrow(data_merged), "\n")
cat("年龄筛选后成人样本量：", nrow(data_clean), "\n")

# 缺失值与异常值检查
# 关键变量列表
key_vars <- c("URXUCD", "LBXTPO", "LBXATG", "RIDAGEYR", "RIAGENDR", "BMXBMI", "WTMEC2YR", "SMQ020", "BPQ020", "DMDEDUC2", "INDFMPIR")
cont_vars <- c("URXUCD", "LBXTPO", "LBXATG", "RIDAGEYR", "BMXBMI", "INDFMPIR")
# 特殊编码转NA
data_clean <- data_clean %>%
  mutate(across(where(is.numeric), ~ na_if(na_if(na_if(., 777), 888), 999)))
#关键变量缺失率
missing_pct <- data_clean %>%
  summarise(across(all_of(key_vars), ~ round(mean(is.na(.)) * 100, 2))) %>%
  pivot_longer(cols = everything(), names_to = "变量", values_to = "缺失率(%)")
print(missing_pct)
# 连续变量异常值（IQR法+占比）
for (var in cont_vars) {
  x <- data_clean[[var]]
  valid_n <- sum(!is.na(x))
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  out <- sum(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR), na.rm = TRUE)
  cat(var, "异常值个数:", out, "占比:", round(out/valid_n*100, 2), "%\n")
}
#分布特征
summary(data_clean[, cont_vars])

# 图1：缺失率图
p1 <- gg_miss_var(data_clean, show_pct = TRUE) +
  labs(title = "关键变量缺失率", x = "变量", y = "缺失百分比") +
  theme_bw()
# 图2：尿镉分布直方图（对数坐标）
p2 <- ggplot(data_clean, aes(x = URXUCD)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(x = "尿镉浓度 (μg/L, log10 刻度)", y = "频数", 
       title = "尿镉分布 (对数变换)") +
  theme_bw()
print(p1)
print(p2)

ggsave("缺失率图.png", p1, width = 8, height = 5)
ggsave("尿镉分布直方图.png", p2, width = 6, height = 4)