library(haven)
library(dplyr)
setwd("D:/NHANES-2017-2018")

#1 基础数据
demo   <- read_xpt("DEMO_J.XPT")
bmx    <- read_xpt("BMX_J.XPT")
dr1tot <- read_xpt("DR1TOT_J.XPT")

#2 读取血脂
tc_dat    <- read_xpt("TCHOL_J.XPT")   #LBXTC总胆固醇
tgldl_dat <- read_xpt("TRIGLY_J.XPT")  #LBXTR甘油三酯、LBDLDL低密度
hdl_dat   <- read_xpt("HDL_J.XPT")     #LBDHDD高密度

#合并四项血脂
lipid <- tc_dat %>% select(SEQN, LBXTC) %>%
  inner_join(tgldl_dat %>% select(SEQN, LBXTR, LBDLDL), by = "SEQN") %>%
  inner_join(hdl_dat %>% select(SEQN, LBDHDD), by = "SEQN") %>%
  rename(LBXHD = LBDHDD)

#全天添加糖三分组：低/中/高
sugar_data <- dr1tot[,c("SEQN","DR1TSUGR","DR1TKCAL")]

cut_q <- quantile(sugar_data$DR1TSUGR, probs = c(0,1/3,2/3,1), na.rm=T)

sugar_data$exposure_3 <- cut(sugar_data$DR1TSUGR,
                             breaks = cut_q,
                             labels = c("低添加糖","中添加糖","高添加糖"),
                             include.lowest = TRUE)

sugar_data$exposure_3 <- factor(sugar_data$exposure_3,levels=c("低添加糖","中添加糖",
                                                               "高添加糖"))

df_all <- inner_join(demo,bmx,by="SEQN") %>% inner_join(sugar_data,by="SEQN")
df <- inner_join(df_all,lipid,by="SEQN")

#1.筛选关键变量
keep_var = c("SEQN","exposure_3","RIDAGEYR","RIAGENDR","RIDRETH1","DMDEDUC2","INDFMPIR",
             
             "BMXBMI","DR1TKCAL","DR1TSUGR","LBXTC","LBXTR","LBXHD","LBDLDL")

# 2.筛选成年人
df_adult <- subset(df, RIDAGEYR >= 18)

df_sel <- df_adult[, keep_var]

# 3. 查看样本量
cat("成年人样本量:", nrow(df_sel), "\n")

# 4. 查看各变量缺失情况
colSums(is.na(df_sel))

# 直接计算缺失率（百分比）
colMeans(is.na(df_sel)) * 100
# 保留两位小数
round(colMeans(is.na(df_sel)) * 100, 2)

# 缺失值处理
key_vars <- c("exposure_3", "LBXTC", "LBXTR", "LBXHD", "LBDLDL")

# 保留没有缺失值的样本
df_clean <- df_sel[complete.cases(df_sel[, key_vars]), ]

# 再核对一轮缺失
colSums(is.na(df_clean))
nrow(df_sel)    # 原样本量
nrow(df_clean)  # 删核心缺失后样本量


# 1. 连续型协变量：中位数插补
df_clean$BMXBMI[is.na(df_clean$BMXBMI)] <- median(df_clean$BMXBMI, na.rm = TRUE)

# 2. 定义众数函数
get_mode <- function(x){
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
}

# 3. 分类协变量：众数插补（调用 get_mode）
df_clean$DMDEDUC2[is.na(df_clean$DMDEDUC2)] <- get_mode(df_clean$DMDEDUC2)
df_clean$INDFMPIR[is.na(df_clean$INDFMPIR)] <- get_mode(df_clean$INDFMPIR)
# 3. 核查全部变量缺失情况
colSums(is.na(df_clean))


# 先算出中位数
bmi_med <- median(df_clean$BMXBMI, na.rm = TRUE)
# 再填充缺失
df_clean$BMXBMI[is.na(df_clean$BMXBMI)] <- bmi_med




#查看异常值
summary(df_clean[, c("BMXBMI", "DR1TKCAL", "LBXTC", "LBXTR", "LBXHD", "LBDLDL")])

#缩尾法
# 1. 计算总能量摄入 1% 分位数（下限）
q01 <- quantile(df_clean$DR1TKCAL, 0.01, na.rm = TRUE)

# 2. 计算总能量摄入 99% 分位数（上限）
q99 <- quantile(df_clean$DR1TKCAL, 0.99, na.rm = TRUE)

# 3. 缩尾
# 新建数据集
df_clean$DR1TKCAL_winsor <- df_clean$DR1TKCAL
# 小于1%分位数，替换为下限
df_clean$DR1TKCAL_winsor[df_clean$DR1TKCAL < q01] <- q01
# 大于99%分位数，替换为上限
df_clean$DR1TKCAL_winsor[df_clean$DR1TKCAL > q99] <- q99

# 4. 对比缩尾前后数据
summary(df_clean[, c("DR1TKCAL", "DR1TKCAL_winsor")])

# 1. 原始筛选：成年人数据集
n1 <- nrow(df_adult)   
cat("1. 原始总样本量：", n1, "\n")

# 2. 筛选目标变量后：df_sel
n2 <- nrow(df_sel)
cat("2. 筛选变量后样本量：", n2, "\n")

# 3. 剔除核心变量（暴露+血脂）缺失值后：df_clean
key_vars <- c("exposure_3", "LBXTC", "LBXTR", "LBXHD", "LBDLDL")
df_clean <- df_sel[complete.cases(df_sel[, key_vars]), ]
n3 <- nrow(df_clean)
cat("3. 剔除核心变量缺失后样本量：", n3, "\n")

# 4. 协变量插补、异常值缩尾后
# 众数/中位数插补 + 能量摄入缩尾，仅修改数值，不删除样本
n4 <- nrow(df_clean)
cat("4. 缺失插补+异常值缩尾后样本量：", n4, "\n")

# 5. 生成最终分析数据集 df_final
library(dplyr)
df_final <- df_clean %>%
  mutate(
    high_tc  = ifelse(LBXTC >= 240, 1, 0),
    high_tg  = ifelse(LBXTR >= 200, 1, 0),
    low_hdl  = ifelse((RIAGENDR == 1 & LBXHD < 40) | (RIAGENDR == 2 & LBXHD < 50), 1, 0),
    high_ldl = ifelse(LBDLDL >= 160, 1, 0)
  )
n5 <- nrow(df_final)
cat("5. 最终分析数据集样本量：", n5, "\n")


table(df_final$high_tc)
table(df_final$high_tg)
table(df_final$low_hdl)
table(df_final$high_ldl)

# 查看数据集所有列
colnames(df_final)
