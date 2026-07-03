# 加载包
library(haven)
library(dplyr)
library(tableone)
setwd("D:/NHANES-2017-2018")

#1。读取并合并原始数据

# 基础数据
demo   <- read_xpt("DEMO_J.XPT")
bmx    <- read_xpt("BMX_J.XPT")
dr1tot <- read_xpt("DR1TOT_J.XPT")

# 读取血脂
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

sugar_data$exposure_3 <- factor(sugar_data$exposure_3,
                                levels=c("低添加糖","中添加糖","高添加糖"))
#数据合并
df_all <- inner_join(demo,bmx,by="SEQN") %>% inner_join(sugar_data,by="SEQN")
df <- inner_join(df_all,lipid,by="SEQN")

#2.筛选变量和样本
#关键变量
keep_var = c("SEQN","exposure_3","RIDAGEYR","RIAGENDR","RIDRETH1","DMDEDUC2","INDFMPIR",
             
             "BMXBMI","DR1TKCAL","DR1TSUGR","LBXTC","LBXTR","LBXHD","LBDLDL")

# 筛选成年人
df_adult <- subset(df, RIDAGEYR >= 18)

n1 <- nrow(df_adult)
cat("1. 成年原始样本量：", n1, "\n")

# 筛选变量
df_sel <- df_adult[, keep_var]
n2 <- nrow(df_sel)
cat("2. 筛选变量后样本量：", n2, "\n")

# 查看各变量缺失情况
colSums(is.na(df_sel))
# 直接计算缺失率（百分比）
colMeans(is.na(df_sel)) * 100
# 保留两位小数
round(colMeans(is.na(df_sel)) * 100, 2)

#缺失值处理
#3.剔除核心变量缺失（暴露+血脂）
key_vars <- c("exposure_3", "LBXTC", "LBXTR", "LBXHD", "LBDLDL")
df_clean <- df_sel[complete.cases(df_sel[, key_vars]), ]
n3 <- nrow(df_clean)
cat("\n3. 剔除核心变量缺失后样本量：", n3, "\n")


# 核对一轮缺失
colSums(is.na(df_clean))
nrow(df_sel)    # 原样本量
nrow(df_clean)  # 删核心缺失后样本量

#4.协变量缺失处理
# 查看变量存储类型
class(df_sel$INDFMPIR)

# 查看取值、分布、缺失
summary(df_sel$INDFMPIR)

# 统计有多少种不同取值（判断是“编码分类”还是“真实连续”）
length(unique(na.omit(df_sel$INDFMPIR)))


# 连续型协变量BMXBMI INDFMPIR：中位数插补
# 先算出中位数
bmi_med <- median(df_clean$BMXBMI, na.rm = TRUE)
# 再填充缺失
df_clean$BMXBMI[is.na(df_clean$BMXBMI)] <- bmi_med

# INDFMPIR（连续偏态变量）：中位数插补
indf_med <- median(df_clean$INDFMPIR, na.rm = TRUE)
df_clean$INDFMPIR[is.na(df_clean$INDFMPIR)] <- indf_med

# 分类变量，如 DMDEDUC2：众数插补
#定义众数函数
get_mode <- function(x){
  tbl <- table(x)
  mode_str <- names(tbl)[which.max(tbl)]
  if(is.numeric(x)){
    return(as.numeric(mode_str))
  }else{
    return(mode_str)
  }
}
df_clean$DMDEDUC2[is.na(df_clean$DMDEDUC2)] <- get_mode(df_clean$DMDEDUC2)

# 核查全部变量缺失情况
colSums(is.na(df_clean))


#5.查看异常值
summary(df_clean[, c("BMXBMI", "DR1TSUGR","DR1TKCAL", "LBXTC", "LBXTR", "LBXHD", "LBDLDL")])

#缩尾法
# 计算总能量摄入 1% 分位数（下限）
q01 <- quantile(df_clean$DR1TKCAL, 0.01, na.rm = TRUE)

# 计算总能量摄入 99% 分位数（上限）
q99 <- quantile(df_clean$DR1TKCAL, 0.99, na.rm = TRUE)

# 缩尾处理
# 新建数据集
df_clean$DR1TKCAL_winsor <- df_clean$DR1TKCAL
# 小于1%分位数，替换为下限
df_clean$DR1TKCAL_winsor[df_clean$DR1TKCAL < q01] <- q01
# 大于99%分位数，替换为上限
df_clean$DR1TKCAL_winsor[df_clean$DR1TKCAL > q99] <- q99

# 对比缩尾前后数据
summary(df_clean[, c("DR1TKCAL", "DR1TKCAL_winsor")])

n4 <- nrow(df_clean)
cat("\n4. 插补+缩尾后样本量：", n4, "\n")

# 6. 生成最终分析数据集 df_final
df_final <- df_clean %>%
  mutate(
    high_tc  = ifelse(LBXTC >= 240, 1, 0),
    high_tg  = ifelse(LBXTR >= 200, 1, 0),
    low_hdl  = ifelse((RIAGENDR == 1 & LBXHD < 40) | (RIAGENDR == 2 & LBXHD < 50), 1, 0),
    high_ldl = ifelse(LBDLDL >= 160, 1, 0)
  )

n5 <- nrow(df_final)

cat("5. 最终分析样本量：", n5, "\n")

# 查看数据集所有列
colnames(df_final)

# 血脂异常分布
cat("\n血脂异常人数分布：\n")
print(table(df_final$high_tc))
print(table(df_final$high_tg))
print(table(df_final$low_hdl))
print(table(df_final$high_ldl))

