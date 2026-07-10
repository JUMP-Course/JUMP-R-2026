data <- read.csv("C:/Users/ASUS/Documents/GitHub/JUMP-R-2026/students/xuminqi/R/data.csv")
# 1. 选取需要求和的自伤条目列
selfharm_cols <- c("n96","n97","n98","n99","n100","n103","n104","n105","n106","n107","n108","field1")

# 2. 按行求和，na.rm=TRUE忽略NA缺失值
data$sum_nssi <- rowSums(data[, selfharm_cols], na.rm = TRUE)

# 3. 新建二分类变量：总和>12=1(有NSSI)，≤12=0(无NSSI)
data$nssi_group <- ifelse(data$sum_nssi > 12, 1, 0)

# 查看分组人数
table(data$nssi_group)

# 查看总分分布
summary(data$sum_nssi)

# 基础直方图，na.rm写在数据筛选，不写在绘图参数
hist(data$sum_nssi,
     main = "NSSI条目总分分布",
     xlab = "总分",
     col = "lightgray")

# 添加临界分割线12
abline(v = 12, lty = 2, lwd = 2, col = "darkblue")

# 第二步：按分类逐条赋值
data$n23_var[data$n23 == 1] <- 0
data$n23_var[data$n23 == 2] <- 0.5
data$n23_var[data$n23 == 3] <- 1.5
data$n23_var[data$n23 == 4] <- 2.5
data$n23_var[data$n23 == 5] <- 3.5
data$n23_var[data$n23 == 6] <- 4.5

# 第三步：核对转换结果（原始列+新列前20行）
head(data[, c("n23", "n23_var")], 20)

# 第四步：交叉表校验映射是否正确
table(data$n23, data$n23_var)

# 第五步：查看转换后时长分布
summary(data$n23_var)

# 1. 强制把n24转为数值型，避免字符匹配失败
data$n24 <- as.numeric(data$n24)

# 2. 初始化新列，填充NA
data$n24_var <- NA

# 3. 分类逐条映射赋值
data$n24_var[data$n24 == 1] <- 0
data$n24_var[data$n24 == 2] <- 0.5
data$n24_var[data$n24 == 3] <- 1.5
data$n24_var[data$n24 == 4] <- 2.5
data$n24_var[data$n24 == 5] <- 3.5
data$n24_var[data$n24 == 6] <- 4.5

# 4. 抽查前20行转换结果
head(data[, c("n24", "n24_var")], 20)

# 5. 交叉表校验映射关系，同时展示缺失值
table(data$n24, data$n24_var, useNA = "ifany")

# 新建屏幕时长 screentime = (n23_var*5 + n24_var*2)/7
data$screentime <- (data$n23_var * 5 + data$n24_var * 2) / 7

# 查看前20行核对结果
head(data[, c("n23_var", "n24_var", "screentime")], 20)

# 查看整体分布、缺失值、四分位数
summary(data$screentime)

# 剔除缺失值用于绘图
clean_st <- na.omit(data$screentime)

# 屏幕时长直方图
hist(clean_st, main="屏幕时长 screentime 分布", xlab="时长", col="lightblue")

# 屏幕时长箱线图（圆点为异常值）
boxplot(clean_st, main="屏幕时长箱线图（圆点为异常值）")

# 每一列缺失数量统计
colSums(is.na(data))# 1. 分别标记三类样本是否要剔除（独立判断，互不干扰）
# i4剔除条件：i4缺失 或 i4<3 或 i4>9.9
drop_i4 <- is.na(data_raw$i4) | data_raw$i4 < 3 | data_raw$i4 > 9.9
# screentime剔除条件：screentime缺失 或 <0 或 >4.5
drop_st <- is.na(data_raw$screentime) | data_raw$screentime < 0 | data_raw$screentime > 4.5
# n18剔除条件：n18缺失 或 <0 或 >999 或 >360
drop_n18 <- is.na(data_raw$n18) | data_raw$n18 < 0 | data_raw$n18 > 999 | data_raw$n18 > 360

# 2. 最终保留：三个变量全都不满足剔除条件的行
keep_row <- !drop_i4 & !drop_st & !drop_n18
data_clean <- data_raw[keep_row, ]

# 3. 清洗日志（单独统计每个变量各自要删掉多少，不受其他变量影响）
cat("===== 各变量单独剔除统计（独立计算，互不干扰）=====\n")
cat("原始总样本：", nrow(data_raw), "\n")
cat("仅因i4需剔除的样本数：", sum(drop_i4), "\n")
cat("仅因screentime需剔除的样本数：", sum(drop_st), "\n")
cat("仅因n18需剔除的样本数：", sum(drop_n18), "\n")
cat("三个变量全部合格、最终保留样本：", nrow(data_clean), "\n")

# 4. 校验清洗后数据
sum(is.na(data_clean$i4))
sum(is.na(data_clean$screentime))
sum(is.na(data_clean$n18))
range(data_clean$i4)
range(data_clean$screentime)
range(data_clean$n18)

# 5. 清洗前后对比绘图数据准备（绘图需提前加载tidyr、ggplot2）
raw_sub <- data_raw[,c("i4","screentime","n18")]
raw_sub$组别 <- "清洗前(原始)"
clean_sub <- data_clean[,c("i4","screentime","n18")]
clean_sub$组别 <- "清洗后"
# 合并+转长格式
compare_df <- rbind(raw_sub, clean_sub) %>% pivot_longer(-组别)
# 加载绘图包（第一次运行先执行安装：install.packages(c("tidyr","ggplot2"))）
library(tidyr)
library(ggplot2)

# 构造对比数据集
raw_sub <- data_raw[, c("i4","screentime","n18")]
raw_sub$组别 <- "清洗前(原始)"

clean_sub <- data_clean[, c("i4","screentime","n18")]
clean_sub$组别 <- "清洗后"

# 合并数据并转为长格式
compare_df <- rbind(raw_sub, clean_sub) %>% pivot_longer(cols = -组别, names_to = "变量", values_to = "数值")

# 绘制分面对比直方图
ggplot(compare_df, aes(x = 数值, fill = 组别)) +
  geom_histogram(alpha = 0.6, bins = 30) +
  facet_grid(变量 ~ 组别, scales = "free_x") +
  labs(title = "i4 / screentime / n18 清洗前后分布对比", x = "变量取值", y = "频数") +
  scale_fill_manual(values = c("salmon","steelblue")) +
library(tableone)
# 导入数据！！
dat <- read.csv("C:/Users/ASUS/Desktop/data.csv")
# 1. 构建分析变量向量

vars <- c("sum_nssi", "i4", "screentime", "n18") 
# i4、n18、screentime = 连续变量
# sum_nssi = 分类变量
# 2. 创建Table 1
table1 <- CreateTableOne(
  vars = vars,
   data = dat
)
# 3. 查看原始结果
print(table1)

# 4. 【汇报关键】优化输出：连续变量+分类变量指定格式
# continuous：连续变量用均值±标准差；如果偏态改用中位数(IQR)
# factorVars：指定哪些是分类变量
table1_final <- print(
  table1,
  factorVars = c("sum_nssi"), # 声明分类变量
  contDigits = 1,    # 连续变量小数位数
  catDigits = 1,     # 分类变量百分比小数
  pDigits = 3,       # P值保留3位
  showPvalues = TRUE,# 是否展示组间比较P值
  test = TRUE        # 开启组间检验
)

# 导出到csv，直接粘贴Word做三线表
write.csv(table1_final, "基线特征表.csv", row.names = TRUE)