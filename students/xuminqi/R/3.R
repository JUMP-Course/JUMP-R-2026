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
drop_i4 <- is.na(data$i4) | data$i4 < 3 | data$i4 > 9.9
# screentime剔除条件：screentime缺失 或 <0 或 >4.5
drop_st <- is.na(data$screentime) | data$screentime < 0 | data$screentime > 4.5
# n18剔除条件：n18缺失 或 <0 或 >999 或 >360
drop_n18 <- is.na(data$n18) | data$n18 < 0 | data$n18 > 999 | data$n18 > 360

# 2. 最终保留：三个变量全都不满足剔除条件的行
keep_row <- !drop_i4 & !drop_st & !drop_n18
data_clean <- data[keep_row, ]

# 3. 清洗日志（单独统计每个变量各自要删掉多少，不受其他变量影响）
cat("===== 各变量单独剔除统计（独立计算，互不干扰）=====\n")
cat("原始总样本：", nrow(data), "\n")
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
raw_sub <- data[,c("i4","screentime","n18")]
raw_sub$组别 <- "清洗前(原始)"
clean_sub <- data_clean[,c("i4","screentime","n18")]
clean_sub$组别 <- "清洗后"

# 加载绘图包（第一次运行先执行安装：install.packages(c("tidyr","ggplot2"))）
library(tidyr)
library(tableone)
library(ggplot2)

# 构造对比数据集
raw_sub <- data[, c("i4","screentime","n18")]
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



# 1. 构建分析变量向量

vars <- c("sum_nssi", "i4", "screentime", "n18","s4","n1","n14","n15","n16") 
# i4、n18、screentime = 连续变量
# sum_nssi、s4、n1、n14、n15、n16 = 分类变量

 2.# 分组变量
group_var <- "s4"
# 2. 定义所有分类变量
factor_list <- c("s4","n1","n14","n15","n16")
#创建Table 1
table1 <- CreateTableOne(
  vars = vars,
  strata = group_var,
   data = dat
)
# 3. 查看原始结果
print(table1)

# 4. 【汇报关键】优化输出：连续变量+分类变量指定格式
# continuous：连续变量用均值±标准差；如果偏态改用中位数(IQR)
# factorVars：指定哪些是分类变量
table1_final <- print(
  table1,
  factorVars = c("sum_nssi","s4","n1","n14","n15","n16"), # 声明分类变量
  contDigits = 1,    # 连续变量小数位数
  catDigits = 1,     # 分类变量百分比小数
  pDigits = 3,       # P值保留3位
  showPvalues = TRUE,# 是否展示组间比较P值
  test = TRUE        # 开启组间检验
)
# 导出到csv，直接粘贴Word做三线表
write.csv(table1_final, "基线特征表.csv", row.names = TRUE)
# 安装仅第一次运行
install.packages(c("ggplot2","dplyr","corrplot","tableone","patchwork"))
library(ggplot2)
library(dplyr)
library(corrplot)
library(patchwork) # 拼图，一次性展示多张核心图
# 导入你的数据
dat <- read.csv("C:/Users/ASUS/Desktop/data.csv", fileEncoding = "UTF-8")
# 构建NSSI总分
dat <- dat %>% mutate(sum_nssi =n96+n97+n98+n99+n100+n103+n104+n105+n106+n107+n108+field1)
p_dist1 <- ggplot(dat,aes(x=sum_nssi))+
  geom_histogram(aes(y=after_density),bins=30,fill="#4472C4",alpha=0.7)+
  geom_density(linewidth=1,color="red")+
  labs(x="NSSI自伤总分",y="密度",title="结局变量sum_nssi分布")+
  theme_bw()
p_dist2 <- ggplot(dat,aes(x=i4))+
  geom_histogram(aes(y=after_density),bins=30,fill="#548235",alpha=0.7)+
  geom_density(linewidth=1,color="red")+
  labs(x="每日睡眠时长(h)",y="密度",title="睡眠时长分布")+
  theme_bw()

# patchwork拼图，一页展示
p_all_dist <- p_dist1 + p_dist2
print(p_all_dist)
ggsave("1_变量分布图.png",p_all_dist,width=12,height=5,dpi=300)
# 分组变量s4（性别/经济/年级均可替换）
p_box <- ggplot(dat,aes(x=s4,y=sum_nssi,fill=s4))+
  geom_boxplot(alpha=0.7,show.legend = F)+
  labs(x="分组（s4）",y="NSSI自伤总分",title="不同组别自伤得分组间对比")+
  theme_bw()
print(p_box)
ggsave("2_组间箱线图.png",p_box,width=8,height=5,dpi=300)
# 分组变量s4（性别/经济/年级均可替换）
p_box <- ggplot(dat,aes(x=n1,y=sum_nssi,fill=s4))+
  geom_boxplot(alpha=0.7,show.legend = F)+
  labs(x="分组（s4）",y="NSSI自伤总分",title="不同组别自伤得分组间对比")+
  theme_bw()
print(p_box)
ggsave("2_组间箱线图.png",p_box,width=8,height=5,dpi=300)
# 分组变量s4（性别/经济/年级均可替换）
p_box <- ggplot(dat,aes(x=n14,y=sum_nssi,fill=s4))+
  geom_boxplot(alpha=0.7,show.legend = F)+
  labs(x="分组（s4）",y="NSSI自伤总分",title="不同组别自伤得分组间对比")+
  theme_bw()
print(p_box)
ggsave("2_组间箱线图.png",p_box,width=8,height=5,dpi=300)
# 分组变量s4（性别/经济/年级均可替换）
p_box <- ggplot(dat,aes(x=n15,y=sum_nssi,fill=s4))+
  geom_boxplot(alpha=0.7,show.legend = F)+
  labs(x="分组（s4）",y="NSSI自伤总分",title="不同组别自伤得分组间对比")+
  theme_bw()
print(p_box)
ggsave("2_组间箱线图.png",p_box,width=8,height=5,dpi=300)
# 分组变量s4（性别/经济/年级均可替换）
p_box <- ggplot(dat,aes(x=n16,y=sum_nssi,fill=s4))+
  geom_boxplot(alpha=0.7,show.legend = F)+
  labs(x="分组（s4）",y="NSSI自伤总分",title="不同组别自伤得分组间对比")+
  theme_bw()
print(p_box)
ggsave("2_组间箱线图.png",p_box,width=8,height=5,dpi=300)
p_trend <- ggplot(dat,aes(x=screentime,y=sum_nssi))+
  geom_point(alpha=0.5,color="gray40")+
  geom_smooth(method="lm",color="red",linewidth=1)+ # 线性拟合
  labs(x="每日屏幕使用时长(h)",y="NSSI总分",title="屏幕时长与自伤得分变化趋势")+
  theme_bw()
print(p_trend)
ggsave("3_趋势散点图.png",p_trend,width=8,height=5,dpi=300)
# 仅首次运行安装
install.packages(c("ggplot2","dplyr","tableone","nnet","emmeans"))
library(ggplot2)
library(dplyr)
library(tableone)

# 导入数据
dat <- read.csv("C:/Users/ASUS/Desktop/data.csv", fileEncoding = "UTF-8")
# 生成自伤总分
dat <- dat %>% mutate(sum_nssi = n96+n97+n98+n99+n100+n103+n104+n105+n106+n107+n108+field1)

# sum_nssi > 12 = 1（自伤组）；sum_nssi ≤12 = 0（非自伤组）
dat <- dat %>% mutate(nssi_case = ifelse(sum_nssi > 12, 1, 0))
# 查看两组人数分布，检验分组是否成功
table(dat$nssi_case)
p_t <- ggplot(dat,aes(x=factor(nssi_case),y=sum_nssi,fill=factor(nssi_case)))+
  geom_boxplot(alpha=0.7)+
  labs(x="分组（0=非自伤，1=自伤）",y="NSSI自伤总分",title="自伤/非自伤人群总分组间对比")+
  scale_x_discrete(labels = c("非自伤(≤12分)","自伤(＞12分)"))+
  theme_bw()+theme(legend.position = "none")
print(p_t)
ggsave("自伤分组t检验箱线图.png",p_t,width=7,height=5,dpi=300)
# 独立样本t检验：比较两组总分均值差异
t_res <- t.test(sum_nssi ~ nssi_case, data = dat)
print(t_res)

# 适用条件核查
# 1.正态性
shapiro.test(dat$sum_nssi[dat$nssi_case==0])
shapiro.test(dat$sum_nssi[dat$nssi_case==1])
# 2.方差齐性
var.test(sum_nssi ~ nssi_case, data = dat)
# 多因素logistic，校正混杂变量（示例：i4睡眠、screentime屏幕时长、s4性别）
multi_logit <- glm(nssi_case ~ i4 + screentime + s4,
                   data = dat, family = binomial(link = "logit"))
summary(multi_logit)
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

# 输出OR值+95%置信区间（论文标准结果）
exp(cbind(OR = coef(multi_logit), confint(multi_logit)))
p_logit <- ggplot(dat,aes(x=screentime,y=nssi_case))+
  geom_jitter(width=0,height=0.05,alpha=0.4)+
  geom_smooth(method="glm",method.args=list(family="binomial"),color="red")+
  labs(x="每日屏幕使用时长",y="发生自伤（总分＞12）概率",title="屏幕时长与自伤发生风险趋势")+
  theme_bw()
print(p_logit)
ggsave("logistic自伤风险趋势图.png",p_logit,width=7,height=5,dpi=300)
# 加载包
library(ggplot2)
library(tableone)
library(broom)    # 提取模型系数、CI、P
library(broom.mixed)
library(gt)       # 美观表格输出
library(AICcmodavg)

# 1. 拟合logistic回归（自变量：screentime、i4、n13等，结局nssi_case）
fit <- glm(nssi_case ~ screentime + i4 + n13 + s4, 
           data = dat, 
           family = binomial(link = "logit"))

# 2. 提取完整结果：β系数、95%CI、P值
res_raw <- tidy(fit, conf.int = TRUE)
res_raw

# 3. 换算OR（exp(β)）+ OR的95%CI，整理成论文标准格式
res_or <- res_raw %>%
  mutate(
    OR = exp(estimate),          # β指数化=OR值
    OR_low = exp(conf.low),      # OR下限95%CI
    OR_high = exp(conf.high),    # OR上限95%CI
    beta = estimate,             # 原始回归系数β
    beta_low = conf.low,
    beta_high = conf.high,
    p_value = p.value
  ) %>%
  select(term, beta, beta_low, beta_high, OR, OR_low, OR_high, p_value)

# 4. 格式化CI文本（统一展示：OR(95%CI)）
res_table <- res_or %>%
  mutate(
    OR_CI = paste0(sprintf("%.2f",OR), "(", sprintf("%.2f",OR_low), "-", sprintf("%.2f",OR_high), ")"),
    beta_CI = paste0(sprintf("%.2f",beta), "(", sprintf("%.2f",beta_low), "-", sprintf("%.2f",beta_high), ")"),
    p_value = ifelse(p_value<0.001, "<0.001", sprintf("%.3f",p_value))
  ) %>%
  select(变量=term, 回归系数β=beta_CI, OR值(95%CI)=OR_CI, P值=p_value)

# 打印干净结果表（汇报直接展示）
print(res_table)

# 5. 模型评价指标（AIC、BIC、对数似然）
model_eval <- glance(fit)
print(model_eval[,c("AIC","BIC","logLik","nobs")])