#单因素分析
#年龄组与门诊率的卡方检验
# 1. 交叉表
tab <- table(df_analy$age_group, df_analy$doctor)
tab
# 2. 行就诊率
prop.table(tab, 1)*100
# 3. Pearson卡方检验
chisq.test(tab, correct = FALSE)
# 4. 单因素Logistic回归（粗OR）
fit_single <- glm(doctor ~ age_group, data = df_analy, family = "binomial")
exp(coef(fit_single))          # OR值
exp(confint(fit_single))       # 95%置信区间
summary(fit_single)            # P值
#年龄组与住院率
tab <- table(df_analy$age_group, df_analy$hospital)
tab
prop.table(tab, 1)*100
chisq.test(tab, correct = FALSE)
fit_single <- glm(hospital ~ age_group, data = df_analy, family = "binomial")
exp(coef(fit_single))          # OR值
exp(confint(fit_single))       # 95%置信区间
summary(fit_single)            # P值

#受教育水平与门诊率的卡方检验和线性趋势检验
install.packages("DescTools")
library(DescTools)
# 1. 数据预处理：定义学历有序分组，设置参照组
edu_order <- c("小学及以下", "小学", "中学", "高中及以上")
df_analy$edu_fac <- factor(df_analy$edu_label, levels = edu_order)
df_analy$edu_fac <- relevel(df_analy$edu_fac, ref = "小学及以下")
df_analy$edu_score <- as.integer(df_analy$edu_fac)
# 2. 构建学历×门诊就诊列联表
tab_edu <- table(df_analy$edu_fac, df_analy$doctor)
cat("===== 学历-门诊就诊列联表 =====\n")
print(tab_edu)
cat("===== 各组门诊就诊率 =====\n")
print(prop.table(tab_edu, margin = 1))
# 3. Pearson卡方检验（整体组间差异）
cat("\n===== Pearson卡方检验（4组整体差异） =====\n")
chisq_edu <- chisq.test(tab_edu)
print(chisq_edu)
# 4. Mantel线性趋势卡方检验（有序线性趋势）
cat("\n===== Mantel线性趋势检验 =====\n")
trend_mh_edu <- MHChisqTest(tab_edu, srow = 1:4, scol = c(0, 1))
print(trend_mh_edu)
# 5. 单因素Logistic回归1：分组粗OR
cat("\n===== 分组粗OR、95%CI、P值 =====\n")
fit_group_edu <- glm(doctor ~ edu_fac, data = df_analy, family = binomial)
summary(fit_group_edu)          # P值
exp(coef(fit_group_edu))        # 粗OR
exp(confint(fit_group_edu))     # 95%置信区间
# 6. 单因素Logistic回归：趋势OR
cat("\n===== 线性趋势OR（每升高1个学历等级） =====\n")
fit_trend_edu <- glm(doctor ~ edu_score, data = df_analy, family = binomial)
summary(fit_trend_edu)
exp(coef(fit_trend_edu))
exp(confint(fit_trend_edu))
#受教育水平与住院率的卡方检验和线性趋势检验
#1.数据预处理
edu_order <- c("小学及以下", "小学", "中学", "高中及以上")
df_analy$edu_fac <- factor(df_analy$edu_label, levels = edu_order)
df_analy$edu_fac <- relevel(df_analy$edu_fac, ref = "小学及以下")
df_analy$edu_score <- as.integer(df_analy$edu_fac)
# 2. 构建学历×住院列联表
tab_edu_hosp <- table(df_analy$edu_fac, df_analy$hospital)
cat("===== 学历-住院列联表 =====\n")
print(tab_edu_hosp)
cat("===== 各组住院率 =====\n")
print(prop.table(tab_edu_hosp, margin = 1))
# 3. Pearson卡方检验（整体组间差异）
cat("\n===== Pearson卡方检验（住院） =====\n")
chisq_edu_hosp <- chisq.test(tab_edu_hosp)
print(chisq_edu_hosp)
# 4. Mantel线性趋势卡方检验（有序线性趋势）
cat("\n===== 住院Mantel线性趋势检验 =====\n")
trend_mh_edu_hosp <- MHChisqTest(tab_edu_hosp, srow = 1:4, scol = c(0, 1))
print(trend_mh_edu_hosp)
# 5. 单因素Logistic回归1：分组粗OR
cat("\n===== 住院分组粗OR、95%CI、P值 =====\n")
fit_group_edu_hosp <- glm(hospital ~ edu_fac, data = df_analy, family = binomial)
summary(fit_group_edu_hosp)          # P值
exp(coef(fit_group_edu_hosp))        # 粗OR
exp(confint(fit_group_edu_hosp))     # 95%置信区间
# 6. 单因素Logistic回归：趋势OR
cat("\n===== 住院线性趋势OR（每升高1个学历等级） =====\n")
fit_trend_edu_hosp <- glm(hospital ~ edu_score, data = df_analy, family = binomial)
summary(fit_trend_edu_hosp)
exp(coef(fit_trend_edu_hosp))
exp(confint(fit_trend_edu_hosp))

#家庭年人均消费与门诊率的卡方检验和线性趋势检验
# 1. 数据预处理：定义消费五等分有序层级
consume_order <- c("前20%", "20%-40%", "40%-60%", "60%-80%", "后20%")
df_analy$consume_fac <- factor(df_analy$consume_group, levels = consume_order)
df_analy$consume_fac <- relevel(df_analy$consume_fac, ref = "前20%")
df_analy$consume_score <- as.integer(df_analy$consume_fac)
# 2. 构建消费分组×门诊就诊列联表
tab_consume <- table(df_analy$consume_fac, df_analy$doctor)
cat("===== 消费五等分-门诊就诊列联表 =====\n")
print(tab_consume)
cat("===== 各组门诊就诊率 =====\n")
print(prop.table(tab_consume, margin = 1))
# 3. Pearson卡方检验：5组门诊率整体差异
cat("\n===== Pearson卡方检验（消费五等分&门诊） =====\n")
chisq_consume <- chisq.test(tab_consume)
print(chisq_consume)
# 4. Mantel线性趋势卡方检验：门诊率随消费升高的线性趋势
cat("\n===== Mantel线性趋势检验（消费五等分） =====\n")
trend_mh_consume <- MHChisqTest(tab_consume, srow = 1:5, scol = c(0, 1))
print(trend_mh_consume)
# 5. 单因素Logistic：分组粗OR
cat("\n===== 分组粗OR、95%CI、P值（消费分组） =====\n")
fit_group_consume <- glm(doctor ~ consume_fac, data = df_analy, family = binomial)
summary(fit_group_consume)          # 输出P值
exp(coef(fit_group_consume))        # 粗OR
exp(confint(fit_group_consume))     # OR 95%置信区间
# 6. 单因素Logistic：线性趋势OR
cat("\n===== 线性趋势OR =====\n")
fit_trend_consume <- glm(doctor ~ consume_score, data = df_analy, family = binomial)
summary(fit_trend_consume)
exp(coef(fit_trend_consume))
exp(confint(fit_trend_consume))
#家庭年人均消费与住院率的卡方检验与线性趋势检验
# 1. 数据预处理：定义消费五等分有序层级
consume_order <- c("前20%", "20%-40%", "40%-60%", "60%-80%", "后20%")
df_analy$consume_fac <- factor(df_analy$consume_group, levels = consume_order)
df_analy$consume_fac <- relevel(df_analy$consume_fac, ref = "前20%")
df_analy$consume_score <- as.integer(df_analy$consume_fac)
# 2. 构建消费分组×住院就诊列联表
tab_consume_hosp <- table(df_analy$consume_fac, df_analy$hospital)
cat("===== 消费五等分-住院列联表 =====\n")
print(tab_consume)
cat("===== 各组住院率 =====\n")
print(prop.table(tab_consume_hosp, margin = 1))
# 3. Pearson卡方检验：5组住院率整体差异
cat("\n===== Pearson卡方检验（消费五等分&住院） =====\n")
chisq_consume_hosp <- chisq.test(tab_consume_hosp)
print(chisq_consume_hosp)
# 4. Mantel线性趋势卡方检验：住院率随消费升高的线性趋势
cat("\n===== Mantel线性趋势检验（消费五等分） =====\n")
trend_mh_consume_hosp <- MHChisqTest(tab_consume_hosp, srow = 1:5, scol = c(0, 1))
print(trend_mh_consume_hosp)
# 5. 单因素Logistic：分组粗OR
cat("\n===== 分组粗OR、95%CI、P值（消费分组） =====\n")
fit_group_consume_hosp <- glm(hospital ~ consume_fac, data = df_analy, family = binomial)
summary(fit_group_consume_hosp)          # 输出P值
exp(coef(fit_group_consume_hosp))        # 粗OR
exp(confint(fit_group_consume_hosp))     # OR 95%置信区间
# 6. 单因素Logistic：线性趋势OR
cat("\n===== 线性趋势OR =====\n")
fit_trend_consume_hosp <- glm(hospital ~ consume_score, data = df_analy, family = binomial)
summary(fit_trend_consume_hosp)
exp(coef(fit_trend_consume_hosp))
exp(confint(fit_trend_consume_hosp))

#自评健康状况与门诊率的卡方检验与线性趋势检验
# 1. 自评健康数据预处理
srh_order <- c("很差", "较差", "一般", "较好", "很好")
df_analy$srh_fac <- factor(df_analy$srh_label, levels = srh_order)
df_analy$srh_fac <- relevel(df_analy$srh_fac, ref = "很差")
df_analy$srh_score <- as.integer(df_analy$srh_fac)
# 2. 构建自评健康×门诊就诊列联表
tab_srh <- table(df_analy$srh_fac, df_analy$doctor)
cat("===== 自评健康-门诊就诊列联表 =====\n")
print(tab_srh)
cat("===== 各组门诊就诊率 =====\n")
print(prop.table(tab_srh, margin = 1))
# 3. Pearson卡方检验
cat("\n===== Pearson卡方检验（自评健康&门诊） =====\n")
chisq_srh <- chisq.test(tab_srh)
print(chisq_srh)
# 4. Mantel线性趋势卡方检验
cat("\n===== Mantel线性趋势检验（自评健康） =====\n")
trend_mh_srh <- MHChisqTest(tab_srh, srow = 1:5, scol = c(0, 1))
print(trend_mh_srh)
# 5. 分组单因素Logistic：各组粗OR、95%CI、P值
cat("\n===== 分组粗OR、95%CI、P值（自评健康） =====\n")
fit_group_srh <- glm(doctor ~ srh_fac, data = df_analy, family = binomial)
summary(fit_group_srh)          # 输出P值
exp(coef(fit_group_srh))        # 粗OR
exp(confint(fit_group_srh))     # OR 95%置信区间
# 6. 线性趋势Logistic检验
cat("\n===== 线性趋势OR（自评健康每提升1个等级） =====\n")
fit_trend_srh <- glm(doctor ~ srh_score, data = df_analy, family = binomial)
summary(fit_trend_srh)
exp(coef(fit_trend_srh))
exp(confint(fit_trend_srh))
#自评健康状况与住院率的卡方检验与线性趋势检验
# 1. 自评健康数据预处理
srh_order <- c("很差", "较差", "一般", "较好", "很好")
df_analy$srh_fac <- factor(df_analy$srh_label, levels = srh_order)
df_analy$srh_fac <- relevel(df_analy$srh_fac, ref = "很差")
df_analy$srh_score <- as.integer(df_analy$srh_fac)
# 2. 构建自评健康×住院列联表
tab_srh_hosp <- table(df_analy$srh_fac, df_analy$hospital)
cat("===== 自评健康-住院列联表 =====\n")
print(tab_srh_hosp)
cat("===== 各组住院率 =====\n")
print(prop.table(tab_srh_hosp, margin = 1))
# 3. Pearson卡方检验
cat("\n===== Pearson卡方检验（自评健康&门诊） =====\n")
chisq_srh_hosp <- chisq.test(tab_srh_hosp)
print(chisq_srh_hosp)
# 4. Mantel线性趋势卡方检验
cat("\n===== Mantel线性趋势检验（自评健康） =====\n")
trend_mh_srh_hosp <- MHChisqTest(tab_srh_hosp, srow = 1:5, scol = c(0, 1))
print(trend_mh_srh_hosp)
# 5. 分组单因素Logistic：各组粗OR、95%CI、P值
cat("\n===== 分组粗OR、95%CI、P值（自评健康） =====\n")
fit_group_srh_hosp <- glm(hospital ~ srh_fac, data = df_analy, family = binomial)
summary(fit_group_srh_hosp)          # 输出P值
exp(coef(fit_group_srh_hosp))        # 粗OR
exp(confint(fit_group_srh_hosp))     # OR 95%置信区间
# 6. 线性趋势Logistic检验
cat("\n===== 线性趋势OR（自评健康每提升1个等级） =====\n")
fit_trend_srh_hosp <- glm(hospital ~ srh_score, data = df_analy, family = binomial)
summary(fit_trend_srh_hosp)
exp(coef(fit_trend_srh_hosp))
exp(confint(fit_trend_srh_hosp))

#慢性病数量与门诊率的卡方检验与线性趋势检验
# 1. 慢性病数据预处理
chronic_group_order <- c("0种","1种" ,"2种", "3种", "4种", "5种及以上")
df_analy$chronic_group_fac <- factor(df_analy$chronic_group, levels = chronic_group_order)
df_analy$chronic_group_fac <- relevel(df_analy$chronic_group_fac, ref = "0种")
df_analy$chronic_group_score <- as.integer(df_analy$chronic_group_fac)
# 2. 构建慢性病数量×门诊就诊列联表
tab_chronic_group <- table(df_analy$chronic_group_fac, df_analy$doctor)
cat("===== 慢性病数量-门诊就诊列联表 =====\n")
print(tab_chronic_group)
cat("===== 各组门诊就诊率 =====\n")
print(prop.table(tab_chronic_group, margin = 1))
# 3. Pearson卡方检验
cat("\n===== Pearson卡方检验慢性病数量&门诊） =====\n")
chisq_chronic_group <- chisq.test(tab_chronic_group)
print(chisq_chronic_group)
# 4. Mantel线性趋势卡方检验
cat("\n===== Mantel线性趋势检验（慢性病数量） =====\n")
trend_mh_chronic_group <- MHChisqTest(tab_chronic_group, srow = 1:6, scol = c(0, 1))
print(trend_mh_chronic_group)
# 5. 分组单因素Logistic：各组粗OR、95%CI、P值
cat("\n===== 分组粗OR、95%CI、P值（慢性病数量） =====\n")
fit_group_chronic_group <- glm(doctor ~ chronic_group_fac, data = df_analy, family = binomial)
summary(fit_group_chronic_group)          # 输出P值
exp(coef(fit_group_chronic_group))        # 粗OR
exp(confint(fit_group_chronic_group))     # OR 95%置信区间
# 6. 线性趋势Logistic检验
cat("\n===== 线性趋势OR（慢性病数量每提升1个等级） =====\n")
fit_trend_chronic_group <- glm(doctor ~ chronic_group_score, data = df_analy, family = binomial)
summary(fit_trend_chronic_group)
exp(coef(fit_trend_chronic_group))
exp(confint(fit_trend_chronic_group))
#慢性病数量与住院率的卡方检验与线性趋势检验
# 1. 慢性病数据预处理
chronic_group_order <- c("0种","1种" ,"2种", "3种", "4种", "5种及以上")
df_analy$chronic_group_fac <- factor(df_analy$chronic_group, levels = chronic_group_order)
df_analy$chronic_group_fac <- relevel(df_analy$chronic_group_fac, ref = "0种")
df_analy$chronic_group_score <- as.integer(df_analy$chronic_group_fac)
# 2. 构建慢性病数量×住院列联表
tab_chronic_group_hosp <- table(df_analy$chronic_group_fac, df_analy$hospital)
cat("===== 慢性病数量-住院列联表 =====\n")
print(tab_chronic_group_hosp)
cat("===== 各组住院率 =====\n")
print(prop.table(tab_chronic_group_hosp, margin = 1))
# 3. Pearson卡方检验
cat("\n===== Pearson卡方检验慢性病数量&住院） =====\n")
chisq_chronic_group_hosp <- chisq.test(tab_chronic_group_hosp)
print(chisq_chronic_group_hosp)
# 4. Mantel线性趋势卡方检验
cat("\n===== Mantel线性趋势检验（慢性病数量） =====\n")
trend_mh_chronic_group_hosp <- MHChisqTest(tab_chronic_group_hosp, srow = 1:6, scol = c(0, 1))
print(trend_mh_chronic_group_hosp)
# 5. 分组单因素Logistic：各组粗OR、95%CI、P值
cat("\n===== 分组粗OR、95%CI、P值（慢性病数量） =====\n")
fit_group_chronic_group_hosp <- glm(hospital ~ chronic_group_fac, data = df_analy, family = binomial)
summary(fit_group_chronic_group_hosp)          # 输出P值
exp(coef(fit_group_chronic_group_hosp))        # 粗OR
exp(confint(fit_group_chronic_group_hosp))     # OR 95%置信区间
# 6. 线性趋势Logistic检验
cat("\n===== 线性趋势OR（慢性病数量每提升1个等级） =====\n")
fit_trend_chronic_group_hosp <- glm(hospital ~ chronic_group_score, data = df_analy, family = binomial)
summary(fit_trend_chronic_group_hosp)
exp(coef(fit_trend_chronic_group_hosp))
exp(confint(fit_trend_chronic_group_hosp))

#医疗保险与门诊率的卡方检验
# 1. 交叉表
tab_ins <- table(df_analy$ins, df_analy$doctor)
tab_ins
# 2. 行就诊率（百分比）
prop.table(tab_ins, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_ins, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_ins <- glm(doctor ~ ins, data = df_analy, family = "binomial")
exp(coef(fit_single_ins))          # OR值
exp(confint(fit_single_ins))       # 95%置信区间
summary(fit_single_ins)            # P值
#医疗保险与住院率的卡方检验
# 1. 交叉表
tab_ins_hosp <- table(df_analy$ins, df_analy$hospital)
tab_ins_hosp
# 2. 行就诊率（百分比）
prop.table(tab_ins_hosp, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_ins_hosp, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_ins_hosp <- glm(hospital ~ ins, data = df_analy, family = "binomial")
exp(coef(fit_single_ins_hosp))          # OR值
exp(confint(fit_single_ins_hosp))       # 95%置信区间
summary(fit_single_ins_hosp)            # P值

#居住地与门诊率的卡方检验
# 1. 交叉表
tab_rural <- table(df_analy$rural, df_analy$doctor)
tab_rural
# 2. 行就诊率（百分比）
prop.table(tab_rural, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_rural, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_rural <- glm(doctor ~ rural, data = df_analy, family = "binomial")
exp(coef(fit_single_rural))          # OR值
exp(confint(fit_single_rural))       # 95%置信区间
summary(fit_single_rural)            # P值
#居住地与住院率的卡方检验
# 1. 交叉表
tab_rural_hosp <- table(df_analy$rural, df_analy$hospital)
tab_rural_hosp
# 2. 行住院率（百分比）
prop.table(tab_rural_hosp, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_rural_hosp, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_rural_hosp <- glm(hospital ~ rural, data = df_analy, family = "binomial")
exp(coef(fit_single_rural_hosp))          # OR值
exp(confint(fit_single_rural_hosp))       # 95%置信区间
summary(fit_single_rural_hosp)            # P值

#性别与门诊率的卡方检验
# 1. 交叉表
tab_gender <- table(df_analy$gender, df_analy$doctor)
tab_gender
# 2. 行就诊率（百分比）
prop.table(tab_gender, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_gender, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_gender <- glm(doctor ~ gender, data = df_analy, family = "binomial")
exp(coef(fit_single_gender))          # OR值
exp(confint(fit_single_gender))       # 95%置信区间
summary(fit_single_gender)            # P值
#性别与住院率的卡方检验
# 1. 交叉表
tab_gender_hosp <- table(df_analy$gender, df_analy$hospital)
tab_gender_hosp
# 2. 行住院率（百分比）
prop.table(tab_gender_hosp, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_gender_hosp, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_gender_hosp <- glm(hospital ~ gender, data = df_analy, family = "binomial")
exp(coef(fit_single_gender_hosp))          # OR值
exp(confint(fit_single_gender_hosp))       # 95%置信区间
summary(fit_single_gender_hosp)            # P值

#婚姻状况与门诊率的卡方检验
# 1. 交叉表
tab_marry <- table(df_analy$marry, df_analy$doctor)
tab_marry
# 2. 行就诊率（百分比）
prop.table(tab_marry, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_marry, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_marry <- glm(doctor ~ marry, data = df_analy, family = "binomial")
exp(coef(fit_single_marry))          # OR值
exp(confint(fit_single_marry))       # 95%置信区间
summary(fit_single_marry)            # P值
#婚姻状况与住院率的卡方检验
# 1. 交叉表
tab_marry_hosp <- table(df_analy$marry, df_analy$hospital)
tab_marry_hosp
# 2. 行住院率（百分比）
prop.table(tab_marry_hosp, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_marry_hosp, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_marry_hosp <- glm(hospital ~ marry, data = df_analy, family = "binomial")
exp(coef(fit_single_marry_hosp))          # OR值
exp(confint(fit_single_marry_hosp))       # 95%置信区间
summary(fit_single_marry_hosp)            # P值

#吸烟史与门诊率的卡方检验
# 1. 交叉表
tab_smoken <- table(df_analy$smoken, df_analy$doctor)
tab_smoken
# 2. 行就诊率（百分比）
prop.table(tab_smoken, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_smoken, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_smoken <- glm(doctor ~ smoken, data = df_analy, family = "binomial")
exp(coef(fit_single_smoken))          # OR值
exp(confint(fit_single_smoken))       # 95%置信区间
summary(fit_single_smoken)            # P值
#吸烟史与住院率的卡方检验
# 1. 交叉表
tab_smoken_hosp <- table(df_analy$smoken, df_analy$hospital)
tab_smoken_hosp
# 2. 行住院率（百分比）
prop.table(tab_smoken_hosp, 1)*100
# 3. Pearson卡方检验
chisq.test(tab_smoken_hosp, correct = FALSE)
# 4. 单因素Logistic回归（粗OR、95%CI、P值）
fit_single_smoken_hosp <- glm(hospital ~ smoken, data = df_analy, family = "binomial")
exp(coef(fit_single_smoken_hosp))          # OR值
exp(confint(fit_single_smoken_hosp))       # 95%置信区间
summary(fit_single_smoken_hosp)            # P值


#多因素回归分析
#门诊率的多重共线性检验
install.packages("car")
library(car)
fit_test <- glm(
  doctor ~ chronic_group_score + srh_score + age_group + gender + edu_score + ins + consume_score +smoken,
  family = binomial,
  data = df_analy
)
# 输出VIF值
vif(fit_test)

#住院率的多重共线性检验
fit_test_hosp <- glm(
  hospital ~ chronic_group_score + srh_score + age_group + gender + marry + ins + consume_score +smoken + rural,
  family = binomial,
  data = df_analy
)
# 输出VIF值
vif(fit_test_hosp)

#门诊率的回归
fit_doc <- glm(
  doctor ~ chronic_group_score + srh_score + age_group + gender + edu_score + ins + consume_score +smoken,
  family = binomial,
  data = df_analy
)
# 提取OR与95%CI P值
exp(coef(fit_doc))
exp(confint(fit_doc))
summary(fit_doc)

#住院率的回归
fit_hosp <- glm(
  hospital ~ chronic_group_score + srh_score + age_group + gender + marry + ins + consume_score +smoken + rural,
  family = binomial,
  data = df_analy
)
# 提取OR值与置信区间 P值
exp(coef(fit_hosp))
exp(confint(fit_hosp))
summary(fit_hosp)

# 门诊结局汇总表
out_doc <- cbind(
  OR = exp(coef(fit_doc)),
  LCI = exp(confint.default(fit_doc)[,1]),
  UCI = exp(confint.default(fit_doc)[,2]),
  P = summary(fit_doc)$coefficients[,4]
)
round(out_doc, 4)

# 住院结局汇总表
out_hosp <- cbind(
  OR = exp(coef(fit_hosp)),
  LCI = exp(confint.default(fit_hosp)[,1]),
  UCI = exp(confint.default(fit_hosp)[,2]),
  P = summary(fit_hosp)$coefficients[,4]
)
round(out_hosp, 4)

#拟合优度检验
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(fit_doc$y, fitted(fit_doc))
hoslem.test(fit_hosp$y, fitted(fit_hosp))

#城乡分层分析
# 农村组门诊模型
fit_doc_rural <- glm(doctor ~ chronic_group_score + srh_score + age_group + gender + edu_score + ins + consume_score +smoken,
                     data = subset(df_analy, rural == "农村"),
                     family = binomial)
# 城镇组门诊模型
fit_doc_urban <- glm(doctor ~ chronic_group_score + srh_score + age_group + gender + edu_score + ins + consume_score +smoken,
                     data = subset(df_analy, rural == "城市"),
                     family = binomial)
exp(cbind(OR = coef(fit_doc_rural), confint(fit_doc_rural)))
exp(cbind(OR = coef(fit_doc_urban), confint(fit_doc_urban)))
# 农村组住院模型
fit_hosp_rural <- glm(hospital ~ chronic_group_score + srh_score + age_group + gender + marry + ins + consume_score +smoken,
                      data = subset(df_analy, rural == "农村"),
                      family = binomial)
# 城镇组住院模型
fit_hosp_urban <- glm(hospital ~ chronic_group_score + srh_score + age_group + gender + marry + ins + consume_score +smoken,
                      data = subset(df_analy, rural == "城市"),
                      family = binomial)
exp(cbind(OR = coef(fit_hosp_rural), confint(fit_hosp_rural)))
exp(cbind(OR = coef(fit_hosp_urban), confint(fit_hosp_urban)))

#年龄分层分析
# 第1组：65-74岁门诊
fit_doc_young <- glm(doctor ~ chronic_group_score + srh_score  + gender + edu_score + ins + consume_score +smoken,
                     data = subset(df_analy, age_group == "65-74岁"),
                     family = binomial)
# 第2组：75-84岁门诊
fit_doc_mid <- glm(doctor ~ chronic_group_score + srh_score  + gender + edu_score + ins + consume_score +smoken,
                   data = subset(df_analy, age_group == "75-84岁"),
                   family = binomial)
# 第3组：85岁及以上门诊
fit_doc_old <- glm(doctor ~ chronic_group_score + srh_score  + gender + edu_score + ins + consume_score +smoken,
                   data = subset(df_analy, age_group == "85岁及以上"),
                   family = binomial)
# 提取OR与95%CI
exp(cbind(OR=coef(fit_doc_young), confint(fit_doc_young)))
exp(cbind(OR=coef(fit_doc_mid), confint(fit_doc_mid)))
exp(cbind(OR=coef(fit_doc_old), confint(fit_doc_old)))

# 第1组：65-74岁住院
fit_hosp_young <- glm(hospital ~ chronic_group_score + srh_score + gender + marry + ins + consume_score +smoken + rural,
                      data = subset(df_analy, age_group == "65-74岁"),
                      family = binomial)
# 第2组：75-84岁住院
fit_hosp_mid <- glm(hospital ~ chronic_group_score + srh_score + gender + marry + ins + consume_score +smoken + rural,
                    data = subset(df_analy, age_group == "75-84岁"),
                    family = binomial)
# 第3组：85岁及以上住院
fit_hosp_old <- glm(hospital ~ chronic_group_score + srh_score + gender + marry + ins + consume_score +smoken + rural,
                    data = subset(df_analy, age_group == "85岁及以上"),
                    family = binomial)
# 提取OR与95%CI
exp(cbind(OR=coef(fit_hosp_young), confint(fit_hosp_young)))
exp(cbind(OR=coef(fit_hosp_mid), confint(fit_hosp_mid)))
exp(cbind(OR=coef(fit_hosp_old), confint(fit_hosp_old)))


#从家庭年人均消费视角进行公平性分析
install.packages("rineq")
library(rineq)
library(dplyr)
#门诊集中指数
df_analy$doctor_num <- as.numeric(df_analy$doctor) - 1#将因子转为数值：“否”=1→0，“是”=2→1
ci_doc1<-ci(
  ineqvar = df_analy$hhcperc,
  outcome = df_analy$doctor_num,
  type = "CI"
)
summary(ci_doc1)
#门诊"Erreygers"校正指数
y_bar <- mean(df_analy$doctor_num)
E_ci <- (4 * y_bar / (1 - y_bar)) * 0.09445796
E_ci

#门诊集中曲线图
# 1. 按消费从小到大排序数据
df_sort <- df_analy[order(df_analy$hhcperc), ]
# 2. 计算累计人群占比（横轴x）
df_sort$cum_pop <- seq(1:nrow(df_sort)) / nrow(df_sort)
# 3. 计算门诊就医累计占比（纵轴y）
df_sort$cum_doc <- cumsum(df_sort$doctor_num) / sum(df_sort$doctor_num)
ggplot() +
  # 集中曲线
  geom_line(data = df_sort, aes(x = cum_pop, y = cum_doc),
            linewidth = 1.2, color = "#2E86AB") +
  # 45°公平对角线
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 1, color = "black") +
  labs(
    x = "累计人口占比",
    y = "累计门诊服务利用占比",
    title = "门诊卫生服务利用集中曲线"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#住院集中指数
df_analy$hospital_num <- as.numeric(df_analy$hospital) - 1#将因子转为数值：“否”=1→0，“是”=2→1
ci_hosp1<-ci(
  ineqvar = df_analy$hhcperc,
  outcome = df_analy$hospital_num,
  type = "CI"
)
summary(ci_hosp1)
#住院"Erreygers"校正指数
y_bar <- mean(df_analy$hospital_num)
E_ci <- (4 * y_bar / (1 - y_bar)) * 0.09349178
E_ci

#住院集中曲线图
# 1. 按消费从小到大排序数据
df_sort <- df_analy[order(df_analy$hhcperc), ]
# 2. 计算累计人群占比（横轴x）
df_sort$cum_pop <- seq(1:nrow(df_sort)) / nrow(df_sort)
# 3.计算住院就医累计占比（纵轴y）
df_sort$cum_hosp <- cumsum(df_sort$hospital_num) / sum(df_sort$hospital_num)
ggplot() +
  # 集中曲线
  geom_line(data = df_sort, aes(x = cum_pop, y = cum_hosp),
            linewidth = 1.2, color = "#2E86AB") +
  # 45°公平对角线
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 1, color = "black") +
  labs(
    x = "累计人口占比",
    y = "累计住院服务利用占比",
    title = "住院卫生服务利用集中曲线"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
