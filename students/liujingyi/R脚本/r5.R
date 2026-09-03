####----------------------整理tpm.os数据----------------------####
rm(list = ls())
load('GDC_COAD_data.rda')
load('GDC_COAD_count_tpm.rda')
str(tpm_log2)#查看数据格式，是否为numeric
tpm.os <- t(tpm_log2)#行列转置
tpm.os <- as.data.frame(tpm.os)#转化为数据框
library(tibble)
tpm.os <- rownames_to_column(.data = tpm.os,var = 'ID') #行名转为列, ID
tpm.os <- merge(tpm.os,clinical_OS.time,by ='ID')#两个数据框合并
library(tidyverse)
tpm.os <- select(.data = tpm.os,'ID','OS.time','OS',everything())#列名以'ID','os.time','os' 顺序
tpm.os <- arrange(tpm.os,OS)#对OS列进行排序
tpm.os$OS.time <-as.numeric(tpm.os$OS.time) /365 #天转化为年,转化为数字
tpm.os$os <-as.numeric(tpm.os$OS)#转化为数字格式
library(dplyr)
tpm.os <- tpm.os %>% filter(!is.na(OS.time))
write.csv(tpm.os,file ='GDC_COAD_tpm_os.csv')#csv保存
save(matrix,matrix0,clinical,clinical_OS.time,matrix_tpm,tpm_log2,tpm.os,file = 'GDC_COAD_data.rda') 


####----------------------gene.os数据----------------------####
setwd('D:/gdc-client')#设置工作路径
load('GDC_COAD_data.rda')
library(survival)
library(survminer)
library(tidyverse)
gene <- 'CLDN18'
gene.os <- tpm.os
gene.os$group <- ifelse(gene.os$CLDN18 < median(gene.os$CLDN18),"low","high")
gene.os <- select(gene.os,group,OS,OS.time,CLDN18)
str(gene.os)
gene.os$OS <- as.numeric(gene.os$OS)
str(gene.os$OS)
#用 surv_cutpoint 找最佳截断值
library(survminer)
cutpoint <- surv_cutpoint(gene.os, time = "OS.time", event = "OS", variables = "CLDN18")
cutpoint
gene.os$group2 <- ifelse(gene.os$CLDN18 > cutpoint$cutpoint[1,"cutpoint"], "High", "Low")

####----------------------统计描述----------------------####
library(dplyr)
library(tidyr)
library(survival)
library(survminer)
library(ggplot2)
#install.packages("table1")
library(table1) 
library(tidyverse)
description <- 'CLDN18'
description.os <- tpm.os
description.os <- select(description.os,ID,OS,OS.time,CLDN18)
str(description.os)
description.os$OS <- as.numeric(description.os$OS)
str(description.os$OS)
#用 surv_cutpoint 找最佳截断值
library(survminer)
cutpoint <- surv_cutpoint(gene.os, time = "OS.time", event = "OS", variables = "CLDN18")
cutpoint
description.os$group <- ifelse(description.os$CLDN18 > cutpoint$cutpoint[1,"cutpoint"], "High", "Low")
colnames(clinical_OS.time)
colnames(description.os)
clinical_subset <- clinical_OS.time %>%
  select(ID, age, gender, stage, T, M, N)# 从 clinical_OS.time 中只提取需要的列（ID 和六个临床变量）
description.os <- description.os %>%
  left_join(clinical_subset, by = "ID")# 按 ID 左连接（保留 tpm.os 中的所有行，匹配上 clinical_subset 的信息）
description.os <- description.os %>%
  mutate(age_group = ifelse(age < 60, "< 60", "≥ 60"))# 按60岁创建年龄分组
head(description.os)
colnames(description.os)# 检查合并后的数据
table(description.os$group)
description.os <- description.os %>%
  mutate(
    stage = ifelse(stage == "'--" | stage == "", NA, stage),
    T = ifelse(T == "'--" | T == "", NA, T),
    M = ifelse(M == "'--" | M == "", NA, M),
    N = ifelse(N == "'--" | N == "", NA, N)
  )#stage、T、M、N 中有缺失值，需要先处理# 将 '--' 转为 NA
colSums(is.na(description.os[, c("age", "gender", "stage", "T", "M", "N","age_group")]))#查看缺失值
library(dplyr)
description.os <- description.os %>%
  filter(!is.na(stage) & !is.na(M))# 删除stage和T里面的缺失值
#stage分组
description.os <- description.os %>%
  mutate(
    stage_simple = case_when(
      # 先处理合并后的类别
      stage %in% c("Stage I", "Stage IA", "Stage IB", 
                   "Stage II", "Stage IIA", "Stage IIB", "Stage IIC") ~ "Early",
      stage %in% c("Stage III", "Stage IIIA", "Stage IIIB", "Stage IIIC",
                   "Stage IV", "Stage IVA", "Stage IVB") ~ "Late",
    ),
    # 转为因子，并指定顺序
    stage_simple = factor(stage_simple, levels = c("Early", "Late"))
  )

# 检查合并后的分布
table(description.os$stage_simple, description.os$group)

# 合并 T 分期（T1/T2 合并为早期T，T3/T4 合并为晚期T）
description.os <- description.os %>%
  mutate(
    T_simple = case_when(
      T %in% c("T1", "T2", "Tis") ~ "T1-T2",
      T %in% c("T3", "T4", "T4a", "T4b") ~ "T3-T4",
     ),
    # 合并 M 分期（M0、M1、MX）
    M_simple = case_when(
      M == "M0" ~ "M0",
      M %in% c("M1", "M1a", "M1b") ~ "M1",
      M == "MX" ~ "MX",
    ),
    # 合并 N 分期（N0、N1、N2）
    N_simple = case_when(
      N == "N0" ~ "N0",
      N %in% c("N1", "N1a", "N1b", "N1c") ~ "N1",
      N %in% c("N2", "N2a", "N2b") ~ "N2",
    ),
  )
# 转为因子
description.os$T_simple <- factor(description.os$T_simple, levels = c("T1-T2", "T3-T4"))
description.os$M_simple <- factor(description.os$M_simple, levels = c("M0", "M1", "MX"))
description.os$N_simple <- factor(description.os$N_simple, levels = c("N0", "N1", "N2"))
description.os$gender <- factor(description.os$gender, levels = c("female", "male"))# 把 gender 转为因子
colSums(is.na(description.os[, c("age", "gender", "stage", "T", "M", "N","age_group")]))#查看缺失值
table1(~ age_group + gender + stage_simple + T_simple + M_simple + N_simple | group, data = description.os)# 使用 table1 包制作按 group 分层的基线表
#保存table1
#install.packages("flextable")
library(flextable)
tab1 <- table1(~ age_group + gender + stage_simple + T_simple + M_simple + N_simple | group, data = description.os)
tab1_df <- as.data.frame(tab1)# 把 table1 转为数据框
ft <- flextable(tab1_df)#创建 flextable 对象
# 美化表格
ft <- theme_vanilla(ft)      # 使用简洁主题
ft <- autofit(ft)            # 自动调整列宽
ft <- bold(ft, part = "header")  # 表头加粗
ft <- align(ft, align = "center", part = "all")  # 所有文字居中
#保存为 Word 文档
save_as_docx(ft, path = "Table1.docx")

#箱线图
boxplot_cl <- ggplot(description.os, aes(x = group, y = .data[["CLDN18"]], fill = group)) +
  geom_boxplot() +
  labs(
    title = "CLDN18 expression by risk group",
    x = "Group",
    y = "CLDN18 expression (log2 TPM + 1)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "blue", "High" = "red"))
print(boxplot_cl)
# 保存箱线图
ggsave("CLDN18_boxplot.png", boxplot_cl, width = 6, height = 5)

####----------------------统计推断----------------------####
# 1 年龄的 卡方检验（高表达组 vs 低表达组）
chisq.test(table(description.os$group, description.os$age_group))
# 2 性别的卡方检验
chisq.test(table(description.os$group, description.os$gender))
# 3 分期的卡方检验
chisq.test(table(description.os$group, description.os$stage_simple))
# 4 T的卡方检验
chisq.test(table(description.os$group, description.os$T_simple))
#5 M的卡方检验
chisq.test(table(description.os$group, description.os$M_simple))
# 6 N的卡方检验
chisq.test(table(description.os$group, description.os$N_simple))
####----------------------绘制生存曲线----------------------####
?ggsurvplot
fit <- survfit(Surv(OS.time,OS)~group2, data=gene.os)
KM <- ggsurvplot(fit,data = gene.os,
                 conf.int=T,#是否展示置信区间
                 pval=T,pval.size=5,#是否展示P值，及P值大小
                 legend.labs=c('High','Low'),#group命名
                 legend.title='CLDN18 exp',xlab='Time(years)',#图例及x轴名
                 legend=c(0.9,0.9),font.legend=10,#图例位置及大小
                 risk.table = T,#是否展示risk table
                 break.time.by=1,#按1年间隔展示
                 palette = c('red','blue'),#自定义颜色
                 surv.median.line = 'hv',#添加中位数参考线，h水平，v垂直，none不添加
                 risk.table.height=.3)#数值(在[0-1]中)，指定主生存图下所有表格的总体高度。
KM
#pdf保存km曲线
pdf(file = 'KM2_CLDN18.pdf',width = 6,height = 6)
KM
dev.off()
####----------------------CLDN18单因素Cox回归----------------------####
library(survival)
library(survminer)
library(ggplot2)
library(data.table)
library(dplyr)
gene_cox <- gene.os[, c("OS.time", "OS", "CLDN18")]
gene_cox <- gene_cox[gene_cox$OS.time > 0, ]
nrow(gene_cox)
gene_cox <- as.data.frame(gene_cox)# 把 use_cox 转为数据框
cox_cldn18 <- coxph(Surv(OS.time, OS) ~ `CLDN18`, data = gene_cox)  # 如果列名是 "CLDN18"
# 查看结果
summary(cox_cldn18)
# 加载森林图绘图包
library(survminer)
# 用 ggforest 直接画
forest_plot <- ggforest(cox_cldn18, data = gene_cox, 
         main = "Hazard Ratio of CLDN18", 
         cpositions = c(0.02, 0.22, 0.4), 
         fontsize = 1, 
         noDigits = 3)
forest_plot
# 保存森林图
ggsave("CLDN18_forest_plot.png", plot = forest_plot, width = 10, height = 7, dpi = 300)




####----------------------单因素Cox回归----------------------####
use_cox <- tpm.os
use_cox <- as.data.frame(use_cox)# 把 use_cox 转为数据框
td <- use_cox
outResult <- data.frame()
sigGenes <- c("surstat","surtime")
class(td$OS)
td$OS <- as.numeric(td$OS)
library(data.table)
gene_name <- fread("gene_name.tsv", data.table = FALSE)
gene_name_subset <- gene_name[, 1:3]
gene_name_clean <- gene_name_subset[-c(1:4), ]
protein_coding_names <- gene_name_clean$gene_name[gene_name_clean$gene_type == "protein_coding"]# 提取 protein_coding 的基因名
keep_cols <- intersect(colnames(td), protein_coding_names)
td_protein <- td[, c("ID","OS.time", "OS", keep_cols)]# 只保留 td 里属于 protein_coding 的列
dim(td_protein)
head(colnames(td_protein), 10)  # 查看前 10 个列名
#基因列从第 4 列开始
expr_data <- td_protein[, 4:ncol(td_protein)]
# 计算每个基因在所有样本中的平均表达量（或中位数）
gene_means <- colMeans(expr_data, na.rm = TRUE)
# 只保留平均表达量 > 0.1 的基因
keep_genes <- names(gene_means[gene_means > 0.1])
td_filtered <- td_protein[, c("ID","OS.time", "OS", keep_genes)]
# 看看还剩多少
ncol(td_filtered) - 3
grep("CLDN18", colnames(td_filtered), ignore.case = TRUE, value = TRUE)
# 计算每个基因的方差
gene_vars <- apply(td_filtered[, 4:ncol(td_filtered)], 2, var, na.rm = TRUE)
# 只保留方差大于 75% 分位数的基因（即变异最大的前 25%）
threshold <- quantile(gene_vars, 0.75, na.rm = TRUE)
keep_genes <- names(gene_vars[gene_vars > threshold])
td_filtered <- td_filtered[, c("ID","OS.time", "OS", keep_genes)]
grep("CLDN18", colnames(td_filtered), ignore.case = TRUE, value = TRUE)
ncol(td_filtered) - 3
library(survival)
genes <- colnames(td_filtered)[4:ncol(td_filtered)]
p_values <- numeric(length(genes))

for (i in seq_along(genes)) {
  formula <- as.formula(paste0("Surv(OS.time, OS) ~ `", genes[i], "`"))
  cox_result <- tryCatch(
    coxph(formula, data = td_filtered),
    error = function(e) NULL
  )
  if (!is.null(cox_result)) {
    p_values[i] <- summary(cox_result)$coefficients[, "Pr(>|z|)"]
  } else {
    p_values[i] <- NA
  }
}

# 只保留 p < 0.05 的基因
keep_genes <- genes[!is.na(p_values) & p_values < 0.05]
td_filtered <- td_filtered[, c("ID","OS.time", "OS", keep_genes)]
grep("CLDN18", colnames(td_filtered), ignore.case = TRUE, value = TRUE)
ncol(td_filtered) - 3


result_list <- lapply(colnames(td_filtered)[4:ncol(td_filtered)], function(i) {
  formula <- as.formula(paste0("Surv(OS.time, OS) ~ `", i, "`"))
  tdxcox <- coxph(formula, data = td_filtered)
  tdxcoxSummary <- summary(tdxcox)
  
  data.frame(
    id = i,
    HR = tdxcoxSummary$conf.int[, "exp(coef)"],
    L95CI = tdxcoxSummary$conf.int[, "lower .95"],
    H95CI = tdxcoxSummary$conf.int[, "upper .95"],
    pvalue = tdxcoxSummary$coefficients[, "Pr(>|z|)"]
  )
})

# 一次性合并所有结果
outResult <- do.call(rbind, result_list)
# 转成数值型
outResult$pvalue <- as.numeric(outResult$pvalue)
outResult$HR <- as.numeric(outResult$HR)
outResult$L95CI <- as.numeric(outResult$L95CI)
outResult$H95CI <- as.numeric(outResult$H95CI)

library(tibble)
library(dplyr)
outResult <- column_to_rownames(outResult,var = 'id')
outResult <- arrange(outResult,HR)
outResultt <- as.matrix(outResult)
colnames(outResult)
HR <- outResult[,1:3] #注意第123列为 "HR""L95CI""H95CI"
#可视化作图
# ----- 1. 准备数据-----
# 按 HR 排序（从小到大）
plot_data <- outResult[order(outResult$HR), ]

# 提取数据
gene_names <- rownames(plot_data)
HR <- plot_data$HR
lower <- plot_data$L95CI
upper <- plot_data$H95CI
p_vals <- plot_data$pvalue

# ----- 2. 设置每页显示的基因数 -----
genes_per_page <- 30

# 计算总页数
total_genes <- nrow(plot_data)
n_pages <- ceiling(total_genes / genes_per_page)

# ----- 3. 循环分页画图并保存 -----
for (page in 1:n_pages) {
  
  # 计算当前页的起止行
  start_row <- (page - 1) * genes_per_page + 1
  end_row <- min(page * genes_per_page, total_genes)
  
  # 提取当前页的数据
  page_genes <- plot_data[start_row:end_row, ]
  page_names <- rownames(page_genes)
  page_HR <- page_genes$HR
  page_lower <- page_genes$L95CI
  page_upper <- page_genes$H95CI
  page_p <- page_genes$pvalue
  
  # 构建当前页的表格文字（表头 + 基因）
  tabletext <- cbind(
    c("Gene", page_names),
    c("HR (95% CI)", paste0(round(page_HR, 2), " (", round(page_lower, 2), "-", round(page_upper, 2), ")")),
    c("P value", round(page_p, 4))
  )
  
  # 打开 PDF 设备（每页一个 PDF）
  pdf(file = paste0("Forest_plot_page_", page, ".pdf"), 
      width = 10, height = 8)
  # 加载森林图绘图包
  library(forestplot)
  # 画森林图
  p <- forestplot(
    labeltext = tabletext,
    mean = c(NA, page_HR),
    lower = c(NA, page_lower),
    upper = c(NA, page_upper),
    zero = 1,
    boxsize = 0.15,
    line.margin = 0.1,
    col = fpColors(box = "royalblue", line = "darkblue", zero = "red"),
    xlab = "Hazard Ratio (95% CI)",
    title = paste0("Univariate Cox Regression Forest Plot (Page ", page, "/", n_pages, ")")
  )
  
  p
  
  # 关闭 PDF 设备
  dev.off()
  
  cat("已保存第", page, "/", n_pages, "页，包含", nrow(page_genes), "个基因\n")
}

cat("所有森林图已保存！共", n_pages, "个 PDF 文件。\n")

####----------------------Lasso-cox降维----------------------####
#install.packages("glmnet")
library(glmnet)
#设置随机种子
set.seed(123)
dat<-td_filtered
str(dat)
sum(dat$OS.time <= 0)
dat <- dat[dat$OS.time > 0, ]
#构建Lasso回归模型x，y
x <- as.matrix(dat[,c(4:ncol(dat))])
y <- Surv(dat$OS.time,dat$OS)
fit <- glmnet(x, y, family = "cox", nfolds = 10, lambda.min.ratio = 0.01)
#c-index
cvfit <- cv.glmnet(x,y,family='cox',type.measure = 'C',nfolds = 10)
pdf(file = 'lasso.c-index.pdf',width = 6,height = 6)
plot(cvfit)
abline(v=log(c(cvfit$lambda.min, cvfit$lambda.1se)),lty='dashed')
dev.off()
plot(cvfit)
#deviance
cvfit <- cv.glmnet(x,y,family='cox',type.measure = 'deviance',nfolds=10)
pdf(file = 'lasso.cvfit.pdf',width = 6,height = 6)
plot(cvfit)
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty='dashed')
dev.off()
#coefficients
pdf(file = 'lasso.lambda.pdf',width = 6,height = 6)
plot(fit,xvar = 'lambda',label = T)
abline(v=log (c(cvfit$lambda.min)),lty='dashed')
dev.off()
#输出lasso结果
coef <- coef(fit,s=cvfit$lambda.min)
index <- which(coef!=0)
actcoef <- coef[index]
lassogenes <- row.names(coef)[index]
lassogenes
genecoef <- cbind(Gene=lassogenes,Coef=actcoef)
exp3 <- select(dat,c(1:3,lassogenes))
write.csv(exp3,file = 'lasso.tpm.csv')

####----------------------多因素Cox----------------------####
setwd('D:/gdc-client')#设置工作路径
library(survival)
library(survminer)
library(tidyverse)
#数据导入
det <- read.csv(file = 'lasso.tpm.csv',header = T,row.names =1)
rownames(det) <- NULL
det <- column_to_rownames(det,var = 'ID')
#多因素cox
multicox <- coxph(Surv(OS.time,OS)~.,data = det)
multicoxSum <- summary(multicox)
multicoxSum
#colnames(multicoxSum$conf.int)
multicoxres<-cbind(HR=multicoxSum$conf.int[,'exp(coef)'],
                   HR.95L=multicoxSum$conf.int[,'lower .95'],
                   HR.95H=multicoxSum$conf.int[,'upper .95'],
                   pvalue=multicoxSum$coefficients[,"Pr(>|z|)"])
view(multicoxres)
rownames(multicoxres)
write.csv(multicoxres,file ='多因素.csv',row.names = T)
rt <- read.csv('多因素.csv',header = T,row.names = 1)
diff <- rt %>% filter(pvalue < 0.05) %>% row.names()
diff
exp4 <- select(det,c(1:2,diff))
write.csv(exp4,file ='多因素tpm.csv',row.names = T)
#可视化作图
#多因素cox
colnames(det)
library(survival)
library(forestplot)
# 确保数据没有 NA
det_clean <- det[complete.cases(det), ]
# 重新建模
multicox <- coxph(Surv(OS.time, OS) ~ ., data = det_clean, model = TRUE)
# 查看结果
summary(multicox)
# 提取结果
cox_sum <- summary(multicox)
# 提取变量名
var_names <- rownames(cox_sum$coefficients)
# 提取 HR、CI、P 值
HR <- cox_sum$coefficients[, "exp(coef)"]
CI_lower <- cox_sum$conf.int[, "lower .95"]
CI_upper <- cox_sum$conf.int[, "upper .95"]
P_val <- cox_sum$coefficients[, "Pr(>|z|)"]
# 格式化文字
hr_text <- sprintf("%.2f", HR)
ci_text <- sprintf("%.2f (%.2f-%.2f)", HR, CI_lower, CI_upper)
p_text <- ifelse(P_val < 0.001, "<0.001", sprintf("%.3f", P_val))
# 构建表格 矩阵格式
tabletext <- cbind(
  c("Variable", var_names),
  c("HR (95% CI)", ci_text),
  c("P value", p_text)
)
# 画森林图（用 forestplot）
forestplot(
  labeltext = tabletext,
  mean = c(NA, HR),
  lower = c(NA, CI_lower),
  upper = c(NA, CI_upper),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.1,
  col = fpColors(box = "royalblue", line = "darkblue", zero = "red"),
  xlab = "Hazard Ratio (95% CI)",
  title = "Multivariate Cox Regression Forest Plot"
)
# 保存为 PDF
pdf(file = "多-森林图.pdf", width = 10, height = max(6, length(var_names) * 0.4))
forestplot(
  labeltext = tabletext,
  mean = c(NA, HR),
  lower = c(NA, CI_lower),
  upper = c(NA, CI_upper),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.1,
  col = fpColors(box = "royalblue", line = "darkblue", zero = "red"),
  xlab = "Hazard Ratio (95% CI)",
  title = "Multivariate Cox Regression Forest Plot"
)
dev.off()


setwd('D:/gdc-client')
library(survival)
library(forestplot)

# 1. 读取数据
exp4 <- read.csv('多因素tpm.csv', header = T, row.names = 1)

# 2. 建模
multicox <- coxph(Surv(OS.time, OS) ~ ., data = exp4)
sum_multicox <- summary(multicox)

# 3. 提取结果
var_names <- rownames(sum_multicox$coefficients)
HR <- sum_multicox$coefficients[, "exp(coef)"]
L95CI <- sum_multicox$conf.int[, "lower .95"]
H95CI <- sum_multicox$conf.int[, "upper .95"]
P_val <- sum_multicox$coefficients[, "Pr(>|z|)"]

# 4. 构建表格
tabletext <- cbind(
  c("Variable", var_names),
  c("HR (95% CI)", paste0(sprintf("%.2f", HR), " (", sprintf("%.2f", L95CI), "-", sprintf("%.2f", H95CI), ")")),
  c("P value", ifelse(P_val < 0.001, "<0.001", sprintf("%.3f", P_val)))
)

# 5. 画图
forestplot(
  labeltext = tabletext,
  mean = c(NA, HR),
  lower = c(NA, L95CI),
  upper = c(NA, H95CI),
  zero = 1,
  col = fpColors(box = "royalblue", line = "darkblue", zero = "red"),
  xlab = "Hazard Ratio (95% CI)",
  title = "Multivariate Cox Regression (p < 0.05)"
)

pdf('多因素_显著基因_森林图.pdf', width = 10, height = 10)
forestplot(
  labeltext = tabletext,
  mean = c(NA, HR),
  lower = c(NA, L95CI),
  upper = c(NA, H95CI),
  zero = 1,
  col = fpColors(box = "royalblue", line = "darkblue", zero = "red"),
  xlab = "Hazard Ratio (95% CI)",
  title = "Multivariate Cox Regression (p < 0.05)"
)
dev.off()

install.packages("rmarkdown")
install.packages("knitr")
library(rmarkdown)
install.packages("rmarkdown", type = "binary")
