

#==================== 1 安装包====================
options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

install.packages("dunn.test") 
install.packages("stargazer") 
install.packages("mgcv")   
install.packages("car")     
install.packages("ResourceSelection") 
install.packages("forestplot")
install.packages("splines")

#====================2 加载数据集df_final====================
library(dplyr)            
setwd("D:/NHANES-2017-2018")
load("df_final_nhanes2017.RData") 

#定义变量
#定义连续血脂变量
lipid_con <- c("LBXHD","LBXTR","LBDLDL","LBXTC")   
#定义二分类血脂变量
lipid_bin <- c("high_tc","high_tg","low_hdl","high_ldl")    
#定义混杂变量（年龄、性别、种族、教育、收入、BMI、总热量）
confounder <- c("RIDAGEYR","RIAGENDR","RIDRETH1","DMDEDUC2","INDFMPIR","BMXBMI","DR1TKCAL")


#=====================3 基线血脂指标组间差异分析====================
#Kruskal+Dunn两两比较（连续血脂三组组间差异）
library(dunn.test)          # 基线检验，两两比较

for(var in lipid_con){
  cat("\n==================== 血脂指标：", var, " ====================\n")
  dunn.test(x = df_final[[var]], g = df_final$exposure_3, method = "bonferroni")
}


# 卡方检验（二分类血脂异常）
for(var in lipid_bin){
  cat("\n==================== 血脂异常：", var, " ====================\n")
  tab <- table(df_final$exposure_3, df_final[[var]])
  print(tab)
  chi_result <- chisq.test(tab)
  print(chi_result)
}


# Spearman秩相关（连续添加糖与血脂关联） 

for(var in lipid_con){
  cat("\n========== Spearman相关：DR1TSUGR vs " , var, " ==========\n")
  cor_result <- cor.test(
    df_final[[exposure_var]],
    df_final[[var]], 
    method = "spearman",
    exact = FALSE)
  print(cor_result)
}


#==================== 4 多因素回归分析 ====================
library(car)                # 回归、多重共线性
library(stargazer)          # 回归结果输出表格

# 自定义函数：生成三层回归公式
build_form <- function(outcome, expo) {
  
  # M2校正人口社会学混杂因素
  confounder_demo <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "INDFMPIR")
  # M3校正：M2、BMI、每日总热量摄入
  confounder_all <- c(confounder_demo, "BMXBMI", "DR1TKCAL")
  
  form_M1 <- as.formula(paste0(outcome, " ~ ", expo))
  form_M2 <- as.formula(paste0(outcome, " ~ ", expo, " + ", paste(confounder_demo, collapse = "+")))
  form_M3 <- as.formula(paste0(outcome, " ~ ", expo, " + ", paste(confounder_all, collapse = "+")))
  
  # 返回公式列表
  return(list(M1 = form_M1, M2 = form_M2, M3 = form_M3))
}


## 4.1 连续添加糖→连续血脂，分层多元线性回归，输出β系数，探究对血脂浓度的线性影响
library(stargazer)

for(y_out in lipid_con){
  cat("\n==================== 连续血脂结局：", y_out, " ====================\n")
  formula_list_con <- build_form(outcome = y_out, expo = "DR1TSUGR")
  lm_M1 <- lm(formula_list_con$M1, data = df_final)
  lm_M2 <- lm(formula_list_con$M2, data = df_final)
  lm_M3 <- lm(formula_list_con$M3, data = df_final)
#输出结果
  stargazer(lm_M1, lm_M2, lm_M3, 
            type = "text", 
            ci = TRUE, 
            digits = 3)
  # 导出CSV结果
  stargazer(lm_M1, lm_M2, lm_M3, 
            out = paste0("线性回归_",y_out,".csv"),
            ci = TRUE, digits = 3)
  #删除内存里临时的模型对象
  rm(lm_M1,lm_M2,lm_M3)
  gc() #释放内存
}


## 4.2连续添加糖→二分类血脂异常，分层Logistic回归，输出OR值
mod_store <- list()               #创建空列表储存全部M3模型
for(y_bin in lipid_bin){
  cat("\n==================== 二分类血脂异常结局：", y_bin, " ====================\n")
  form_list <- build_form(outcome = y_bin, expo = "DR1TSUGR")
  
  logit_M1 <- glm(form_list$M1, family = binomial(link = "logit"), data = df_final)
  logit_M2 <- glm(form_list$M2, family = binomial(link = "logit"), data = df_final)
  logit_M3 <- glm(form_list$M3, family = binomial(link = "logit"), data = df_final)

  #M3模型存入mod_store
  
  mod_store[[y_bin]] <- logit_M3    #保存M3完整校正模型
  
  #打印结果 # apply.coef = exp 将logit系数转化OR
  stargazer(logit_M1, logit_M2, logit_M3, type = "text", ci = TRUE, apply.coef = exp, digits = 3)
  #导出结果
  stargazer(logit_M1, logit_M2, logit_M3, out = paste0("Logistic回归_",y_bin,".csv"), ci = TRUE, apply.coef = exp, digits = 3)
  
  rm(logit_M1,logit_M2,logit_M3)
  gc()
}


# 4.3  添加糖三分组暴露→二分类血脂异常，分层Logistic回归，验证剂量梯度反应关系

for (y in lipid_bin) {
  # 自动生成M1/M2/M3公式
  formula_list <- build_form(outcome = y, expo =  "exposure_3")
  
  # 拟合三个Logistic模型
  m1 <- glm(formula_list$M1, family = binomial, data = df_final)
  m2 <- glm(formula_list$M2, family = binomial, data = df_final)
  m3 <- glm(formula_list$M3, family = binomial, data = df_final)
  
  # 打印分隔，区分不同血脂指标的结果
  cat("\n==================== 结局：", y, " ====================\n")
  # 输出三层模型对比表
  stargazer(m1, m2, m3,
            apply.coef = exp,
            ci = TRUE,
            type = "text",
            digits = 3)
}

#====================5回归模型前提假设检验与模型诊断  ====================

## 5.1 VIF方差膨胀因子：检验多重共线性

library(car)
for(y_bin in lipid_bin){
  cat("\n========== 结局", y_bin, " 方差膨胀因子VIF ==========\n")
  full_mod <- mod_store[[y_bin]] # 直接调用之前保存好的M3模型
  print(vif(full_mod))
}

# 检验所有连续变量是否存在非线性，判断能否直接线性纳入
y_bin <- "low_hdl"
form_2 <- as.formula(paste0(
  y_bin, " ~ DR1TSUGR + I(DR1TSUGR^2) + RIDAGEYR + I(RIDAGEYR^2) + INDFMPIR + I(INDFMPIR^2) + BMXBMI + I(BMXBMI^2) + DR1TKCAL + I(DR1TKCAL^2) + ",
  paste(c("RIAGENDR","RIDRETH1","DMDEDUC2"), collapse = "+")
))
mod_bt <- glm(form_2, family = binomial(), data = df_final)
cat("\n========== low_hdl Box-Tidwell检验（二次项P值判断非线性） ==========\n")
summary(mod_bt)
#仅 BMI 的二次项显著，说明只有 BMI 不满足 Logistic 回归线性前提

## 5.2  HL拟合优度检验：判断模型拟合的效果
library(ResourceSelection)  # HL拟合优度
for(y_bin in lipid_bin){
  cat("\n========== 结局", y_bin, " Hosmer-Lemeshow拟合优度检验 ==========\n")
  # 直接提取之前存储完毕的完整校正M3模型
  full_mod <- mod_store[[y_bin]]
  # HL检验：真实结局y + 模型预测概率，分为10组
  hl_res <- hoslem.test(full_mod$y, fitted(full_mod), g = 10)
  print(hl_res)
}

## 5.3 low_hdl线性模型拟合不足

library(ResourceSelection)
y_bin <- "low_hdl"
# 仅保留BMI二次项，其余变量只用线性项
form_2 <- as.formula(paste0(
  y_bin, " ~ DR1TSUGR + RIDAGEYR + INDFMPIR + BMXBMI + I(BMXBMI^2) + DR1TKCAL + ",
  paste(c("RIAGENDR","RIDRETH1","DMDEDUC2"), collapse = "+")
))
mod_2 <- glm(form_2, family = binomial(), data = df_final)
# 修正后模型再做HL拟合优度检验
hl2 <- hoslem.test(mod_2$y, fitted(mod_2), g = 10)
print(hl2)


# 切换国内清华镜像
options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# 安装pROC
install.packages("pROC")

## 5.4 ROC曲线与AUC值：模型区分度诊断
library(pROC)

# 循环遍历四种血脂异常M3模型
for(y_bin in lipid_bin){
  cat("\n==================== 结局：", y_bin, " ROC-AUC ====================\n")
  full_mod <- mod_store[[y_bin]]
  
  # 构建ROC对象：真实结局 vs 模型预测发病概率
  roc_obj <- roc(
    response = full_mod$y,    # 真实二分类患病结局
    predictor = fitted(full_mod), # Logistic预测概率
    quiet = TRUE # 关闭冗余输出
  )
  
  # 输出AUC数值+95%置信区间
  auc_val <- auc(roc_obj)
  ci_auc <- ci.auc(roc_obj)
  cat("AUC =", auc_val, "，95%CI：", ci_auc[1], "~", ci_auc[3], "\n")
  
  # 绘制ROC曲线（需要出图再打开plot）
  plot(roc_obj,
       main = paste0("ROC曲线：",y_bin,"完全校正M3模型"),
       print.auc = TRUE, # 图上直接标注AUC
       grid = TRUE)
  abline(a=0,b=1,lty=2,col="gray") # 无鉴别能力参考对角线
}
#原始完整 M3 连续糖模型：AUC=0.599

# 根据膳食添加糖三分位数分层，生成分类变量
# 先定义AUC计算函数
get_auc <- function(glm_model){
  y_true <- glm_model$y
  p_pred <- fitted(glm_model)
  p_case <- p_pred[y_true == 1]
  p_ctrl <- p_pred[y_true == 0]
  U_stat <- sum(outer(p_case, p_ctrl, ">")) + 0.5 * sum(outer(p_case, p_ctrl, "=="))
  auc_res <- U_stat / (length(p_case) * length(p_ctrl))
  return(round(auc_res, 3))
}
df_final$sugar_group <- cut(
  df_final$DR1TSUGR,
  breaks = quantile(df_final$DR1TSUGR, probs = c(0, 1/3, 2/3, 1), na.rm = T),
  labels = c("低摄入","中摄入","高摄入"),
  include.lowest = TRUE
)

# 用三分组分类变量构建完整校正模型
mod_group <- glm(
  high_ldl ~ sugar_group + RIDAGEYR + RIAGENDR + RIDRETH1 + DMDEDUC2 + INDFMPIR + BMXBMI + DR1TKCAL,
  family = binomial(), data = df_final
)
auc_group <- get_auc(mod_group)
cat("DR1TSUGR三分组分类模型 AUC =", auc_group, "\n")
#DR1TSUGR 三分组分类完整模型：AUC=0.599




#==================== 6 敏感性分析：剔除添加糖上下1%极值重跑Logistic ====================
library(dplyr)
q1 <- quantile(df_final$DR1TSUGR, probs = 0.01, na.rm = TRUE)
q99 <- quantile(df_final$DR1TSUGR, probs = 0.99, na.rm = TRUE)
df_sens <- filter(df_final, DR1TSUGR >= q1 & DR1TSUGR <= q99)

for(y_bin in lipid_bin){
  cat("\n========== 敏感性分析（剔除1%极值）结局：", y_bin, " ==========\n")
  form_all <- build_form(outcome = y_bin, expo = "DR1TSUGR")
  mod_sens <- glm(form_all$M3, binomial(), data = df_sens)
  stargazer(mod_sens, type = "text", apply.coef = exp, ci = TRUE, digits = 3)
  rm(mod_sens)
}
rm(df_sens)

#未做敏感性分析的M3模型
for(y_bin in lipid_bin){
  cat("\n========== 原始完整样本预存模型：", y_bin, " ==========\n")
  mod_main <- mod_store[[y_bin]]
  stargazer(mod_main, type = "text", apply.coef = exp, ci = TRUE, digits = 3)
}


#==================== 7 交互项检验：添加糖×BMI、添加糖×性别（low_hdl） ====================
# 7.1 糖与BMI交互
#构建交互公式
form_inter_bmi <- as.formula(paste0("low_hdl ~ DR1TSUGR * BMXBMI + ", paste(confounder, collapse = "+")))

#拟合模型
mod_inter_bmi <- glm(form_inter_bmi, binomial(), df_final)

#输出表格
cat("\n========== 添加糖 × BMI交互模型 ==========\n")
stargazer(mod_inter_bmi, type = "text", apply.coef = exp, ci = TRUE, digits = 3)

#7.2 糖与性别交互

form_inter_sex <- as.formula(paste0("low_hdl ~ DR1TSUGR * RIAGENDR + ", paste(confounder, collapse = "+")))
mod_inter_sex <- glm(form_inter_sex, family = binomial(), data = df_final)
cat("\n========== 添加糖 × 性别交互模型 ==========\n")
stargazer(mod_inter_sex, type = "text", apply.coef = exp, ci = TRUE, digits = 3)


#====================8  性别分层亚组Logistic ====================

library(dplyr)

# 分层专用混杂：剔除性别RIAGENDR
confounder_sub <- confounder[confounder != "RIAGENDR"]

# 构建回归公式
full_form_str <- paste0("low_hdl ~ DR1TSUGR + ", paste(confounder_sub, collapse = "+"))
full_form <- as.formula(full_form_str)

# 女性分层建模
df_female <- filter(df_final, RIAGENDR == 2)
mod_f <- glm(full_form, family = binomial(), data = df_female)

# 男性分层建模
df_male <- filter(df_final, RIAGENDR == 1)
mod_m <- glm(full_form, family = binomial(), data = df_male)

# 输出对比表格
cat("\n========== 性别分层完全校正模型对比 ==========\n")
stargazer(mod_f, mod_m, type = "text", apply.coef = exp, ci = TRUE, digits = 3)

# 全部运行完再清理临时对象
rm(df_female, df_male, mod_f, mod_m, confounder_sub, full_form_str, full_form)

#==================== 9 主分析：四类血脂异常完全校正M3模型OR森林图 ====================

install.packages("forestplot")
library(forestplot)

# 存放结局名称、回归输出OR值、95%置信区间上下限
forest_data <- data.frame(
  outcome_name = c("高总胆固醇血症 high_tc",
                   "高甘油三酯血症 high_tg",
                   "低高密度脂蛋白血症 low_hdl",
                   "高低密度脂蛋白血症 high_ldl"),
  OR = c(1.000, 1.001, 1.004, 1.001),
  CI_lower = c(0.998, 0.998, 1.002, 0.998),
  CI_upper = c(1.003, 1.004, 1.005, 1.004)
)

#生成森林图左右两列文字标签
table_text <- cbind(
  forest_data$outcome_name,  #四类血脂异常的名称
  paste0(forest_data$OR, "(", forest_data$CI_lower, "-", forest_data$CI_upper, ")")#拼接OR 值、置信下限、上限
)
#生成数值矩阵，用于画图
mean_ci_matrix <- cbind(forest_data$OR, forest_data$CI_lower, forest_data$CI_upper)

# 生成森林图
#tiff("膳食添加糖_四类血脂OR森林图.tif", width = 1600, height = 800, res = 300)
forestplot(
  labeltext = table_text,      #导入文字标签
  mean = mean_ci_matrix[,1],   #矩阵第1列：OR值
  lower = mean_ci_matrix[,2],  #矩阵第2列：95%CI下限
  upper = mean_ci_matrix[,3],  #矩阵第3列：95%CI上限
  new_page = TRUE,             #新建空白画布
  zero = 1,                    #绘制垂直参考线OR=1（无效应参考值）
  xlog = FALSE,                #横轴直接显示原始OR数值
  boxsize = 0.4,               #控制红色方块大小
  xlab = "比值比 OR (95%置信区间)",
  title = "图1 膳食添加糖与四类血脂异常患病关联（完全校正M3模型）",
  col = forestplot::fpColors(box = "#E64B35", line = "#4DBBD5", zero = "black")
)
#dev.off()

#==================== 10 连续结局线性模型：膳食添加糖与HDL-C线性关联 ====================
# 模型：多元线性回归，仅纳入DR1TSUGR一次项，检验线性剂量反应
# 校正变量：年龄、性别、BMI、总能量摄入
library(ggplot2)
linear_hdl <- lm(LBXHD ~ DR1TSUGR + RIDAGEYR + RIAGENDR + BMXBMI + DR1TKCAL, data = df_final)

# 构造预测网格：混杂变量固定为均值/女性参照组
newdata_linear <- expand.grid(
  DR1TSUGR = seq(min(df_final$DR1TSUGR, na.rm=T), max(df_final$DR1TSUGR, na.rm=T), length.out=100),
  RIDAGEYR = mean(df_final$RIDAGEYR, na.rm=T),
  BMXBMI = mean(df_final$BMXBMI, na.rm=T),
  DR1TKCAL = mean(df_final$DR1TKCAL, na.rm=T),
  RIAGENDR = factor(1)
)

# 预测拟合均值+95%置信区间
pred_lin <- predict(linear_hdl, newdata = newdata_linear, interval = "confidence")
newdata_linear$fit <- pred_lin[,"fit"]
newdata_linear$lwr <- pred_lin[,"lwr"]
newdata_linear$upr <- pred_lin[,"upr"]

# ggplot绘制线性趋势图
ggplot(newdata_linear, aes(x=DR1TSUGR, y=fit)) +
  geom_line(linewidth=1, color="#1F77B4") +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, fill="#1F77B4") +
  labs(x="膳食添加糖摄入量(g)", y="校正后HDL-C浓度(mg/dL)", title="图2 膳食添加糖摄入量与校正后 HDL-C 浓度的线性关联趋势") +
  theme_bw()

#==================== 11 连续结局非线性模型：膳食添加糖与HDL-C自然三次样条关联 ====================
# 模型：自然三次样条ns(df=4)，捕捉非线性拐点，对比线性模型拟合差异
library(splines)
ns_mod <- lm(LBXHD ~ ns(DR1TSUGR, df = 4) + RIDAGEYR + RIAGENDR + BMXBMI + DR1TKCAL, data = df_final)

#生成标准化预测网格数据
newdata_grid <- expand.grid(
  DR1TSUGR = seq(min(df_final$DR1TSUGR, na.rm = T), max(df_final$DR1TSUGR, na.rm = T), length.out = 100),
  RIDAGEYR = mean(df_final$RIDAGEYR, na.rm = T),
  BMXBMI = mean(df_final$BMXBMI, na.rm = T),
  DR1TKCAL = mean(df_final$DR1TKCAL, na.rm = T),
  RIAGENDR = factor(1)
)
#基于模型预测 HDL-C 拟合值与 95% 置信区间
pred_result <- predict(ns_mod, newdata = newdata_grid, interval = "confidence")
newdata_grid$fit <- pred_result[, "fit"] #取出均值
newdata_grid$lwr <- pred_result[, "lwr"] #取出95%CI下限
newdata_grid$upr <- pred_result[, "upr"] ##取出95%CI上限

ggplot(newdata_grid, aes(x = DR1TSUGR, y = fit)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "#2E86AB") +
  labs(x = "膳食添加糖摄入量（g）", y = "校正后HDL-C（mg/dL）", title = "图3 膳食添加糖与HDL-C非线性关联趋势（自然三次样条df=4）") +
  theme_bw()

# -------------------HDL线性vs样条模型拟合对比检验-------------------
# AIC准则对比两种模型拟合效果
cat("==== HDL-C线性/样条模型AIC对比 ====\n")
AIC(linear_hdl, ns_mod)
# 方差分析检验非线性是否存在统计学意义
cat("==== HDL-C线性vs样条方差分析ANOVA ====\n")
anova(linear_hdl, ns_mod)
#==================== 12 二分类结局线性Logistic：膳食添加糖与低HDL患病风险线性剂量反应 ====================
# 模型：传统Logistic回归，logit尺度严格线性，对应恒定OR值
logit_linear <- glm(low_hdl ~ DR1TSUGR + RIDAGEYR + RIAGENDR + BMXBMI + DR1TKCAL,
                    family = binomial(link="logit"), data = df_final)

# 预测网格（混杂固定均值/女性参照）
newdata_logit <- expand.grid(
  DR1TSUGR = seq(min(df_final$DR1TSUGR, na.rm=T), max(df_final$DR1TSUGR, na.rm=T), length.out=100),
  RIDAGEYR = mean(df_final$RIDAGEYR, na.rm=T),
  BMXBMI = mean(df_final$BMXBMI, na.rm=T),
  DR1TKCAL = mean(df_final$DR1TKCAL, na.rm=T),
  RIAGENDR = factor(1)
)

# type="response"输出发病概率；type="link"输出logit线性值用于计算合规CI
pred_prob <- predict(logit_linear, newdata_logit, type = "response", se.fit = TRUE)
newdata_logit$prob <- pred_prob$fit
# 计算概率95%CI（logit尺度算CI再逆转换，避免概率超出0~1区间）
logit_fit <- predict(logit_linear, newdata_logit, type = "link", se.fit = TRUE)
z95 <- 1.96
newdata_logit$logit_lwr <- logit_fit$fit - z95 * logit_fit$se.fit
newdata_logit$logit_upr <- logit_fit$fit + z95 * logit_fit$se.fit
# logit逆转换函数，将logit值转回发病概率
invlogit <- function(x) exp(x)/(1+exp(x))
newdata_logit$prob_lwr <- invlogit(newdata_logit$logit_lwr)
newdata_logit$prob_upr <- invlogit(newdata_logit$logit_upr)

# 绘制发病概率线性趋势图
ggplot(newdata_logit, aes(x=DR1TSUGR, y=prob)) +
  geom_line(linewidth=1, color="#D62728") +
  geom_ribbon(aes(ymin=prob_lwr, ymax=prob_upr), alpha=0.2, fill="#D62728") +
  labs(x="膳食添加糖摄入量(g)", y="校正后低HDL血症发病概率", title="图4 膳食添加糖与低HDL血症患病风险线性关联曲线（Logistic线性模型）") +
  theme_bw()


#==================== 13 二分类结局非线性GAM：膳食添加糖与低HDL患病平滑非线性曲线 ====================
# 模型：惩罚样条GAM，自动选择最优平滑自由度edf，检验非线性是否存在
library(mgcv)
# 拟合二分类logit连接GAM模型，校正协变量
gam_logit <- gam(low_hdl ~ s(DR1TSUGR) + RIDAGEYR + RIAGENDR + BMXBMI + DR1TKCAL,
                 family = binomial(link = "logit"), data = df_final)

# 查看模型结果，重点读取平滑项s(DR1TSUGR)的edf与P值，判断非线性显著性
summary(gam_logit)

# 绘制添加糖平滑剂量反应曲线（内置绘图，logit尺度平滑曲线）
plot(gam_logit, main = "图5 膳食添加糖摄入量与低HDL血症发病风险平滑非线性曲线（GAM惩罚样条）")

# -------------------二分类线性Logistic vs GAM模型AIC对比-------------------
cat("==== low_hdl线性Logistic/GAM模型AIC对比 ====\n")
AIC(logit_linear, gam_logit)

# 清空冗余模型释放内存
rm(list = ls(pattern = "mod_"))
gc()

