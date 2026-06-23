# ==============================================
# 1. 加载分析所需的R包
# ==============================================
library(readxl)
library(writexl)
library(survey)
library(dplyr)
library(tidyr)

# ==============================================
# 2. 读取原始数据，定义核心分析列
# ==============================================
# 读取桌面的糖尿病权重原始数据
df_raw <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")
# 定义权重列名
weight_col <- "wt"
group_col <- "是否确诊糖尿病" 

# ==============================================
# 3. 定义分类变量、连续变量
# ==============================================
# 分类变量
cat_vars <- c(
  "性别",
  "种族分类1",
  "受教育程度",
  "婚姻状况",
  "既往饮酒状态",
  "高血压患病史",
  "饮食限制情况",
  "饮食相关补充项1",
  "饮食相关补充项2",
  "饮食相关补充项3",
  "自评总体健康状况",
  "近30天饮酒情况",
  "空腹状态标识",
  "超敏C反应蛋白分级",
  "是否吸烟"
)

# 连续变量
cont_vars <- c(
  "年龄岁",
  "家庭收入贫困比值",
  "体质指数BMI",
  "上臂围cm",
  "腰围cm",
  "臀围cm",
  "收缩压",
  "舒张压",
  "白细胞计数",
  "淋巴细胞百分比",
  "单核细胞百分比",
  "中性粒细胞百分比",
  "嗜酸性粒细胞百分比",
  "嗜碱性粒细胞百分比",
  "淋巴细胞绝对值",
  "单核细胞绝对值",
  "中性粒细胞绝对值",
  "嗜酸性粒细胞绝对值",
  "嗜碱性粒细胞绝对值",
  "红细胞计数",
  "血红蛋白",
  "红细胞压积",
  "平均红细胞体积",
  "平均红细胞血红蛋白含量",
  "红细胞分布宽度",
  "血小板计数",
  "平均血小板体积",
  "有核红细胞",
  "高密度脂蛋白胆固醇国际单位制",
  "超敏C反应蛋白",
  "每日久坐时长",
  "总胆固醇国际单位制",
  "甘油三酯国际单位制",
  "低密度脂蛋白胆固醇国际单位制",
  "计算法低密度脂蛋白胆固醇国际单位制",
  "非高密度脂蛋白胆固醇国际单位制"
)
# 合并所有分析变量
all_vars <- c(cat_vars, cont_vars)

# ==============================================
# 4. 数据清洗：筛选有效分析样本
# ==============================================
df_valid <- df_raw[!is.na(df_raw[[weight_col]]) & !is.na(df_raw[[group_col]]), ]

# ==============================================
# 5. 数据类型转换：分类变量转为因子型
# ==============================================
# 遍历所有分类变量，将其转为R的因子类型
for (var in cat_vars) {
  df_valid[[var]] <- factor(df_valid[[var]])
}

# ==============================================
# 6. 按分组拆分数据集：非糖尿病组、糖尿病组
# ==============================================
df_nondiab <- df_valid[df_valid[[group_col]] == 2, ]
df_diab <- df_valid[df_valid[[group_col]] == 1, ]

# ==============================================
# 7. 构建加权调查设计对象（核心适配抽样权重）
# ==============================================
# 全量样本的加权调查设计：无整群聚类独立个体抽样，权重列公式形式
svy_design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_col)), data = df_valid)
svy_nondiab <- subset(svy_design, df_valid[[group_col]] == 2)
svy_diab <- subset(svy_design, df_valid[[group_col]] == 1)

# ==============================================
# 8. 分类变量的加权描述统计
# ==============================================
# 输入参数：
cat_svy_stat <- function(var_name, svy_obj) {
  # 生成分类变量的加权列联表，统计每个类别的加权频数
  tbl <- svytable(as.formula(paste0("~", var_name)), svy_obj)
  # 计算总加权样本量
  total_w <- sum(tbl)
  # 计算加权百分比，保留2位小数
  pct <- round(tbl / total_w * 100, 2)
  paste0(round(tbl, 0), " (", pct, "%)", collapse = "  ")
}

# ==============================================
# 9. 连续变量的加权描述统计
# ==============================================
# 输入参数：
cont_svy_stat <- function(var_name, svy_obj) {
  # 提取调查设计中的原始数据
  dat_tmp <- svy_obj$variables
  x_vec <- dat_tmp[[var_name]]
  w_vec <- dat_tmp[[weight_col]]
  # 筛选出变量值和权重均无缺失的有效样本
  idx <- !is.na(x_vec) & !is.na(w_vec)
  x_clean <- x_vec[idx]
  w_clean <- w_vec[idx]
  # 统计有效样本量
  n <- length(x_clean)
  if (n < 2) return("样本不足")
  
  # 计算加权均值，把变量名转为公式格式
  mean_res <- svymean(as.formula(paste0("~", var_name)), svy_obj, na.rm = TRUE)
  w_mean <- coef(mean_res)
  # 样本量≤5000时，用Shapiro-Wilk检验正态性
  if (n <= 5000) {
    shap_p <- shapiro.test(x_clean)$p.value
  } else {
    shap_p <- 0
  }
  # 正态分布：计算加权均值±加权标准差
  if (shap_p > 0.05) {
    w_sd <- sqrt(sum(w_clean * (x_clean - w_mean)^2) / sum(w_clean))
    return(paste0(round(w_mean, 2), " ± ", round(w_sd, 2)))
    # 非正态分布：计算加权中位数和四分位数
  } else {
    q_res <- svyquantile(as.formula(paste0("~", var_name)), svy_obj, c(0.25,0.5,0.75), na.rm=TRUE)
    q1 <- q_res[[1]][1]
    med <- q_res[[1]][2]
    q3 <- q_res[[1]][3]
    return(paste0(round(med,2), " (", round(q1,2), "-", round(q3,2), ")"))
  }
}

# ==============================================
# 10. 自定义函数：分类变量P值
# ==============================================
# 输入参数：
get_cat_p <- function(var_name){
  # 提取分析所需的变量、分组、权重列
  sub <- df_valid[, c(var_name, group_col, weight_col)]
  sub <- sub[complete.cases(sub), ]
  # 按变量类别+分组，聚合计算加权频数
  tab <- aggregate(sub[[weight_col]], by=list(sub[[var_name]], sub[[group_col]]), FUN=sum)
  colnames(tab) <- c("lev", "gr", "w")
  # 长表转宽表，适配列联表矩阵格式
  wide <- pivot_wider(tab, names_from=gr, values_from=w, values_fill=0)
  # 去掉第一列，转为列联表矩阵
  mtx <- as.matrix(wide[,-1])
  # 计算行和、列和、总频数
  rs <- rowSums(mtx)
  cs <- colSums(mtx)
  tot <- sum(mtx)
  # 计算每个单元格的期望频数
  exp <- outer(rs, cs)/tot
  # 提取最小期望频数
  min_e <- min(exp)
  
  # 最小期望频数<5：Fisher精确检验
  if(min_e < 5){
    # 加权频数取整，
    mtx_int <- round(mtx)
    fisher_res <- fisher.test(mtx_int)
    return(round(fisher_res$p.value,4))
    # 卡方检验
  }else{
    # 用survey包的加权卡方检验
    chi_res <- try(svychisq(as.formula(paste("~",var_name,"+",group_col)), svy_design), silent=TRUE)
    # 加权卡方检验无异常，输出P值
    if(!inherits(chi_res, "try-error")){
      return(round(chi_res$p.value,4))
      # 手动计算卡方值和P值
    }else{
      chi_val <- sum((mtx-exp)^2 / exp)
      df_chi <- (nrow(mtx)-1)*(ncol(mtx)-1)
      p <- 1-pchisq(chi_val, df_chi)
      return(round(p,4))
    }
  }
}

# ==============================================
# 11. 连续变量组间差异的P值
# ==============================================
# 输入参数
get_con_p <- function(var_name){
  # 提取分析所需的变量、分组、权重列，去除缺失值
  sub <- df_valid[, c(var_name, group_col, weight_col)]
  sub <- sub[complete.cases(sub), ]
  # 统计有效样本量
  n <- nrow(sub)
  if(n < 2) return("样本过少")
  x_clean <- sub[[var_name]]
  
  # 样本量≤5000时，用Shapiro-Wilk检验正态性
  if(length(x_clean) <= 5000){
    shap_p <- shapiro.test(x_clean)$p.value
  }else{
    shap_p <- 0
  }
  
  # 正态分布（P>0.05）：用加权t检验
  if(shap_p > 0.05){
    t_res <- try(svyttest(as.formula(paste(var_name,"~",group_col)), svy_design), silent=TRUE)
    # t检验无异常，输出P值
    if(!inherits(t_res, "try-error")){
      return(round(t_res$p.value,4))
    }
  }
  #  t检验异常：采用加权Wilcoxon秩和检验
  wt_res <- wilcox.test(
    formula(paste(var_name,"~",group_col)),
    data = sub,
    weights = sub[[weight_col]],
    exact = FALSE
  )
  return(round(wt_res$p.value,4))
}

# ==============================================
# 12. 批量计算所有变量统计结果和P值
# ==============================================
# 结果表格长度与所有变量相同
res_nodiab <- character(length(all_vars))  # 非糖尿病组统计结果
res_diab <- character(length(all_vars))    # 糖尿病组统计结果
res_p <- character(length(all_vars))        # 组间差异P值

# 循环所有分析变量
for(i in seq_along(all_vars)){
  v <- all_vars[i]
  # 分类变量
  if(v %in% cat_vars){
    res_nodiab[i] <- cat_svy_stat(v, svy_nondiab)
    res_diab[i] <- cat_svy_stat(v, svy_diab)
    res_p[i] <- get_cat_p(v)
    # 连续变量
  }else{
    res_nodiab[i] <- cont_svy_stat(v, svy_nondiab)
    res_diab[i] <- cont_svy_stat(v, svy_diab)
    res_p[i] <- get_con_p(v)
  }
}

# ==============================================
# 13. 构建表格
# ==============================================
table1 <- data.frame(
  指标 = all_vars,
  非糖尿病组 = res_nodiab,
  糖尿病组 = res_diab,
  P值 = res_p,
  stringsAsFactors = FALSE
)

# ==============================================
# 14. 将结果保存到Excel文件
# ==============================================
save_path <- "C:/Users/86156/Desktop/基线描述.xlsx"
# 将结果数据框写入Excel文件
write_xlsx(table1, save_path)