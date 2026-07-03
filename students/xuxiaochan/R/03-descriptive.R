#描述性统计
# ==============================================
# 1. 加载分析所需的R包
# 本模块加载本次基线特征分析所需的全部功能包，明确各包的核心用途
# ==============================================
library(readxl)    # 用于读取.xlsx格式的Excel原始数据文件
library(writexl)   # 用于将分析结果数据框写入Excel文件，无格式错乱
library(survey)    # 核心包，实现复杂抽样设计下的加权统计描述与组间差异检验
library(dplyr)     # 用于数据筛选、变量提取、数据框整理等基础数据处理
library(tidyr)     # 用于数据长表与宽表的转换，适配列联表统计的格式要求

# ==============================================
# 2. 读取原始数据，定义核心分析列
# 本模块完成原始数据读取，统一定义核心分析列名，避免后续代码硬编码，提升可维护性
# ==============================================
# 从指定文件路径读取Excel格式的原始分析数据，返回数据框格式对象
df_raw <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")
# 定义全局字符对象，存储抽样权重列的列名，后续所有加权分析统一调用
weight_col <- "wt"
# 定义全局字符对象，存储结局分组列的列名，用于后续分组统计与组间差异检验
group_col <- "是否确诊糖尿病" 

# ==============================================
# 3. 定义分类变量、连续变量
# 本模块明确区分分析变量的类型，分类变量为离散分组指标，连续变量为数值型检测指标，用于后续批量循环分析
# ==============================================
# 分类变量：离散型分组指标，涵盖人口学特征、生活行为、既往疾病、实验室分级等分类指标
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

# 连续变量：数值型连续指标，包含人体测量学、血常规、血脂、炎症指标、行为时长等连续性检测指标
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
# 合并分类变量与连续变量，得到全部待分析的指标集合，用于后续批量循环处理
all_vars <- c(cat_vars, cont_vars)

# ==============================================
# 4. 数据清洗：筛选有效分析样本
# 本模块完成核心分析样本的筛选，剔除核心变量缺失的样本，保证后续分析的有效性与稳定性
# ==============================================
# 筛选抽样权重列与结局分组列均无缺失值的观测样本，核心变量缺失会导致加权分析报错或结果偏倚
df_valid <- df_raw[!is.na(df_raw[[weight_col]]) & !is.na(df_raw[[group_col]]), ]

# ==============================================
# 5. 数据类型转换：分类变量转为因子型
# 本模块完成分类变量的数据格式转换，适配R语言与survey包的统计分析要求
# ==============================================
# 遍历所有分类变量，将其数据格式转换为R的因子类型
# 因子类型可被survey包正确识别为分类变量，避免将分类指标误作为连续变量处理，同时自动设置分组参照组
for (var in cat_vars) {
  df_valid[[var]] <- factor(df_valid[[var]])
}

# ==============================================
# 6. 按分组拆分数据集：非糖尿病组、糖尿病组
# 本模块按结局分组拆分有效样本，方便后续分别计算两组的基线描述统计量，实现组间特征对比
# ==============================================
# 拆分非糖尿病组样本：结局分组值为2的观测
df_nondiab <- df_valid[df_valid[[group_col]] == 2, ]
# 拆分糖尿病组样本：结局分组值为1的观测
df_diab <- df_valid[df_valid[[group_col]] == 1, ]

# ==============================================
# 7. 构建加权调查设计对象
# 本模块构建survey包的核心抽样设计对象，绑定抽样权重，适配本次独立个体抽样的研究设计，是所有加权分析的基础
# ==============================================
# 全量样本的加权调查设计：无整群聚类的独立个体抽样，绑定抽样权重列，后续所有加权分析均基于该设计对象
svy_design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_col)), data = df_valid)
# 拆分非糖尿病组的加权调查设计，用于非糖尿病组的加权统计描述
svy_nondiab <- subset(svy_design, df_valid[[group_col]] == 2)
# 拆分糖尿病组的加权调查设计，用于糖尿病组的加权统计描述
svy_diab <- subset(svy_design, df_valid[[group_col]] == 1)

# ==============================================
# 8. 分类变量的加权描述统计
# 本模块定义自定义函数，实现分类变量的加权频数与加权百分比计算，适配抽样权重，输出符合论文规范的统计格式
# ==============================================
# 自定义函数：输入分类变量名、加权调查设计对象，输出"加权频数(加权百分比)"格式的统计结果
cat_svy_stat <- function(var_name, svy_obj) {
  # 生成分类变量的加权列联表，统计每个类别的加权频数，适配抽样设计
  tbl <- svytable(as.formula(paste0("~", var_name)), svy_obj)
  # 计算该变量的总加权样本量，用于后续百分比计算
  total_w <- sum(tbl)
  # 计算每个类别的加权百分比，保留2位小数
  pct <- round(tbl / total_w * 100, 2)
  # 拼接为"频数(百分比)"的规范格式，多个类别用双空格分隔
  paste0(round(tbl, 0), " (", pct, "%)", collapse = "  ")
}

# ==============================================
# 9. 连续变量的加权描述统计
# 本模块定义自定义函数，实现连续变量的加权描述统计，先做正态性检验，再选择适配的统计量，符合统计学规范
# ==============================================
# 自定义函数：输入连续变量名、分组加权调查对象，正态判定依据全样本整体分布
cont_svy_stat <- function(var_name, svy_obj) {
  # 提取全样本调查设计对象中的原始数据框
  dat_full <- svy_design$variables
  # 提取该连续变量的数值向量
  x_full_vec <- dat_full[[var_name]]
  # 提取对应样本的抽样权重向量
  w_full_vec <- dat_full[[weight_col]]
  # 筛选出变量值与权重均无缺失的有效样本，避免缺失值导致统计结果异常
  idx_full <- !is.na(x_full_vec) & !is.na(w_full_vec)
  x_full_clean <- x_full_vec[idx_full]
  # 统计全样本有效样本量
  n_full <- length(x_full_clean)
  # 全样本有效样本量不足2时，直接返回样本不足提示
  if (n_full < 2) return("样本不足")
  
  # 样本量≤5000时，采用Shapiro-Wilk检验进行正态性检验；大样本下正态性检验意义有限，直接设P值为0
  if (n_full <= 5000) {
    shap_p <- shapiro.test(x_full_clean)$p.value
  } else {
    shap_p <- 0
  }
  # 正态分布判断依据：全样本整体分布
  full_normal <- shap_p > 0.05
  
  # ===================== 提取分组的数据，用于计算描述统计 =====================
  # 提取传入分组调查设计对象中的原始数据框
  dat_tmp <- svy_obj$variables
  # 提取该连续变量的数值向量
  x_vec <- dat_tmp[[var_name]]
  # 提取对应样本的抽样权重向量
  w_vec <- dat_tmp[[weight_col]]
  # 筛选出变量值与权重均无缺失的有效样本，避免缺失值导致统计结果异常
  idx <- !is.na(x_vec) & !is.na(w_vec)
  x_clean <- x_vec[idx]
  w_clean <- w_vec[idx]
  # 统计当前分组有效样本量
  n <- length(x_clean)
  # 当前分组有效样本量不足2时，返回样本不足提示，避免统计检验报错
  if (n < 2) return("样本不足")
  
  # 转化为公式格式，得到当前分组的全套统计信息的 survey 结果对象
  mean_res <- svymean(as.formula(paste0("~", var_name)), svy_obj, na.rm = TRUE)
  #提取svymean算出来的加权均值数值,存一个纯数值类型的加权平均值
  w_mean <- coef(mean_res)
  
  # 正态：统一计算加权均值±加权标准差
  if (full_normal) {
    # 计算加权标准差，适配抽样权重
    w_sd <- sqrt(sum(w_clean * (x_clean - w_mean)^2) / sum(w_clean))
    # 拼接为"均值±标准差"的规范格式，保留2位小数
    return(paste0(round(w_mean, 2), " ± ", round(w_sd, 2)))
    # 非正态：统一计算加权中位数与四分位数
  } else {
    # 计算该变量的加权四分位数（25%、50%、75%分位数）
    q_res <- svyquantile(as.formula(paste0("~", var_name)), svy_obj, c(0.25,0.5,0.75), na.rm=TRUE)
    q1 <- q_res[[1]][1]  # 25%分位数
    med <- q_res[[1]][2] # 50%分位数（中位数）
    q3 <- q_res[[1]][3]  # 75%分位数
    # 拼接为"中位数(四分位数)"的格式，保留2位小数
    return(paste0(round(med,2), " (", round(q1,2), "-", round(q3,2), ")"))
  }
}
# ==============================================
# 10. 自定义函数：分类变量组间差异P值
# 本模块定义自定义函数，计算分类变量两组间差异的P值，根据列联表特征选择适配的统计检验方法，适配抽样权重
# ==============================================
# 自定义函数：输入分类变量名，输出组间差异检验的P值，保留4位小数
get_cat_p <- function(var_name){
  # 提取分析所需的变量、分组、权重列，剔除缺失值
  sub <- df_valid[, c(var_name, group_col, weight_col)]
  sub <- sub[complete.cases(sub), ]
  # 按变量类别+结局分组，计算每个单元格的加权频数
  tab <- aggregate(sub[[weight_col]], by=list(sub[[var_name]], sub[[group_col]]), FUN=sum)
  colnames(tab) <- c("lev", "gr", "w")
  # 长表转换为宽表，适配列联表的矩阵格式要求
  wide <- pivot_wider(tab, names_from=gr, values_from=w, values_fill=0)
  # 去掉第一列的列名，转为列联表矩阵
  mtx <- as.matrix(wide[,-1])
  # 计算列联表的行和、列和、总加权频数
  rs <- rowSums(mtx)
  cs <- colSums(mtx)
  tot <- sum(mtx)
  # 计算每个单元格的期望频数，用于判断检验方法的适用性
  exp <- outer(rs, cs)/tot
  # 提取列联表中的最小期望频数
  min_e <- min(exp)
  
  # 最小期望频数<5时，采用Fisher精确检验，适配小样本或稀疏列联表的组间差异检验
  if(min_e < 5){
    # 加权频数取整，适配Fisher精确检验的整数输入要求
    mtx_int <- round(mtx)
    fisher_res <- fisher.test(mtx_int)
    # 返回P值，保留4位小数
    return(round(fisher_res$p.value,4))
    # 最小期望频数≥5时，采用卡方检验，适配大样本列联表的组间差异检验
  }else{
    # 优先调用survey包的加权卡方检验，适配抽样设计与权重
    chi_res <- try(svychisq(as.formula(paste("~",var_name,"+",group_col)), svy_design), silent=TRUE)
    # 加权卡方检验无异常时，返回检验P值
    if(!inherits(chi_res, "try-error")){
      return(round(chi_res$p.value,4))
      # 加权卡方检验异常时，手动计算Pearson卡方值与对应P值，保证结果的稳健性
    }else{
      chi_val <- sum((mtx-exp)^2 / exp)
      # 计算卡方检验的自由度
      df_chi <- (nrow(mtx)-1)*(ncol(mtx)-1)
      # 计算卡方分布对应的P值
      p <- 1-pchisq(chi_val, df_chi)
      return(round(p,4))
    }
  }
}

# ==============================================
# 11. 连续变量组间差异的P值
# 本模块定义自定义函数，计算连续变量两组间差异的P值，根据正态性检验结果选择适配的参数/非参数检验方法
# ==============================================
# 自定义函数：输入连续变量名，输出组间差异检验的P值，保留4位小数
get_con_p <- function(var_name){
  # 提取分析所需的变量、分组、权重列，剔除缺失值
  sub <- df_valid[, c(var_name, group_col, weight_col)]
  sub <- sub[complete.cases(sub), ]
  # 统计有效样本量
  n <- nrow(sub)
  # 有效样本量不足2时，返回样本过少提示，避免检验报错
  if(n < 2) return("样本过少")
  x_clean <- sub[[var_name]]
  
  # 样本量≤5000时，采用Shapiro-Wilk检验进行正态性检验；大样本下直接设P值为0，采用非参数检验
  if(length(x_clean) <= 5000){
    shap_p <- shapiro.test(x_clean)$p.value
  }else{
    shap_p <- 0
  }
  
  # 正态分布（P>0.05）：采用加权t检验，适配抽样设计与权重
  if(shap_p > 0.05){
    t_res <- try(svyttest(as.formula(paste(var_name,"~",group_col)), svy_design), silent=TRUE)
    # 加权t检验无异常时，返回检验P值
    if(!inherits(t_res, "try-error")){
      return(round(t_res$p.value,4))
    }
  }
  # 非正态分布或t检验异常时，采用加权Wilcoxon秩和检验（Mann-Whitney U检验），适配非正态分布的组间差异检验
  wt_res <- wilcox.test(
    formula(paste(var_name,"~",group_col)),
    data = sub,
    weights = sub[[weight_col]],
    exact = FALSE
  )
  # 返回检验P值，保留4位小数
  return(round(wt_res$p.value,4))
}

# ==============================================
# 12. 批量计算所有变量统计结果和P值
# 循环遍历所有分析变量，根据变量类型调用对应的统计函数，批量生成两组的描述统计结果与组间差异P值
# ==============================================
# 初始化结果向量，长度与所有分析变量一致，用于存储对应统计结果
res_nodiab <- character(length(all_vars))  # 非糖尿病组的描述统计结果
res_diab <- character(length(all_vars))    # 糖尿病组的描述统计结果
res_p <- character(length(all_vars))        # 两组间差异检验的P值

# 循环遍历所有分析变量，逐个完成统计计算
for(i in seq_along(all_vars)){
  v <- all_vars[i]
  # 分类变量：调用分类变量统计函数与P值函数
  if(v %in% cat_vars){
    res_nodiab[i] <- cat_svy_stat(v, svy_nondiab)
    res_diab[i] <- cat_svy_stat(v, svy_diab)
    res_p[i] <- get_cat_p(v)
    # 连续变量：调用连续变量统计函数与P值函数
  }else{
    res_nodiab[i] <- cont_svy_stat(v, svy_nondiab)
    res_diab[i] <- cont_svy_stat(v, svy_diab)
    res_p[i] <- get_con_p(v)
  }
}

# ==============================================
# 13. 构建基线特征表
# 将批量计算的统计结果整理为规范的基线特征表
# ==============================================
table1 <- data.frame(
  指标 = all_vars,              # 分析指标名称
  非糖尿病组 = res_nodiab,      # 非糖尿病组的描述统计结果
  糖尿病组 = res_diab,          # 糖尿病组的描述统计结果
  P值 = res_p,                   # 两组间差异检验的P值
  stringsAsFactors = FALSE       # 禁止字符列自动转为因子，保证输出格式稳定
)

# ==============================================
# 14. 将结果保存到Excel文件
# 将整理完成的基线特征表写入Excel
# ==============================================
# 定义结果文件的保存路径
# 在R控制台输出基线描述统计表
print(table1, row.names = FALSE)
save_path <- "C:/Users/86156/Desktop/基线描述.xlsx"
# 将基线特征表数据框写入指定路径的Excel文件
write_xlsx(table1, save_path)