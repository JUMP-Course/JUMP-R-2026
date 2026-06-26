#描述性统计

#基线特征表核心包
library(tableone) 
library(dplyr)      # 数据汇总、趋势图计算

#变量分组+变量类型转换

#连续变量：
con_vars <- c("RIDAGEYR", "BMXBMI", "DR1TSUGR", "DR1TKCAL", "DR1TKCAL_winsor",
              "LBXTC", "LBXTR", "LBXHD", "LBDLDL", "INDFMPIR")

#分类变量
cat_vars <- c("RIAGENDR", "RIDRETH1", "DMDEDUC2","exposure_3", "high_tc", 
              "high_tg", "low_hdl", "high_ldl")

# 定义偏态连续变量
nonnormal_vars <- c("BMXBMI", "DR1TSUGR", "DR1TKCAL", "DR1TKCAL_winsor",
                    "LBXTC", "LBXTR", "LBXHD", "LBDLDL", "INDFMPIR")
# 分类变量转因子
df_final[cat_vars] <- lapply(df_final[cat_vars], factor)


#查看分类变量的分布
cat("\n分类变量频数分布\n")   

for(v in cat_vars){
  cat("变量：", v, "\n")
  print(table(df_final[[v]]))
  cat("\n")
}

# 查看连续变量分布
lapply(df_final[, con_vars], summary) #对每一个连续变量，分别执行 summary()

#直接查看 summary(df_final[, con_vars]) 


#基于tableone包生成基线表
#总体基线特征表
cat("\n总体基线特征表\n")
tab_total <- CreateTableOne(  
  vars = c(con_vars, cat_vars),   #指定纳入的变量
  data = df_final,
  factorVars = cat_vars           #标明分类变量
)
print(tab_total, 
      showAllLevels = TRUE,       #展示分类变量的所有分组
      nonnormal = nonnormal_vars, #指定偏态分布的变量
      pDigits = 3                 #保留三位小数
)


#分组基线特征表
cat("\n按添加糖分组基线特征表\n")
tab_group <- CreateTableOne(
  vars = c(con_vars, cat_vars),       
  strata = "exposure_3",         #strata表示分组，是以exposure3为分组标准
  data = df_final,
  factorVars = cat_vars)
print(tab_group, showAllLevels = TRUE, nonnormal = nonnormal_vars, pDigits = 3,
      test = TRUE)

#导出csv格式
# 1. 导出总体表
tab_total_export <- print(tab_total,showAllLevels = TRUE,nonnormal = nonnormal_vars,
                          pDigits = 3,printToggle = FALSE)

write.csv(
  tab_total_export, 
  file="总体描述统计表.csv",             #保存的文件名
  row.names = TRUE,                      #写入列名、变量名
)

# 2. 导出分组表（含P值）
tab_group_export <- print(tab_group,showAllLevels = TRUE,nonnormal = nonnormal_vars,
                          pDigits = 3,test = TRUE,printToggle = FALSE)

write.csv(
  tab_group_export, 
  file="分组描述统计表.csv",       #保存的文件名
  row.names = TRUE,                #写入列名
)


#安装绘图包和拼图包
install.packages("ggplot2")
install.packages("patchwork")
# 加载包
library(ggplot2)

# 分类变量转换为因子型
df_final$exposure_3 <- factor(df_final$exposure_3)

#绘制DR1TSUGR（单日总添加糖）的分布图
ggplot(data = df_final, aes(x = DR1TSUGR, y = ..density..)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6, color = "black") +
  geom_density(color = "red", size = 1)+
  labs(title = "膳食添加糖摄入量分布", x = "DR1TSUGR (g)", y = "密度")


#箱线图
# 01 绘制总添加糖摄入量三组箱线图
ggplot(data = df_final, aes(x = exposure_3, y = DR1TSUGR)) +
  # 图形:箱线图，配色+透明度
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  # 坐标轴文字、标题
  labs(
    x = "膳食添加糖摄入分组",
    y = "总添加糖摄入量",
    title = "不同添加糖摄入组总添加糖摄入量对比"
  ) +
  # 白色背景
  theme_bw() 


# 02 HDL-C
p_hdl <- ggplot(data = df_final, aes(x = exposure_3, y = LBXHD)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(
    y = "HDL-C (mg/dL)",
    title = "高密度脂蛋白胆固醇"
  ) +
  theme_bw()

# 03 甘油三酯
p_tg <- ggplot(data = df_final, aes(x = exposure_3, y = LBXTR)) +
  geom_boxplot(fill = "red", alpha = 0.7) +
  labs(
    y = "TG（mg/dL）",
    title = "甘油三酯"
  ) +
  theme_bw()

# 04 LDL-C
p_ldl <- ggplot(data = df_final, aes(x = exposure_3, y = LBDLDL)) +
  geom_boxplot(fill = "brown", alpha = 0.7) +
  labs(
    y = "LDL-C (mg/dL)",
    title = "低密度脂蛋白胆固醇"
  ) +
  theme_bw()

# 05 总胆固醇 TC
p_tc <- ggplot(data = df_final, aes(x = exposure_3, y = LBXTC)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(
    y = "TC (mg/dL)",
    title = "总胆固醇"
  ) +
  theme_bw()


library(patchwork)
# 2行2列拼接
all_lipid <- (p_hdl + p_tg) / (p_ldl + p_tc)

# 添加总图标题 + 全局统一底部横轴
all_lipid + plot_annotation(
  title = "不同膳食添加糖摄入组血脂指标分布对比",
  caption = "膳食添加糖摄入分组", # 全局横轴文字
) & theme(
  plot.caption = element_text(hjust = 0.5, size = 13), # 居中放大横轴文字
)


#趋势图
# 按分组变量分组，计算每组的均值和95%置信区间
#01 HDL
#  分组计算HDL统计量，生成汇总表
df_group <- group_by(df_final, exposure_3) #分组

df_trend_hdl <- summarise(
  df_group,
  mean_hdl = mean(LBXHD, na.rm = TRUE),    #计算均值
  se_hdl = sd(LBXHD, na.rm = TRUE)/sqrt(n()),  #计算标准误 se=sd/sqrt(n)
  lower = mean_hdl - 1.96*se_hdl,     #置信区间下限
  upper = mean_hdl + 1.96*se_hdl      #置信区间上限
)

# HDL趋势图
ggplot(data = df_trend_hdl, aes(x = exposure_3, y = mean_hdl, group = 1)) +
  geom_point(size = 3, color = "darkblue") +     #绘制散点
  geom_line(color = "darkblue", linewidth = 1) +     #绘制折线
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +   #绘制误差线
  labs(
    title = "膳食添加糖摄入与高密度脂蛋白胆固醇的趋势关系",
    x = "膳食添加糖摄入分组",
    y = "高密度脂蛋白胆固醇（mg/dL）"
  ) +
  theme_bw() 

#02 LDL
# 计算LDL均值与95%置信区间
df_group <- group_by(df_final, exposure_3)
df_trend_ldl <- summarise(
  df_group,
  mean_ldl = mean(LBDLDL, na.rm = TRUE),
  se_ldl = sd(LBDLDL, na.rm = TRUE)/sqrt(n()),
  lower = mean_ldl - 1.96 * se_ldl,
  upper = mean_ldl + 1.96 * se_ldl
)

# LDL趋势图
ggplot(data = df_trend_ldl, aes(x = exposure_3, y = mean_ldl, group = 1)) +
  geom_point(size = 3, color = "purple") +
  geom_line(color = "purple", linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "膳食添加糖摄入与低密度脂蛋白胆固醇的趋势关系",
    x = "膳食添加糖摄入分组",
    y = "低密度脂蛋白胆固醇（mg/dL）"
  ) +
  theme_bw() 


#相关性散点图
# 膳食添加糖与高密度脂蛋白胆固醇相关性散点图
# 图1：添加糖 vs HDL-C
p_scatter_hdl <- ggplot(data = df_final, aes(x = DR1TSUGR, y = LBXHD)) +
  geom_point(alpha = 0.2, color = "gray50") +
  geom_smooth(method = "lm",
              color = "steelblue", linewidth = 1, se = TRUE) +
  labs(
    title = "膳食添加糖摄入量与HDL-C相关性",
    x = "膳食添加糖摄入量（g）",
    y = "HDL-C（mg/dL）"
  ) +
  theme_bw() + theme(
    # 缩小子图标题字号
    plot.title = element_text(size = 11, margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.x = element_blank()
  )

# 图2：添加糖 vs LDL-C
p_scatter_ldl <- ggplot(data = df_final, aes(x = DR1TSUGR, y = LBDLDL)) +
  geom_point(alpha = 0.2, color = "gray50") +
  geom_smooth(method = "lm", color = "red", linewidth = 1, se = TRUE) +
  labs(
    title = "膳食添加糖摄入量与LDL-C相关性",
    x = "膳食添加糖摄入量（g）",
    y = "LDL-C（mg/dL）"
  ) +
  theme_bw() + theme(
    # 缩小子图标题字号
    plot.title = element_text(size = 11, margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.x = element_blank()
  )
scatter_all <- p_scatter_hdl + p_scatter_ldl
scatter_all + plot_annotation(
  title = "膳食添加糖摄入量与血脂指标线性相关可视化",
  caption = "膳食添加糖摄入量（g）"
) & theme(plot.caption = element_text(hjust = 0.5, size = 12))

