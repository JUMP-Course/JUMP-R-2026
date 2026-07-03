# 加载读取Excel文件的包readxl、写出Excel文件的包writexl
library(readxl)
library(writexl)

df <- read_excel("C:/Users/86156/Desktop/糖尿病.xlsx")

# =====================权重归一化处理=====================
# raw_w：提取两年安全检测权重整列数据
raw_w <- df$`两年安全检测权重`
# 生成归一化权重列wt：原始权重 / 原始权重均值
df$wt <- raw_w / mean(raw_w, na.rm = TRUE) 

# 筛留wt权重不为空、糖尿病分组不为空的行
df_use <- df[!is.na(df$wt) & !is.na(df$`是否确诊糖尿病`), ]

# =====================分类变量加权统计自定义函数=====================
# calc_cat(变量列, 权重列)
calc_cat <- function(var, wt_vec){
  # idx：无缺失的行位置
  idx <- !is.na(var) & !is.na(wt_vec)
  # v：清洗后无缺失的分类变量数值
  v <- var[idx]
  # w：和v一一匹配、无缺失的权重数值
  w <- wt_vec[idx]
  # 权重总和，代表整体人群体量
  total_w <- sum(w)
  # 按分类水平分组，每组内权重求和 = 该水平加权人数
  group_w <- tapply(w, v, sum)
  # 每组加权人数/总权重*100，保留1位小数=加权百分比
  pct <- round(group_w/total_w*100,1)
  paste0(round(group_w,0), " (", pct, "%)", collapse = " ")
}

# =====================连续变量加权统计自定义函数=====================
# calc_con(变量列, 权重列)：正态输出均值±标准差；非正态输出中位数(Q1-Q3)
calc_con <- function(var, wt_vec){
  # 筛选变量、权重均无缺失的行
  idx <- !is.na(var) & !is.na(wt_vec)
  x <- var[idx] # 清洗后连续变量
  w <- wt_vec[idx] # 匹配权重
  # length(x)<2 有效样本不足2个，无法计算统计量，返回横线占位
  if(length(x)<2) return("-")
  # shapiro.test，p>0.05认为符合正态分布
  if(shapiro.test(x)$p>0.05){
    # wm：加权均值公式 ∑(数值×权重) / ∑权重
    wm <- sum(x*w)/sum(w)
    # ws：加权标准差简化计算公式
    ws <- sqrt(sum(w*(x-wm)^2)/sum(w))
    # 保留2位小数拼接：均值 ± 标准差
    return(paste0(round(wm,2)," ± ",round(ws,2)))
  }else{
    # 非正态分布，计算加权四分位数
    ord <- order(x) # 对连续数值从小到大排序，返回排序下标
    x_ord <- x[ord] # 排序后的数值
    w_ord <- w[ord] # 对应排序后的权重
    cumw <- cumsum(w_ord) # 累加权重，得到累计权重向量
    total <- sum(w) # 总权重
    # which.min(abs(cumw - total*0.25))：找到累计权重最接近总权重25%的位置=Q1
    q1_val <- x_ord[which.min(abs(cumw - total*0.25))]
    med_val <- x_ord[which.min(abs(cumw - total*0.5))] # 累计50%=加权中位数
    q3_val <- x_ord[which.min(abs(cumw - total*0.75))] # 累计75%=Q3
    # 拼接格式：中位数 (Q1-Q3)
    return(paste0(round(med_val,2)," (",round(q1_val,2),"-",round(q3_val,2),")"))
  }
}

# =====================按糖尿病分组拆分样本=====================
# g1：提取分组等于1的行
g1 <- df_use[df_use$`是否确诊糖尿病`==1,]
# g2：提取分组等于2的行
g2 <- df_use[df_use$`是否确诊糖尿病`==2,]

# 将新增归一化wt权重列的完整数据集df，写出到桌面Excel文件：糖尿病权重.xlsx
write_xlsx(df, "C:/Users/86156/Desktop/糖尿病权重.xlsx") 
# 加载读取Excel数据包、绘图数据包
library(readxl)
library(ggplot2)

# 读取桌面文件，数据存入df
df <- read_excel("C:/Users/86156/Desktop/糖尿病.xlsx")
# 新建分组标签列，转为有序因子
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)
p <- ggplot(
  data = df_valid,
  # ：x横轴=分组；y纵轴=糖化血红蛋白；weight=wt 加权计算分位数
  aes(x = 分组, y = `糖化血红蛋白`, weight = wt)
) +
  geom_boxplot(
    fill = "#73a2ff",       
    alpha = 0.7,             
    outlier.color = "#c82423",
    outlier.size = 0.8      
  ) +
  labs(x = "研究分组", y = "糖化血红蛋白（国际单位制）") +
  # 用白底网格
  theme_bw()
print(p)

library(ggplot2)
library(tidyr)
df <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")
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

# 分组
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)

# 将连续变量数据集进行宽格式转长格式，方便ggplot批量绘制多指标箱线图
df_long <- pivot_longer(
  data = df_valid,         # 输入清洗后的有效数据集
  cols = cont_vars,        # 指定需要转换的所有连续型变量
  names_to = "检测指标",   # 原数据的列名存入检测指标
  values_to = "指标数值"   # 原数据对应的数值存入指标数值
)

# 绘制加权箱线图，基于抽样权重wt进行加权展示多组连续指标分布
p <- ggplot(df_long, aes(x = 分组, y = 指标数值, weight = wt, fill = 分组)) +
  # 绘制箱线图，透明度0.7，异常值设置为红色、点大小0.5
  geom_boxplot(alpha = 0.7, outlier.color = "#c82423", outlier.size = 0.5) +
  # 按照检测指标分面绘图，每行4个子图，Y轴根据各指标范围自由缩放
  facet_wrap(~检测指标, ncol = 4, scales = "free_y") +
  # 设置坐标轴、图例标题
  labs(x = "研究分组", y = "指标测量值", fill = "分组") +
  # 使用白底经典绘图主题
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # X轴标签旋转45度防止文字重叠
    legend.position = "bottom"                         # 图例放置在图表底部
  )
print(p)

ggsave("C:/Users/86156/Desktop/箱线图.tiff",
       p, width = 16, height = 12, dpi = 600)
library(ggplot2)
library(tidyr)
library(readxl)

# 读取数据
df_valid <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")

# 定义分类变量
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

# 分类变量转为因子，固定分类顺序
for (var in cat_vars) {
  df_valid[[var]] <- factor(df_valid[[var]])
}

# 设置分组，固定顺序：非糖尿病在前
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)

# 宽格式数据集转换为长格式数据集，用于批量绘制多个分类变量的柱状图
df_long_cat <- pivot_longer(
  data = df_valid,         # 输入清洗完成的分析数据集
  cols = all_of(cat_vars), # 选取预先定义好的全部分类变量，all_of可避免变量名不存在时报错
  names_to = "分类指标",   # 将原始数据的列名保存到分类指标
  values_to = "分类水平"   # 原始数据中每个分类变量对应的类别取值存入分类水平
)

# 加权并列柱状图，实现加权频数统计
p <- ggplot(df_long_cat, aes(x = 分类水平, weight = wt, fill = 分组)) +
  geom_col(stat = "count", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
  facet_wrap(~分类指标, ncol = 4, scales = "free") +
  labs(x = "分类水平", y = "加权频数", fill = "研究分组") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom"
  )

print(p)

# 保存高清图
ggsave(
  "C:/Users/86156/Desktop/柱状图.tiff",
  plot = p,
  width = 16,
  height = 12,
  dpi = 600
)