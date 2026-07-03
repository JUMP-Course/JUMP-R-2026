# 加载数据读取包与可视化绘图包
library(readxl)
library(ggplot2)

# 重新读取原始数据集
df <- read_excel("C:/Users/86156/Desktop/糖尿病.xlsx")
# 构建分组标签有序因子，规范绘图时分组展示顺序，固定非糖尿病在前、糖尿病在后
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)
# 绘制糖化血红蛋白加权箱线图，依据抽样权重调整箱线分布
# aes中weight=wt仅为绘图层面加权，用来展示总体人群的变量分布特征
p <- ggplot(
  data = df_valid,
  aes(x = 分组, y = `糖化血红蛋白`, weight = wt)
) +
  geom_boxplot(
    fill = "#73a2ff",       # 箱体填充浅蓝色
    alpha = 0.7,             # 设置填充透明度
    outlier.color = "#c82423", # 异常值点设置为红色，方便识别极端值
    outlier.size = 0.8      # 异常值点大小
  ) +
  labs(x = "研究分组", y = "糖化血红蛋白（国际单位制）") +
  theme_bw() # 采用白底网格学术绘图主题，符合期刊图片规范
print(p) # 在绘图窗口输出图像

# 加载绘图、数据重塑、数据读取工具包
library(ggplot2)
library(tidyr)
library(readxl)

# 读取经过权重处理后的数据集
df <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")

# 定义所有需要可视化的连续型检测指标
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

# 构建有序分组变量，固定图表内分组展示顺序
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)

# 宽格式数据转换为长格式数据，实现多指标批量分面绘图
# ggplot分面绘图使用长格式数据，pivot_longer实现宽表转长表
df_long <- pivot_longer(
  data = df_valid,
  cols = cont_vars,
  names_to = "检测指标",
  values_to = "指标数值"
)

# 批量绘制多连续指标加权箱线图
p <- ggplot(df_long, aes(x = 分组, y = 指标数值, weight = wt, fill = 分组)) +
  geom_boxplot(alpha = 0.7, outlier.color = "#c82423", outlier.size = 0.5) +
  facet_wrap(~检测指标, ncol = 4, scales = "free_y") + # 每行列4张子图，Y轴按各指标范围自由缩放，避免数值跨度大压缩图表
  labs(x = "研究分组", y = "指标测量值", fill = "分组") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # X轴标签旋转45度防止文字重叠,右对齐
    legend.position = "bottom"                         # 图例放置在图表底部，节省绘图区域
  ) +
  scale_y_continuous(labels = scales::label_number()) # 关闭Y轴科学计数法，常规数字展示，符合期刊图表要求
print(p)
# 保存600DPI高清TIFF格式图片，满足论文图片格式要求，高分辨率防止期刊印刷模糊
ggsave("C:/Users/86156/Desktop/箱线图.tiff",
       p, width = 16, height = 12, dpi = 600)

# 加载可视化、数据重塑、Excel读取包
library(ggplot2)
library(tidyr)
library(readxl)

# 读取权重处理后的有效数据集
df_valid <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")

# 定义所有待可视化的分类变量
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

# 将分类变量转换为因子类型，保证分组绘图顺序稳定
# 字符型变量会按字母自动排序，因子可以人为固定分类顺序，图表展示结果可控
for (var in cat_vars) {
  df_valid[[var]] <- factor(df_valid[[var]])
}

# 构建有序分组因子，固定非糖尿病组在前的展示顺序
df_valid$分组 <- factor(
  ifelse(df_valid$`是否确诊糖尿病` == 1, "糖尿病", "非糖尿病"),
  levels = c("非糖尿病", "糖尿病")
)

# 宽表转长表，用于批量绘制多分类变量的分面柱状图
df_long_cat <- pivot_longer(
  data = df_valid,
  cols = all_of(cat_vars), # all_of防止变量名错误时报错
  names_to = "分类指标",
  values_to = "分类水平"
)

# 绘制加权并列柱状图，展示两组各分类指标的加权频数分布
# stat="count"结合weight实现加权频数统计，直观展示总体人群中各分类的分布
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

# 导出高清分类变量加权柱状图
ggsave(
  "C:/Users/86156/Desktop/柱状图.tiff",
  plot = p,
  width = 16,
  height = 12,
  dpi = 600
)