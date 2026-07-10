
# ===================== 加载所需工具包 =====================
# readxl：读取Excel表格文件
library(readxl)
# dplyr：数据筛选、分组、汇总管道处理（主流数据清洗工具）
library(dplyr)
# lubridate：专门处理日期、时间的工具包，简化日期转换操作
library(lubridate)
# ggplot2：专业可视化绘图包，用于绘制各类疫情趋势折线图
library(ggplot2)
# gridExtra：多图拼接辅助包（本代码未使用，预留拓展多子图功能）
library(gridExtra)

# 设置基础绘图设备字体为黑体，解决基础绘图中文乱码
par(family = "SimHei")
# 统一编码格式为UTF-8，避免中文乱码、特殊字符报错
options(encoding = "UTF-8")
# ggplot全局统一主题设置：白底网格模板，全部文字默认黑体，无需每张图重复设置字体
theme_set(theme_bw(base_family = "SimHei"))

# ===================== 1. 数据读取与预处理 =====================
# 读取同目录下的疫情原始Excel文件china_provincedata.xlsx
df <- read_excel("china_provincedata.xlsx")

# lubridate包ymd函数：将数字格式dateId（如20200120）直接转为标准Date日期类型
df$date <- ymd(df$dateId)

# 管道%>%连续处理数据
# filter：筛选2020-01-01至2020-07-31范围内的数据
# arrange(date)：按日期从小到大排序数据
# rownames(df) <- NULL：重置行序号，避免乱序行号干扰后续统计
df <- df %>%
  filter(date >= ymd("2020-01-01") & date <= ymd("2020-07-31")) %>%
  arrange(date)
rownames(df) <- NULL

# ===================== 2. 全国层面数据聚合 =====================
# 按日期分组，每日全国所有省份数据求和，得到全国每日指标
national_df <- df %>%
  group_by(date) %>% # 分组依据：日期
  summarise(
    confirmedIncr = sum(confirmedIncr),   # 当日新增确诊总和
    confirmedCount = sum(confirmedCount), # 累计确诊总和
    deadCount = sum(deadCount),          # 累计死亡总和
    curedCount = sum(curedCount),        # 累计治愈总和
    .groups = "drop" # 分组计算完成后解除分组状态
  )

# 计算疫情日增长率指标
national_df <- national_df %>%
  mutate(
    prev_confirmed = lag(confirmedCount), # lag取前一天累计确诊数值
    daily_growth_rate = confirmedIncr / prev_confirmed # 日增长率=当日新增/前一日累计
  ) %>%
  mutate(daily_growth_rate = ifelse(is.na(daily_growth_rate), 0, daily_growth_rate))
# ifelse判断：首日无前一日数据，增长率填充为0，避免NA缺失

# ===================== 3.1 全国新增病例趋势图 =====================
# 定义绘图对象p1：全国每日新增折线图
p1 <- ggplot(national_df, aes(x = date, y = confirmedIncr)) +
  # 绘制折线：红色，线条粗细1
  geom_line(color = "#C00000", linewidth = 1) +
  # 设置图表标题、XY坐标轴名称
  labs(
    title = "COVID-19疫情早期全国每日新增确诊病例时间趋势",
    x = "日期", y = "新增确诊病例数"
  ) +
  # 调用全局白底主题（已提前theme_set设置字体）
  theme_bw() +
  # 精细化图表样式调整
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5), # 标题加粗、字号12、居中
    axis.text.x = element_text(angle = 45, hjust = 1), # X轴日期文字旋转45度防重叠
    panel.grid.y = element_line(linetype = "dashed", color = "gray80") # Y轴虚线网格
  ) +
  # 隐藏图例
  guides(color = guide_legend(title = "")) +
  # 右上角添加文字标注
  annotate("text", label = "全国当日新增确诊病例", x = Inf, y = Inf, hjust = 1, vjust = 1)

# 弹出画布展示p1图表
print(p1)

# ===================== 3.2 全国累计病例S型曲线 =====================
p2 <- ggplot(national_df, aes(x = date, y = confirmedCount)) +
  # 蓝色累计确诊折线，粗细1
  geom_line(color = "#0070C0", linewidth = 1) +
  labs(
    title = "COVID-19疫情早期全国累计确诊病例时间趋势",
    x = "日期", y = "累计确诊病例数"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5), # 标题居中加粗
    axis.text.x = element_text(angle = 45, hjust = 1), # X轴日期旋转
    panel.grid.y = element_line(linetype = "dashed", color = "gray80") # 横向虚线网格
  )

# 展示累计确诊S曲线图
print(p2)

# ===================== 3.3 重点省份新增对比 =====================
target_provinces <- c("湖北省", "广东省", "浙江省", "河南省", "湖南省")
# 原始数据筛选出目标省份全部记录
province_df <- df %>% filter(provinceName %in% target_provinces)

# 按省份+日期分组，汇总每日新增确诊
province_daily <- province_df %>%
  group_by(provinceName, date) %>%
  summarise(confirmedIncr = sum(confirmedIncr), .groups = "drop")

province_colors <- c(
  "湖北省" = "#C00000",
  "广东省" = "#0070C0",
  "浙江省" = "#00B050",
  "河南省" = "#FFC000",
  "湖南省" = "#9932CC"
)

# 五省新增对比折线图
p4 <- ggplot(province_daily, aes(x = date, y = confirmedIncr, color = provinceName)) +
  geom_line(linewidth = 1) + # 分省份自动区分颜色绘制折线
  scale_color_manual(values = province_colors) + # 绑定自定义配色
  labs(
    title = "重点省份每日新增确诊病例趋势对比",
    x = "日期", y = "新增确诊", color = "" # color="" 清空图例标题
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_line(linetype = "dashed", color = "gray80") # 横竖虚线网格
  )

# 展示省份对比图
print(p4)

# ===================== 3.4 全国日增长率趋势图 =====================
p5 <- ggplot(national_df, aes(x = date, y = daily_growth_rate)) +
  geom_line(color = "#7030A0", linewidth = 1) + # 紫色增长率曲线
  # 武汉封城 2020-01-23
  geom_vline(xintercept = ymd("2020-01-23"), color = "red", linetype = "dashed", alpha = 0.7) +
  # 一级响应 2020-01-25
  geom_vline(xintercept = ymd("2020-01-25"), color = "blue", linetype = "dashed", alpha = 0.7) +
  annotate("text", label = "1.23武汉封城", x = ymd("2020-01-23"), y = max(national_df$daily_growth_rate, na.rm = T),
           label = "1.23武汉封城", color = "red", hjust = -0.1) +
  annotate("text", x = ymd("2020-01-25"), y = max(national_df$daily_growth_rate, na.rm = T)*0.9,
           label = "1.25一级响应", color = "blue", hjust = -0.1) +
  labs(
    title = "全国疫情日增长率变化趋势",
    x = "日期", y = "日增长率"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_line(linetype = "dashed", color = "gray80")
  )

# 展示增长率图表
print(p5)

# ===================== 控制台输出文字结果 =====================
# 打印分隔线提示
cat("\n===== 疫情阶段划分 =====\n")
# 构建疫情四阶段总结表格
summary_df <- data.frame(
  阶段 = c("① 快速增长", "② 达到峰值", "③ 持续下降", "④ 基本控制"),
  时间 = c("2020年1月", "2020年2月", "2020年3月", "2020年4-7月")
)
# 在控制台打印阶段表格
print(summary_df)

# 打印分隔线提示
cat("\n===== 趋势总结 =====\n")
# 再次输出阶段汇总表
print(summary_df)