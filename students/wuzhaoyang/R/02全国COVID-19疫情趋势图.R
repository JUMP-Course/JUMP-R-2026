install.packages("ggpubr")
# ===================== 加载包 =====================
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)
library(ggpubr)

# 设置中文显示（Windows 用 SimHei，Mac 用 "PingFang SC"，Linux 用 "WenQuanYi Micro Hei"）
options(encoding = "UTF-8")
par(family = "SimHei")

# ===================== 1. 读取并预处理数据 =====================
df_raw <- read_excel("china_provincedata.xlsx")
df_raw$date <- ymd(df_raw$dateId)  # 日期转换

# 筛选 2020-01-01 ~ 2020-07-31
df <- df_raw %>%
  filter(date >= ymd("2020-01-01") & date <= ymd("2020-07-31"))

# 按日期聚合全国数据
national_df <- df %>%
  group_by(date) %>%
  summarise(
    confirmedIncr = sum(confirmedIncr),   # 新增确诊
    confirmedCount = sum(confirmedCount), # 累计确诊
    curedCount = sum(curedCount),         # 累计治愈
    deadCount = sum(deadCount),           # 累计死亡
    .groups = "drop"
  )

# 重命名列名
colnames(national_df) <- c("date", "新增确诊", "累计确诊", "累计治愈", "累计死亡")

# ===================== 2. 绘制双Y轴趋势图 =====================
# 1) 先画左Y轴（累计类）
p <- ggplot() +
  geom_line(data = national_df, aes(x = date, y = `累计确诊`, color = "累计确诊"), linewidth = 1.2) +
  geom_line(data = national_df, aes(x = date, y = `累计治愈`, color = "累计治愈"), linewidth = 1.2) +
  geom_line(data = national_df, aes(x = date, y = `累计死亡`, color = "累计死亡"), linewidth = 1.2) +
  scale_color_manual(
    name = "",
    values = c(
      "累计确诊" = "#0070C0",
      "累计治愈" = "#00B050",
      "累计死亡" = "#C00000"
    )
  ) +
  labs(
    title = "全国COVID-19疫情新增/累计确诊/治愈/死亡趋势图",
    x = "日期",
    y = "累计病例数"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# 2) 新增右Y轴（新增确诊）
# 计算左右Y轴的缩放比例（让新增确诊的轴和累计轴在视觉上对齐）
max_cum <- max(national_df$`累计确诊`, na.rm = TRUE)
max_new <- max(national_df$`新增确诊`, na.rm = TRUE)
scale_factor <- max_cum / max_new

# 把新增确诊数据按比例缩放，画在同一图上，再用 sec_axis 还原刻度
p <- p +
  geom_line(
    data = national_df,
    aes(x = date, y = `新增确诊` * scale_factor, color = "新增确诊"),
    linewidth = 1.2
  ) +
  scale_y_continuous(
    name = "累计病例数",
    sec.axis = sec_axis(~ . / scale_factor, name = "新增确诊病例")
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "累计确诊" = "#0070C0",
      "累计治愈" = "#00B050",
      "累计死亡" = "#C00000",
      "新增确诊" = "#FF0000"
    )
  ) +
  theme(
    axis.title.y.right = element_text(color = "#FF0000"),
    axis.text.y.right = element_text(color = "#FF0000")
  )

# 显示图表
print(p)

cat("图表已直接输出显示\n")
