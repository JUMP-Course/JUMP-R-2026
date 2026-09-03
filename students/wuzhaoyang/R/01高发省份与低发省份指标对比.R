
# 加载Excel读取包readxl，用于导入china_provincedata.xlsx数据文件
library(readxl)
# 加载ggplot2绘图包，绘制折线对比图
library(ggplot2)
# 加载scales包，用于对坐标轴数值格式化显示
library(scales)
# ---------------------- 全局绘图基础设置 ----------------------
# 注册Windows系统黑体SimHei，解决绘图中文乱码问题
windowsFonts(hei = windowsFont("SimHei"))
# 定义统一基础绘图主题：白底网格+全局文字使用黑体字体
theme_base <- theme_bw() + theme(text = element_text(family = "hei"))

# ---------------------- 1. 读取并预处理原始Excel数据 ----------------------
# 读取同目录下的疫情Excel原始数据
df_raw <- read_excel("china_provincedata.xlsx")

# 将数字格式的日期id（如20200101）转换为标准Date日期格式
df_raw$date <- as.Date(as.character(df_raw$dateId), format = "%Y%m%d")

# 筛选2020年1月1日至2020年7月31日的数据
df_all <- subset(df_raw, date >= "2020-01-01" & date <= "2020-07-31")

# 批量修改数据列名，替换原英文字段为中文
colnames(df_all)[colnames(df_all) == "provinceName"] <- "省份"
colnames(df_all)[colnames(df_all) == "confirmedIncr"] <- "新增确诊"
colnames(df_all)[colnames(df_all) == "confirmedCount"] <- "累计确诊"
colnames(df_all)[colnames(df_all) == "curedCount"] <- "累计治愈"
colnames(df_all)[colnames(df_all) == "deadCount"] <- "累计死亡"

# 定义全国31个省、自治区、完整名单，用于筛选有效省份数据
prov_list <- c(
  "北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
  "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
  "浙江省", "安徽省", "福建省", "江西省", "山东省",
  "河南省", "湖北省", "湖南省", "广东省", "广西壮族自治区",
  "海南省", "重庆市", "四川省", "贵州省", "云南省",
  "西藏自治区", "陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区"
)
# 过滤数据：只保留上面列表内31个省市的记录，剔除无关数据
df_all <- subset(df_all, 省份 %in% prov_list)

# ---------------------- 2. 按累计确诊总量划分高/低发病省份 ----------------------
# 按省份分组，计算每个省份全周期最大累计确诊病例数
prov_max_case <- aggregate(累计确诊 ~ 省份, data = df_all, max)
# 划分高发病省份：全省最大累计确诊≥1000例
high_prov <- prov_max_case$省份[prov_max_case$累计确诊 >= 1000]
# 划分低发病省份：全省最大累计确诊＜1000例
low_prov  <- prov_max_case$省份[prov_max_case$累计确诊 < 1000]

# 在控制台打印输出两类省份名单，方便核对分组结果
cat("高发病省份：", high_prov, "\n")
cat("低发病省份：", low_prov, "\n")

# ---------------------- 3. 封装通用绘图函数 ----------------------
# 参数说明：
# total_data：完整数据集
# target_prov：需要绘图的省份向量（高发/低发）
# var_name：绘图指标名称（新增确诊/累计确诊/累计治愈/累计死亡）
# chart_title：图表标题文字
draw_chart <- function(total_data, target_prov, var_name, chart_title){
  # 筛选出当前分组需要绘图的省份数据
  sub_data <- subset(total_data, 省份 %in% target_prov)
  
  # 构建ggplot绘图对象
  p <- ggplot(sub_data, aes(x = date)) +
    # 绘制折线，x轴为日期，y轴为对应指标，不同省份自动区分颜色，线条粗细0.8（纤细）
    geom_line(aes(y = get(var_name), color = 省份), linewidth = 0.8) +
    # 设置Y轴为对数坐标轴，解决大小数值差距过大、小省份线条重叠看不清的问题，数字正常逗号分隔
    scale_y_log10(labels = comma) +
    # 设置图表标题、横轴、纵轴名称
    labs(title = chart_title, x = "日期", y = var_name) +
    # 套用全局统一绘图主题
    theme_base +
    # 精细化调整图表样式
    theme(
      plot.title = element_text(size = 14, face = "bold"), # 标题字号14、加粗
      axis.text.x = element_text(angle = 45, hjust = 1),   # X轴日期文字旋转45度，防止重叠
      legend.text = element_text(size = 7),                # 图例省份名字号缩小至7
      legend.key.size = unit(0.4, "cm")                    # 图例色块缩小，节省画布空间
    )
  # 弹出绘图窗口展示图表
  print(p)
}

# ===================== 第一组：高发病省份 4张独立对比图 =====================
# 图1：高发省份新增确诊对数对比图
draw_chart(df_all, high_prov, "新增确诊", "高发病省份（累计确诊≥1000）新增确诊对比（对数坐标）")
# 图2：高发省份累计确诊对数对比图
draw_chart(df_all, high_prov, "累计确诊", "高发病省份（累计确诊≥1000）累计确诊对比（对数坐标）")
# 图3：高发省份累计治愈对数对比图
draw_chart(df_all, high_prov, "累计治愈", "高发病省份（累计确诊≥1000）累计治愈对比（对数坐标）")
# 图4：高发省份累计死亡对数对比图
draw_chart(df_all, high_prov, "累计死亡", "高发病省份（累计确诊≥1000）累计死亡对比（对数坐标）")

# ===================== 第二组：低发病省份 4张独立对比图 =====================
# 图5：低发省份新增确诊对数对比图
draw_chart(df_all, low_prov, "新增确诊", "低发病省份（累计确诊<1000）新增确诊对比（对数坐标）")
# 图6：低发省份累计确诊对数对比图
draw_chart(df_all, low_prov, "累计确诊", "低发病省份（累计确诊<1000）累计确诊对比（对数坐标）")
# 图7：低发省份累计治愈对数对比图
draw_chart(df_all, low_prov, "累计治愈", "低发病省份（累计确诊<1000）累计治愈对比（对数坐标）")
# 图8：低发省份累计死亡对数对比图
draw_chart(df_all, low_prov, "累计死亡", "低发病省份（累计确诊<1000）累计死亡对比（对数坐标）")

# 控制台输出提示，代表全部8张图绘制完成
cat("8张图表全部绘制完成\n")