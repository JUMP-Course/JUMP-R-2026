
#单因素、多因素分析
# ===================== 第一步：安装并加载所需R包 =====================
# 首次运行取消下面注释安装所有依赖包，后续运行可注释该行
# install.packages(c("readxl","survey","dplyr","writexl","forestploter","grid","ggplot2","car"))
# readxl：用于读取.xlsx格式的Excel文件
library(readxl)
# survey：实现复杂抽样设计下的加权统计建模，支持加权Logistic回归分析
library(survey)
# dplyr：提供数据筛选、变量新增、数据拼接、数据汇总等数据整理功能
library(dplyr)
# writexl：将数据框对象快速导出为无格式错乱的Excel文件，用于结果存档与论文制表
library(writexl)
# forestploter：专门用于回归效应量可视化，绘制置信区间森林图，适配流行病学统计图表规范
library(forestploter)
# grid：R底层绘图工具包，用于自定义森林图主题、线条、布局等绘图细节设置
library(grid)
# ggplot2：主流可视化工具包，依托ggsave函数实现高分辨率图片导出
library(ggplot2)
# car：提供多重共线性诊断函数VIF，用于自变量间共线性问题检验
library(car)

# ===================== 第二步：数据读取、结局变量预处理 =====================
# 从指定文件路径读取Excel格式数据集，返回数据框格式对象
df <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")

# 二元Logistic回归模型要求因变量为0、1二分类格式
# 通过条件判断函数将原始二分类结局变量重新编码：患病赋值为1，未患病赋值为0
df$y <- ifelse(df$`是否确诊糖尿病` == 1, 1, 0)

# 定义全局字符对象存储抽样权重列名，后续所有加权分析统一调用该对象，便于路径批量修改维护
weight_col <- "wt"

# ===================== 第三步：定义自变量：分类变量+连续变量 =====================
# 构建字符向量存储所有分类自变量名称，涵盖人口学特征、生活行为、既往疾病、实验室分级指标等分类危险因素
cat_vars <- c(
  "性别","种族分类1","受教育程度","婚姻状况","既往饮酒状态",
  "高血压患病史","饮食限制情况","饮食相关补充项1","饮食相关补充项2",
  "饮食相关补充项3","自评总体健康状况","近30天饮酒情况","空腹状态标识",
  "超敏C反应蛋白分级","是否吸烟"
)

# 构建字符向量存储所有连续自变量名称，包含人体测量学指标、血常规、血脂、炎症指标、行为时长等连续性检测变量
cont_vars <- c(
  "年龄岁","家庭收入贫困比值","体质指数BMI","上臂围cm","腰围cm","臀围cm",
  "收缩压","舒张压","白细胞计数","淋巴细胞百分比","单核细胞百分比",
  "中性粒细胞百分比","嗜酸性粒细胞百分比","嗜碱性粒细胞百分比",
  "淋巴细胞绝对值","单核细胞绝对值","中性粒细胞绝对值","嗜酸性粒细胞绝对值",
  "嗜碱性粒细胞绝对值","红细胞计数","血红蛋白","红细胞压积","平均红细胞体积",
  "平均红细胞血红蛋白含量","红细胞分布宽度","血小板计数","平均血小板体积",
  "有核红细胞","高密度脂蛋白胆固醇国际单位制","超敏C反应蛋白","每日久坐时长",
  "总胆固醇国际单位制","甘油三酯国际单位制","低密度脂蛋白胆固醇国际单位制",
  "计算法低密度脂蛋白胆固醇国际单位制","非高密度脂蛋白胆固醇国际单位制"
)

# 拼接分类自变量与连续自变量向量，得到全部待纳入单因素回归分析的自变量集合，用于循环批量建模
all_vars <- c(cat_vars, cont_vars)

# ===================== 第四步：分类变量转换为因子类型 =====================
# 循环遍历所有分类变量，将变量数据格式转换为因子类型
# 因子格式可被抽样回归函数识别为分类变量，模型会自动将第一个水平设置为参照组，其余水平与参照组进行效应比较
for (v in cat_vars) {
  df[[v]] <- factor(df[[v]])
}

# ===================== 第五步：数据清洗 + 构建抽样加权设计对象 =====================
# 筛选抽样权重与结局变量均无缺失值的观测样本，剔除缺失样本避免回归建模过程中出现缺失值报错
df_valid <- df[!is.na(df[[weight_col]]) & !is.na(df$y), ]

# 构建复杂抽样调查设计对象，用于后续所有加权统计分析
df_valid <- df[
  !is.na(df[[weight_col]]) & 
    !is.na(df[["y"]]) &
    !is.na(df[["抽样层"]]) &
    !is.na(df[["初级抽样单元"]]),
]

# ===================== 第六步：单因素加权Logistic回归） =====================
# 初始化空数据框，用于循环存储所有自变量单因素加权Logistic回归的统计结果
uni_result <- data.frame(
  原始变量名 = character(),   # 记录自变量原始列名称
  变量详情 = character(),    # 记录分类变量各分组水平名称，连续变量仅输出变量主名称
  OR = numeric(),            # 存储指数化后的比值比效应值
  CI95_L = numeric(),        # 存储95%置信区间下限
  CI95_U = numeric(),        # 存储95%置信区间上限
  P数值 = numeric()
)

# 循环遍历全部自变量，逐个构建单因素加权二元Logistic回归模型，完成危险因素初筛
for (var in all_vars) {
  # 将字符格式的回归变量拼接为模型公式，构建单变量回归表达式：因变量~单个自变量
  formula_uni <- as.formula(paste("y ~", var))
  # 基于抽样设计对象构建加权广义线性模型，采用quasibinomial分布适配复杂抽样下的二分类结局回归
  fit <- svyglm(formula_uni, design = svy_des, family = quasibinomial())
  
  # 提取模型回归系数汇总表，包含回归系数、标准误、Z值、P值
  coef_tab <- coef(summary(fit))
  # 计算模型回归系数的95%置信区间
  ci_tab <- confint(fit)
  
  # 剔除模型截距项对应的结果行，仅保留自变量各水平的效应估计结果
  rows <- rownames(coef_tab)[-1]
  # 若自变量无有效效应结果行则终止当前循环，进入下一个自变量建模
  if (length(rows) == 0) next
  
  # 对回归系数进行指数转换得到OR值，效应值与置信区间保留3位小数，P值保留4位小数，构建单变量结果临时数据集
  temp <- data.frame(
    原始变量名 = var,
    变量详情 = rows,
    OR = round(exp(coef_tab[-1, 1]), 3),
    CI95_L = round(exp(ci_tab[-1, 1]), 3),
    CI95_U = round(exp(ci_tab[-1, 2]), 3),
    P数值 = format(round(coef_tab[-1, 4], 4), scientific = FALSE, digits = 4)
  )
  # 将当前自变量回归结果追加存储至总结果数据集
  uni_result <- rbind(uni_result, temp)
}
uni_result <- uni_result %>%
  mutate(
    P值 = case_when(
      P数值 < 0.001 ~ paste0(format(P数值, digits = 4, scientific = FALSE), "***"),
      P数值 < 0.01  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "**"),
      P数值 < 0.05  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "*"),
      TRUE ~ format(P数值, digits = 4, scientific = FALSE)
    )
  ) %>%
  select(-P数值)

# 控制台打印单因素Logistic回归全部结果
cat("====================单因素加权Logistic回归结果====================\n")
print(uni_result, row.names = FALSE)

# 将全部单因素回归统计结果导出至指定路径的Excel文件，用于论文表格整理、结果存档
write_xlsx(uni_result, "C:/Users/86156/Desktop/单因素Logistic回归结果.xlsx")

# ===================== 第七步：多因素加权Logistic回归 =====================
# 筛选单因素分析中P值小于0.05、具备统计学意义的自变量，作为多因素模型待纳入候选变量
sig_uni <- uni_result[uni_result$P值 < 0.05, ]
# 对显著自变量名称去重，获取多因素回归候选变量集合
sig_vars <- unique(sig_uni$原始变量名)

# 初始化多因素回归结果存储对象
multi_result <- NULL
# 定义需要强制纳入多因素模型的混杂变量集合，用于校正混杂偏倚
confounder_vars <- c("年龄岁", "性别", "种族分类1", "受教育程度")

# 仅当存在统计学显著的候选自变量时，执行多因素回归建模流程
if (length(sig_vars) > 0) {
  # 合并混杂变量与单因素显著自变量并去重，确定最终纳入多因素模型的全部自变量
  model_vars <- unique(c(confounder_vars, sig_vars))
  
  # -------------- VIF多重共线性检验 --------------
  # 提取结局变量与待纳入多因素模型的所有自变量，构建共线性检验数据集
  df_vif <- df_valid[, c("y", model_vars)]
  # 删除检验数据集中存在缺失值的观测样本，避免线性模型拟合报错
  df_vif <- na.omit(df_vif)
  # 构建线性回归公式，用于计算方差膨胀因子VIF
  vif_form <- as.formula(paste("y ~", paste(model_vars, collapse = "+")))
  # 拟合普通最小二乘线性模型，为VIF检验提供基础模型对象
  vif_lm <- lm(vif_form, data = df_vif)
  # 调用car包的 vif 函数计算所有自变量的方差膨胀因子，用于判断自变量间多重共线性程度
  vif_result <- car::vif(vif_lm)
  
  # 在控制台打印各变量VIF检验结果，用于结果查看与共线性程度判断
  cat("========== 各变量VIF多重共线性检验结果 ==========\n")
  print(vif_result)
  # 将VIF检验结果导出为Excel文件，实现检验结果存档
  write_xlsx(as.data.frame(vif_result), "C:/Users/86156/Desktop/VIF多重共线性检验结果.xlsx")
  
  # -------------- 构建多因素回归公式 --------------
  # 拼接所有待纳入变量，构建多因素加权Logistic回归模型公式  
  multi_form <- as.formula(paste("y ~", paste(model_vars, collapse = "+")))
  # 基于抽样设计对象拟合校正混杂因素后的多因素加权广义线性模型，采用quasibinomial分布处理二分类结局
  multi_fit <- svyglm(multi_form, design = svy_des, family = quasibinomial())
  
  # 提取多因素模型的回归系数、标准误、检验统计量及P值汇总结果
  multi_coef <- coef(summary(multi_fit))
  # 计算多因素模型各变量回归系数的95%置信区间
  multi_ci <- confint(multi_fit)
  
  # 将回归系数指数化转换为校正OR值，规范结果小数位数，整理多因素回归结果数据集
  multi_result <- data.frame(
    变量 = rownames(multi_coef),
    OR = round(exp(multi_coef[, 1]), 3),
    CI95_L = round(exp(multi_ci[, 1]), 3),
    CI95_U = round(exp(multi_ci[, 2]), 3),
    P数值 = format(round(multi_coef[, 4], 4), scientific = FALSE, digits = 4)
  )
  multi_result <- multi_result %>%
    mutate(
      P值 = case_when(
        P数值 < 0.001 ~ paste0(format(P数值, digits = 4, scientific = FALSE), "***"),
        P数值 < 0.01  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "**"),
        P数值 < 0.05  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "*"),
        TRUE ~ format(P数值, digits = 4, scientific = FALSE)
      )
    ) %>%
    select(-P数值)
  
  # 控制台打印多因素加权Logistic回归结果
  cat("====================多因素加权Logistic回归结果====================\n")
  print(multi_result, row.names = FALSE)
  # 将校正后的多因素回归结果导出为Excel文件，用于论文结果整理
  write_xlsx(multi_result, "C:/Users/86156/Desktop/多因素Logistic回归结果.xlsx")
}

# ===================== 第八步：设置森林图全局绘图主题 =====================
# 自定义森林图全局可视化主题参数，统一设置字体、置信区间样式、参考线格式，保证图表符合学术绘图规范
tm <- forest_theme(
  base_size = 10,                # 设置图表基础字体大小
  ci_col = "#453781",           # 设置置信区间线条颜色
  fill_ci_col = "#8E82FE",      # 设置置信区间色块填充颜色
  refline_gp = gpar(col = "red", lwd = 1), # 设置无效参考线颜色与线条粗细，参考线对应OR=1
  arrow_type = "closed"          # 设置置信区间两端为封闭式箭头，为流行病学常用图表样式
)

# ===================== 第九步：绘制单因素Logistic回归森林图 =====================
# 基于单因素回归结果数据集拼接置信区间文本格式，新增空白占位列用于置信区间水平线条布局，整理绘图表格数据
dt_uni <- uni_result %>%
  mutate(
    `OR(95%CI)` = paste0(OR, "(", CI95_L, "-", CI95_U, ")"),
    CI_占位 = paste(rep(" ", 30), collapse = " ")
  ) %>%
  select(变量详情, `OR(95%CI)`, CI_占位, P值)
# 重命名数据框列名，规范表格展示列标题
colnames(dt_uni) <- c("变量", "OR(95%CI)", "CI_占位", "P值")

# 调用forestploter包绘制单因素回归效应森林图，绑定绘图表格、效应值、置信区间参数与自定义主题
p1 <- forest(
  data = dt_uni,
  ci_column = 3,                 # 指定数据框第三列为置信区间绘图占位列
  est = uni_result$OR,            # 传入未校正比值比效应值
  lower = uni_result$CI95_L,      # 传入95%置信区间下限
  upper = uni_result$CI95_U,      # 传入95%置信区间上限
  ref_line = 1,                   # 设置无效效应参考线位置为1
  xlim = c(0, 10),                 # 限定横轴数值范围，避免极端置信区间被截断
  ticks_at = c(0, 1, 2, 5, 10), # 设置横轴刻度节点
  xlab = "Odds Ratio",            # 设置横轴坐标轴标签
  theme = tm                      # 调用前文自定义的全局绘图主题
)

# 将绘制完成的单因素森林图保存为TIFF格式高清图片，设置分辨率、画布宽高
grid::grid.draw(p1)
cat("正在保存：单因素Logistic回归森林图\n")
ggsave("C:/Users/86156/Desktop/单因素Logistic森林图.tiff",
       plot = p1, width = 12, height = 16, dpi = 600)

# ===================== 第十步：绘制多因素Logistic回归森林图 =====================
# 仅当多因素回归结果存在时，执行多因素森林图绘制流程
if (!is.null(multi_result)) {
  # 剔除模型截距项结果行，拼接校正OR及置信区间文本，新增布局占位列，整理多因素绘图表格数据
  dt_multi <- multi_result %>%
    filter(变量 != "(Intercept)") %>%
    mutate(
      `校正OR(95%CI)` = paste0(OR, "(", CI95_L, "-", CI95_U, ")"),
      CI_占位 = paste(rep(" ", 25), collapse = " ")
    ) %>%
    select(变量, `校正OR(95%CI)`, CI_占位, P值)
  colnames(dt_multi) <- c("变量", "校正OR(95%CI)", "CI_占位", "P值")
  
  # 剔除截距项，提取用于绘图的多因素效应量与置信区间数值数据集
  multi_data <- multi_result[multi_result$变量 != "(Intercept)", ]
  
  # 绘制校正混杂因素后的多因素回归森林图，沿用全局绘图主题与横轴布局设置
  p2 <- forest(
    data = dt_multi,
    ci_column = 3,
    est = multi_data$OR,
    lower = multi_data$CI95_L,
    upper = multi_data$CI95_U,
    ref_line = 1,
    xlim = c(0, 10),
    ticks_at = c(0, 1, 2 ,5, 10),
    xlab = "Adjusted Odds Ratio", # 设置横轴标签为校正比值比
    theme = tm
  )
  
  # 保存多因素回归森林图，固定画布尺寸
  grid::grid.draw(p2)
  cat("正在保存：多因素Logistic回归森林图\n")
  ggsave("C:/Users/86156/Desktop/多因素Logistic森林图.tiff",
         plot = p2, width = 12, height = 10, dpi = 600)
} 
# 加载所需包
library(readxl)
library(survey)
library(dplyr)
library(writexl)
library(forestploter)
library(grid)
library(ggplot2)

# 自定义Winsor缩尾函数
# 设置双侧截断百分位参数，分别定义下截断分位数与上截断分位数
# 计算指定变量的下截断临界值与上截断临界值
# 将小于下临界值的观测替换为下临界值，大于上临界值的观测替换为上临界值
# 返回完成极端值替换后的变量序列
winsor <- function(x, lower_p = 0.01, upper_p = 0.99) {
  q_lower <- quantile(x, probs = lower_p, na.rm = TRUE)
  q_upper <- quantile(x, probs = upper_p, na.rm = TRUE)
  x <- ifelse(x < q_lower, q_lower, x)
  x <- ifelse(x > q_upper, q_upper, x)
  return(x)
}

# 读取指定路径下的Excel格式数据集
df <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")
# 基于原始结局变量构建二分类因变量
# 原始取值1赋值为1，代表糖尿病；其余取值赋值为0，代表非糖尿病
df$y <- ifelse(df$`是否确诊糖尿病` == 1, 1, 0)

# 构建字符向量存储所有连续自变量名称
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

# 构建字符向量存储所有分类自变量名称
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

# 循环遍历分类变量向量
# 将每一个分类变量的数据格式转换为因子格式
for (v in cat_vars) {
  df[[v]] <- factor(df[[v]])
}

# 筛选抽样权重变量与结局变量均无缺失值的观测样本
df_valid <- df[!is.na(df$wt) & !is.na(df$y), ]

# 循环遍历所有连续自变量
# 调用自定义缩尾函数对连续变量进行百分位极端值处理
# 统一设置下截断百分位为0.01，上截断百分位为0.99
for (v in cont_vars) {
  df_valid[[v]] <- winsor(df_valid[[v]], lower_p = 0.01, upper_p = 0.99)
}

# 构建抽样调查设计对象
# 设置独立个体抽样结构，绑定归一化抽样权重
svy_des <- svydesign(ids = ~1, weights = ~wt, data = df_valid)

# 构建加权多元Logistic回归模型
# 设定模型因变量与全部纳入的自变量集合
# 指定模型所用抽样设计对象
# 设定模型分布族为准二项分布，适配复杂抽样下的二分类结局
fit_weight <- svyglm(
  formula = y ~ 年龄岁 + 性别 + 种族分类1 + 受教育程度 + 婚姻状况 + 高血压患病史 + 是否吸烟 +
    家庭收入贫困比值 + 体质指数BMI + 上臂围cm + 腰围cm + 臀围cm + 收缩压 +
    白细胞计数 + 淋巴细胞百分比 + 中性粒细胞百分比 + 淋巴细胞绝对值 +
    单核细胞绝对值 + 中性粒细胞绝对值 + 嗜碱性粒细胞绝对值 + 平均红细胞体积 +
    平均红细胞血红蛋白含量 + 红细胞分布宽度 + 高密度脂蛋白胆固醇国际单位制 +
    总胆固醇国际单位制 + 甘油三酯国际单位制 + 低密度脂蛋白胆固醇国际单位制 +
    计算法低密度脂蛋白胆固醇国际单位制 + 非高密度脂蛋白胆固醇国际单位制,
  design = svy_des,
  family = quasibinomial
)

# 提取模型回归系数汇总结果
coef_tab <- coef(summary(fit_weight))
# 计算回归系数95%置信区间
ci_tab <- confint(fit_weight)

# 提取模型自变量名称作为结果表第一列内容
# 对模型回归系数进行指数转换得到比值比，数值保留四位小数
# 计算回归系数95%置信区间并做指数转换，上下限数值保留四位小数
# 提取模型检验原始P值用于显著性判定
res_weight <- data.frame(
  变量 = rownames(coef_tab),
  OR = round(exp(coef_tab[, 1]), 4),
  下限95CI = round(exp(ci_tab[, 1]), 4),
  上限95CI = round(exp(ci_tab[, 2]), 4),
  P数值 = as.numeric(coef_tab[, 4])
)

# 按照SPSS规则将显著性星号拼接在P值后方
res_weight <- res_weight %>%
  mutate(
    P值 = case_when(
      P数值 < 0.001 ~ paste0(format(P数值, digits = 4, scientific = FALSE), "***"),
      P数值 < 0.01  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "**"),
      P数值 < 0.05  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "*"),
      TRUE ~ format(P数值, digits = 4, scientific = FALSE)
    )
  ) %>%
  select(-P数值)

# 在控制台打印加权敏感性分析结果表格
cat("========= 1%-99%缩尾加权Logistic回归敏感性分析结果 =========\n")
cat("显著性说明：*** P<0.001；** P<0.01；* P<0.05\n")
print(res_weight, row.names = FALSE)

# 将回归分析结果表格导出为Excel格式文件并保存至指定路径
write_xlsx(res_weight, "C:/Users/86156/Desktop/1_99缩尾_加权敏感性分析结果.xlsx")

# 剔除模型截距项对应的结果行，仅保留自变量效应估计结果
dt <- res_weight[res_weight$变量 != "(Intercept)", ]

# 拼接比值比与置信区间为标准化文本格式
# 生成空白字符列用于置信区间线条布局占位
dt$`OR(95%CI)` <- paste0(dt$OR, "(", dt$下限95CI, "-", dt$上限95CI, ")")
dt$CI <- paste(rep(" ", 30), collapse = "")

# 筛选绘图所需字段，构建森林图基础数据表
dt_forest <- dt[, c("变量", "OR(95%CI)", "CI", "P值")]

# 自定义森林图主题参数配置
# 设置置信区间线条颜色
# 设置置信区间填充色块颜色
# 设置无效效应参考线颜色
# 设置参考线线型为虚线
# 设置参考线线条粗细
# 设置置信区间端点样式为封闭式箭头
# 设置坐标轴字体大小
# 设置表格文字字体大小
tm <- forest_theme(
  ci_col = "#453781",
  fill_ci_col = "#8E82FE",
  refline_gp = gpar(col = "red", lwd = 1, lty = 2),
  arrow_type = "closed",
  xaxis_gp = gpar(fontsize = 8),
  table_gp = gpar(fontsize = 9)
)

# 基于配置数据表与主题参数绘制森林图
# 指定置信区间绘图所在列序号
# 传入比值比数值作为效应点坐标
# 传入置信区间上下限数值
# 设置无效效应参考线横坐标位置
# 设定横轴数值显示范围
# 定义横轴刻度节点位置
# 设置横轴坐标轴名称
# 绑定预设绘图主题参数
p <- forest(
  data = dt_forest,
  ci_column = 3,
  est = dt$OR,
  lower = dt$下限95CI,
  upper = dt$上限95CI,
  ref_line = 1,
  xlim = c(0, 10),
  ticks_at = c(0, 1, 2, 5, 10),
  xlab = "Odds Ratio (95% CI)",
  theme = tm
)

# 在绘图窗口预览生成的森林图
print(p)

# 将森林图保存为TIFF格式高清图片
# 设定图片宽度、高度与分辨率参数
ggsave("C:/Users/86156/Desktop/1_99缩尾_加权敏感性分析_森林图.tiff",
       p, width = 12, height = 10, dpi = 600)
# 加载分析所需R包
# readxl用于读取Excel格式数据集
# survey用于构建复杂抽样设计并实现加权广义线性模型拟合
# dplyr用于数据整理、变量新增与结果格式化处理
# car用于多重共线性诊断相关统计函数调用
library(readxl)
library(survey)
library(dplyr)
library(car)

# 读取指定路径下的Excel原始数据集
df <- read_excel("C:/Users/86156/Desktop/糖尿病权重.xlsx")
# 构建二分类结局变量
# 原始数据取值1代表确诊糖尿病，赋值为1；其余取值统一赋值为0代表未确诊糖尿病
df$y <- ifelse(df$`是否确诊糖尿病` == 1, 1, 0)
# 定义字符变量存储抽样权重列名，便于后续统一调用修改
weight_col <- "wt"

# 构建字符向量存储全部分类自变量名称
cat_vars <- c(
  "性别","种族分类1","受教育程度","婚姻状况","既往饮酒状态",
  "高血压患病史","饮食限制情况","饮食相关补充项1","饮食相关补充项2",
  "饮食相关补充项3","自评总体健康状况","近30天饮酒情况","空腹状态标识",
  "超敏C反应蛋白分级","是否吸烟"
)

# 构建字符向量存储全部连续自变量名称
cont_vars <- c(
  "年龄岁","家庭收入贫困比值","体质指数BMI","上臂围cm","腰围cm","臀围cm",
  "收缩压","舒张压","白细胞计数","淋巴细胞百分比","单核细胞百分比",
  "中性粒细胞百分比","嗜酸性粒细胞百分比","嗜碱性粒细胞百分比",
  "淋巴细胞绝对值","单核细胞绝对值","中性粒细胞绝对值","嗜酸性粒细胞绝对值",
  "嗜碱性粒细胞绝对值","红细胞计数","血红蛋白","红细胞压积","平均红细胞体积",
  "平均红细胞血红蛋白含量","红细胞分布宽度","血小板计数","平均血小板体积",
  "有核红细胞","高密度脂蛋白胆固醇国际单位制","超敏C反应蛋白","每日久坐时长",
  "总胆固醇国际单位制","甘油三酯国际单位制","低密度脂蛋白胆固醇国际单位制",
  "计算法低密度脂蛋白胆固醇国际单位制","非高密度脂蛋白胆固醇国际单位制"
)
# 合并分类变量与连续变量向量，得到全部自变量集合
all_vars <- c(cat_vars, cont_vars)

# 循环遍历所有分类变量
# 将变量数据格式转换为因子类型，回归模型会自动将首个水平设为参照组
for (v in cat_vars) {
  df[[v]] <- factor(df[[v]])
}

# 筛选有效分析样本
# 保留抽样权重与结局变量均不存在缺失值的观测记录
df_valid <- df[!is.na(df[[weight_col]]) & !is.na(df$y), ]

# 按照性别变量完成研究人群亚组拆分
# 筛选性别编码为1的观测，构建男性亚组数据集
df_male   <- subset(df_valid, 性别 == "1")
# 筛选性别编码为2的观测，构建女性亚组数据集
df_female <- subset(df_valid, 性别 == "2")

# 基于男性亚组数据集构建抽样调查设计对象
# 设定独立个体抽样结构，绑定归一化抽样权重
svy_male   <- svydesign(ids = ~1, weights = ~wt, data = df_male)
# 基于女性亚组数据集构建抽样调查设计对象
# 抽样结构与权重设置和男性亚组保持一致，保证两组模型可比
svy_female <- svydesign(ids = ~1, weights = ~wt, data = df_female)

# 设定必须强制纳入模型的混杂变量集合，用于校正混杂偏倚
confounder_vars <- c("年龄岁", "种族分类1", "受教育程度")
# 选取单因素分析中具备统计学意义的自变量作为多因素模型候选变量
sig_vars <- c("婚姻状况","高血压患病史","体质指数BMI","腰围cm","高密度脂蛋白胆固醇国际单位制")
# 合并混杂变量与显著自变量并去除重复变量名称，确定最终纳入模型的变量集合
model_vars <- unique(c(confounder_vars, sig_vars))
# 将变量向量拼接为模型公式格式
formula_str <- as.formula(paste("y ~", paste(model_vars, collapse = " + ")))

# 男性亚组多因素加权Logistic回归模型拟合
fit_male <- svyglm(formula_str, design = svy_male, family = quasibinomial)
# 提取模型回归系数、标准误、Z统计量与P值汇总表
coef_m <- coef(summary(fit_male))
# 计算各变量回归系数对应的95%置信区间
ci_m <- confint(fit_male)

# 整理男性亚组回归结果数据表
# 提取变量名称、指数化后比值比、置信区间上下限、原始P值
res_male <- data.frame(
  变量 = rownames(coef_m),
  OR = round(exp(coef_m[, 1]), 3),
  CI95_L = round(exp(ci_m[, 1]), 3),
  CI95_U = round(exp(ci_m[, 2]), 3),
  P数值 = as.numeric(coef_m[, 4])
)

# 根据SPSS显著性标记规则在P值后拼接对应星号
# P小于0.001标记***，P介于0.001至0.01标记**，P介于0.01至0.05标记*，其余不添加标记
# 格式化P值保留四位小数，关闭科学计数法显示格式
res_male <- res_male %>%
  mutate(
    P值 = case_when(
      P数值 < 0.001 ~ paste0(format(P数值, digits = 4, scientific = FALSE), "***"),
      P数值 < 0.01  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "**"),
      P数值 < 0.05  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "*"),
      TRUE ~ format(P数值, digits = 4, scientific = FALSE)
    )
  ) %>% select(-P数值)

# 在控制台输出男性亚组回归结果表头与显著性说明规则
cat("==================== 【男性亚组 性别=1】多因素加权Logistic回归结果 ====================\n")
cat("显著性：*** P<0.001；** P<0.01；* P<0.05\n")
# 在控制台打印男性亚组完整回归结果表格
print(res_male, row.names = FALSE)

# 女性亚组多因素加权Logistic回归模型拟合
fit_female <- svyglm(formula_str, design = svy_female, family = quasibinomial)
# 提取女性亚组模型回归系数、标准误、Z统计量与P值汇总表
coef_f <- coef(summary(fit_female))
# 计算女性亚组各变量回归系数对应的95%置信区间
ci_f <- confint(fit_female)

# 整理女性亚组回归结果数据表
# 提取变量名称、指数化后比值比、置信区间上下限、原始P值
res_female <- data.frame(
  变量 = rownames(coef_f),
  OR = round(exp(coef_f[, 1]), 3),
  CI95_L = round(exp(ci_f[, 1]), 3),
  CI95_U = round(exp(ci_f[, 2]), 3),
  P数值 = as.numeric(coef_f[, 4])
)

# 按照统一的SPSS显著性规则格式化P值并在末尾拼接显著性星号
res_female <- res_female %>%
  mutate(
    P值 = case_when(
      P数值 < 0.001 ~ paste0(format(P数值, digits = 4, scientific = FALSE), "***"),
      P数值 < 0.01  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "**"),
      P数值 < 0.05  ~ paste0(format(P数值, digits = 4, scientific = FALSE), "*"),
      TRUE ~ format(P数值, digits = 4, scientific = FALSE)
    )
  ) %>% select(-P数值)

# 在控制台输出女性亚组回归结果表头与显著性说明规则
cat("\n==================== 【女性亚组 性别=2】多因素加权Logistic回归结果 ====================\n")
cat("显著性：*** P<0.001；** P<0.01；* P<0.05\n")
# 在控制台打印女性亚组完整回归结果表格
print(res_female, row.names = FALSE)