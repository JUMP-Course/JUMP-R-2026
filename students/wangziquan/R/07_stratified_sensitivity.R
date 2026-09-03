# =========================================================
# 07_stratified_sensitivity.R
# 项目：成年人 BMI 与高血压患病状态关联分析

# 1. 按性别进行分层 Logistic 回归
# 2. 将 BMI 作为连续变量进行敏感性分析
# 3. 检验 BMI 分组与性别是否存在交互作用
# 4. 绘制按性别分层的森林图
# 5. 统一使用 UTF-8 格式保存表格和说明文件
# =========================================================


# ---------------------------------------------------------
# 0. 加载所需 R 包
# ---------------------------------------------------------

# tidyverse 包含数据读取、整理、绘图和导出功能
library(tidyverse)

# 如果电脑还没有安装 broom，则自动安装一次
# broom 用于将回归模型结果整理为 OR、95%CI、P 值表
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}

# 加载 broom 包
library(broom)

# 设置默认编码为 UTF-8
# 目的：避免中文文件上传 GitHub 或在其他电脑打开时乱码
options(encoding = "UTF-8")


# ---------------------------------------------------------
# 1. 创建输出文件夹
# ---------------------------------------------------------

# 如果 tables 文件夹不存在，就创建它
# showWarnings = FALSE 表示文件夹已经存在时不显示警告
dir.create("tables", showWarnings = FALSE)

# 如果 figures 文件夹不存在，就创建它
dir.create("figures", showWarnings = FALSE)


# ---------------------------------------------------------
# 2. 读取清洗后的最终分析数据
# ---------------------------------------------------------

# 读取已经在前面清洗完成的分析数据
# show_col_types = FALSE 用于减少 Console 中变量类型提示信息
data_final <- read_csv(
  "output/nhanes_bmi_hypertension_clean.csv",
  show_col_types = FALSE
)

# 查看数据的行数和列数
# 目的：确认数据没有读成空数据
dim(data_final)

# 查看变量名称、类型及部分内容
# 目的：确认 bmi、age、sex_clean 等变量存在
glimpse(data_final)


# ---------------------------------------------------------
# 3. 整理模型所需变量
# ---------------------------------------------------------

data_model <- data_final %>%
  mutate(
    
    # 将年龄转换为数值型
    # 目的：年龄在回归模型中作为连续变量进入
    age = as.numeric(age),
    
    # 将 BMI 转换为数值型
    # 目的：后续敏感性分析需要将 BMI 作为连续变量
    bmi = as.numeric(bmi),
    
    # 设置 BMI 分组顺序
    # Normal 放在第一个水平，Logistic 回归会自动将其作为参照组
    bmi_group_clean = factor(
      as.character(bmi_group_clean),
      levels = c("Normal", "Underweight", "Overweight", "Obesity")
    ),
    
    # 先把原始高血压变量转换为字符
    # 目的：兼容数字型 0/1 和字符型 Hypertension/No hypertension
    hypertension_chr = as.character(hypertension_main),
    
    # 将高血压统一定义为二分类变量
    # 1 = 高血压；0 = 无高血压
    hypertension_binary = case_when(
      hypertension_chr %in% c("1", "Hypertension") ~ 1,
      hypertension_chr %in% c("0", "No hypertension") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 将性别设置为分类变量
    # 明确 Female 和 Male 的顺序，便于后续分层和交互作用分析
    sex_clean = factor(
      as.character(sex_clean),
      levels = c("Female", "Male")
    ),
    
    # 构建 BMI 每增加 5 kg/m² 的变量
    # 目的：连续 BMI 的 OR 用“每增加 5 kg/m²”解释更直观
    bmi_5 = bmi / 5
  ) %>%
  
  # 只保留模型必需变量均不缺失的对象
  # 目的：Logistic 回归不能使用结局、暴露或协变量缺失的记录
  filter(
    !is.na(age),
    !is.na(bmi),
    !is.na(bmi_5),
    !is.na(bmi_group_clean),
    !is.na(hypertension_binary),
    !is.na(sex_clean)
  ) %>%
  
  # 删除筛选后已经没有人的空因子水平
  # 目的：避免回归时报“因子只有一个水平”等错误
  droplevels()


# ---------------------------------------------------------
# 4. 检查进入模型的数据是否正常
# ---------------------------------------------------------

# 查看最终进入补充分析的样本量
nrow(data_model)

# 查看 BMI 各组人数
table(data_model$bmi_group_clean, useNA = "ifany")

# 查看男女样本量
table(data_model$sex_clean, useNA = "ifany")

# 查看高血压结局是否同时有 0 和 1
table(data_model$hypertension_binary, useNA = "ifany")

# 如果最终样本量为 0，停止运行并提示原因
if (nrow(data_model) == 0) {
  stop("data_model 没有有效样本，请检查前面的数据清洗和变量名称。")
}

# 如果高血压结局只剩一种取值，停止运行
if (n_distinct(data_model$hypertension_binary) < 2) {
  stop("高血压变量只有一种取值，无法进行 Logistic 回归。")
}


# ---------------------------------------------------------
# 5. 自定义函数：统一整理 Logistic 回归的 OR 表
# ---------------------------------------------------------

# model：输入一个 glm Logistic 回归模型
# model_name：给模型命名，例如“女性分层模型”
# strata_name：记录该结果属于哪个分层人群
tidy_logistic_or <- function(model, model_name, strata_name = NA_character_) {
  
  # broom::tidy 提取模型结果
  # conf.int = TRUE 表示计算 95%CI
  # exponentiate = TRUE 表示把回归系数转换为 OR
  broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    
    # 截距项不属于研究重点，因此删除
    filter(term != "(Intercept)") %>%
    
    # 整理为统一列名，方便后续合并和导出
    transmute(
      model = model_name,
      strata = strata_name,
      variable = term,
      OR = estimate,
      CI_low = conf.low,
      CI_high = conf.high,
      p_value = p.value
    )
}


# ---------------------------------------------------------
# 6. 自定义函数：统一保存 UTF-8 CSV 文件
# ---------------------------------------------------------

# data：要保存的数据框
# path：文件保存路径
save_csv_utf8 <- function(data, path) {
  
  # readr::write_csv 默认采用 UTF-8 编码
  # na = "" 表示缺失值保存为空白，便于 Excel 查看
  readr::write_csv(
    data,
    file = path,
    na = ""
  )
}


# ---------------------------------------------------------
# 7. 自定义函数：统一保存 UTF-8 Markdown 说明文件
# ---------------------------------------------------------

# lines：要写入的文字向量
# path：Markdown 文件保存路径
save_text_utf8 <- function(lines, path) {
  
  # 创建 UTF-8 编码的文件连接
  con <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )
  
  # 确保函数结束后文件连接关闭
  on.exit(close(con), add = TRUE)
  
  # 将文字逐行写入 Markdown 文件
  writeLines(
    lines,
    con = con,
    useBytes = TRUE
  )
}


# ---------------------------------------------------------
# 8. 生成按性别分层前的样本概况表
# ---------------------------------------------------------

# 计算女性和男性各自的样本量、高血压人数和高血压患病率
# 目的：分层回归前先了解每个亚组的基本情况
stratified_sample_summary <- data_model %>%
  group_by(sex_clean) %>%
  summarise(
    n = n(),
    hypertension_n = sum(hypertension_binary == 1),
    hypertension_rate_pct = round(
      mean(hypertension_binary == 1) * 100,
      1
    ),
    .groups = "drop"
  )

# 在 Console 查看分层样本概况
stratified_sample_summary

# 保存分层样本概况表
save_csv_utf8(
  stratified_sample_summary,
  "tables/stratified_sample_summary_by_sex.csv"
)


# ---------------------------------------------------------
# 9. 自定义函数：在某一性别亚组中建立回归模型
# ---------------------------------------------------------

# data：完整模型数据
# sex_value：需要筛选的性别，例如 Female 或 Male
fit_sex_stratified_model <- function(data, sex_value) {
  
  # 筛选当前性别亚组
  data_sub <- data %>%
    filter(as.character(sex_clean) == sex_value)
  
  # 检查该性别亚组中是否同时存在高血压和非高血压对象
  if (n_distinct(data_sub$hypertension_binary) < 2) {
    stop(
      paste0(
        sex_value,
        " 亚组中高血压变量只有一个取值，无法进行 Logistic 回归。"
      )
    )
  }
  
  # 检查 BMI 分组是否至少有两组
  if (n_distinct(data_sub$bmi_group_clean) < 2) {
    stop(
      paste0(
        sex_value,
        " 亚组中 BMI 分组少于两组，无法进行 Logistic 回归。"
      )
    )
  }
  
  # 在当前性别亚组内建立回归模型
  # 已经按性别分层，因此公式中不再放 sex_clean
  # 仍调整年龄，因为年龄与 BMI 和高血压都可能有关
  model_sub <- glm(
    hypertension_binary ~ bmi_group_clean + age,
    data = data_sub,
    family = binomial()
  )
  
  # 将该亚组的模型结果整理成 OR 表
  tidy_logistic_or(
    model = model_sub,
    model_name = "BMI group + age",
    strata_name = sex_value
  )
}


# ---------------------------------------------------------
# 10. 按性别分别建立 Logistic 回归模型
# ---------------------------------------------------------

# 女性亚组模型
female_results <- fit_sex_stratified_model(
  data = data_model,
  sex_value = "Female"
)

# 男性亚组模型
male_results <- fit_sex_stratified_model(
  data = data_model,
  sex_value = "Male"
)

# 合并女性和男性模型结果
stratified_results_raw <- bind_rows(
  female_results,
  male_results
)

# 生成适合导出和汇报的结果表
stratified_results <- stratified_results_raw %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p_value = round(p_value, 4)
  )

# 查看性别分层回归结果
stratified_results

# 保存性别分层结果
save_csv_utf8(
  stratified_results,
  "tables/stratified_logistic_by_sex.csv"
)


# ---------------------------------------------------------
# 11. 建立 BMI × 性别交互作用模型
# ---------------------------------------------------------

# 该模型用于检验 BMI 分组与高血压的关联
# 是否在不同性别中存在统计学上的差异
model_interaction <- glm(
  hypertension_binary ~ bmi_group_clean * sex_clean + age,
  data = data_model,
  family = binomial()
)

# 提取交互作用项
# 交互作用项通常含有冒号，例如：
# bmi_group_cleanObesity:sex_cleanMale
interaction_results <- broom::tidy(
  model_interaction,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(str_detect(term, ":")) %>%
  transmute(
    interaction_term = term,
    OR = round(estimate, 2),
    CI_low = round(conf.low, 2),
    CI_high = round(conf.high, 2),
    p_value = round(p.value, 4)
  )

# 查看交互作用结果
interaction_results

# 保存交互作用结果
save_csv_utf8(
  interaction_results,
  "tables/interaction_bmi_sex.csv"
)


# ---------------------------------------------------------
# 12. BMI 连续变量敏感性分析
# ---------------------------------------------------------

# 将 BMI 作为连续变量进入模型
# bmi_5 表示 BMI 每增加 5 kg/m²
# 同时调整年龄和性别
model_bmi_continuous <- glm(
  hypertension_binary ~ bmi_5 + age + sex_clean,
  data = data_model,
  family = binomial()
)

# 查看连续 BMI 模型的原始输出
summary(model_bmi_continuous)

# 整理连续 BMI 模型的 OR、95%CI 和 P 值
sensitivity_bmi_continuous_raw <- tidy_logistic_or(
  model = model_bmi_continuous,
  model_name = "BMI continuous model"
)

# 仅保留 BMI 每增加 5 kg/m² 的核心结果
sensitivity_bmi_continuous <- sensitivity_bmi_continuous_raw %>%
  filter(variable == "bmi_5") %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p_value = round(p_value, 4)
  )

# 查看 BMI 连续变量敏感性分析结果
sensitivity_bmi_continuous

# 保存 BMI 连续变量敏感性分析结果
save_csv_utf8(
  sensitivity_bmi_continuous,
  "tables/sensitivity_bmi_continuous.csv"
)


# ---------------------------------------------------------
# 13. 整理分层森林图需要的数据
# ---------------------------------------------------------

# 只保留 BMI 分组相关的回归结果
# 年龄变量不属于本图展示重点，因此不画入森林图
plot_stratified <- stratified_results_raw %>%
  filter(str_detect(variable, "^bmi_group_clean")) %>%
  mutate(
    
    # 将模型变量名称改成更容易理解的 BMI 分组名称
    bmi_group = case_when(
      variable == "bmi_group_cleanUnderweight" ~ "Underweight",
      variable == "bmi_group_cleanOverweight" ~ "Overweight",
      variable == "bmi_group_cleanObesity" ~ "Obesity",
      TRUE ~ variable
    ),
    
    # 固定 BMI 分组的展示顺序
    bmi_group = factor(
      bmi_group,
      levels = c("Underweight", "Overweight", "Obesity")
    ),
    
    # 固定性别分面顺序
    strata = factor(
      strata,
      levels = c("Female", "Male")
    )
  )

# 查看绘图数据
plot_stratified


# ---------------------------------------------------------
# 14. 绘制按性别分层的森林图
# ---------------------------------------------------------

# 图中：
# 点 = OR 点估计
# 横线 = 95%CI
# 虚线 OR=1 = 与正常体重组无关联
# 分面 = 女性和男性分别展示
p_stratified <- ggplot(
  plot_stratified,
  aes(
    x = bmi_group,
    y = OR
  )
) +
  
  # 添加 OR=1 的无关联参考线
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  
  # 添加 95% 置信区间
  geom_errorbar(
    aes(
      ymin = CI_low,
      ymax = CI_high
    ),
    width = 0.15
  ) +
  
  # 添加 OR 点估计
  geom_point(
    size = 3
  ) +
  
  # OR 通常使用对数坐标展示
  # 原因：OR<1 和 OR>1 相对于1的偏离在对数坐标中更对称
  scale_y_log10() +
  
  # 按性别分面
  facet_wrap(~ strata) +
  
  # 翻转坐标，让图形呈现传统森林图形式
  coord_flip() +
  
  # 设置标题、坐标轴和图注
  labs(
    title = "Stratified association between BMI group and hypertension",
    subtitle = "Logistic regression stratified by sex and adjusted for age",
    x = "BMI group",
    y = "Odds ratio, log scale",
    caption = "Reference group: Normal BMI. Each sex-specific model was adjusted for age."
  ) +
  
  # 使用简洁主题
  theme_minimal() +
  
  # 调整标题、图注和坐标轴文字样式
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    axis.title = element_text(face = "bold")
  )

# 在右下角 Plots 窗口显示森林图
p_stratified

# 保存森林图
# dpi = 300 表示适合放入课堂汇报和报告
ggsave(
  filename = "figures/stratified_logistic_by_sex_forest.png",
  plot = p_stratified,
  width = 9,
  height = 5,
  dpi = 300
)


# ---------------------------------------------------------
# 15. 生成补充分析说明文件
# ---------------------------------------------------------

# 获取 BMI 连续变量敏感性分析的核心结果
bmi_continuous_result <- sensitivity_bmi_continuous %>%
  slice(1)

# 将方法、解释原则和当前局限写成 Markdown 文字
extension_text <- c(
  "# 分层分析与敏感性分析说明",
  "",
  "## 分析目的",
  "",
  "本阶段在主分析基础上开展分层分析、交互作用分析和敏感性分析。",
  "目的在于评估 BMI 与高血压患病状态关联在不同性别中的表现，并检查主分析结果是否稳定。",
  "",
  "## 性别分层分析",
  "",
  "分别在女性和男性中建立 Logistic 回归模型。",
  "由于已经按性别分层，分层模型中不再纳入性别变量，仅调整年龄。",
  "",
  "## BMI 连续变量敏感性分析",
  "",
  "主分析将 BMI 作为分类变量。",
  "敏感性分析将 BMI 作为连续变量，并以每增加 5 kg/m² 为单位进行解释。",
  paste0(
    "连续 BMI 模型中，BMI 每增加 5 kg/m² 的调整 OR 为 ",
    bmi_continuous_result$OR,
    "，95%CI 为 ",
    bmi_continuous_result$CI_low,
    "–",
    bmi_continuous_result$CI_high,
    "，P 值为 ",
    bmi_continuous_result$p_value,
    "。"
  ),
  "",
  "## 交互作用分析",
  "",
  "BMI 分组与性别的交互作用项已单独输出至 interaction_bmi_sex.csv。",
  "交互作用检验用于判断 BMI 与高血压的关联是否在不同性别之间存在统计学差异。",
  "",
  "## 解释原则",
  "",
  "若女性和男性亚组中的关联方向大体一致，提示主分析结果具有一定稳定性。",
  "若交互作用项具有统计学证据，则提示 BMI 与高血压的关联可能因性别而异。",
  "所有分析均基于横断面数据，只能解释为关联，不能直接解释为因果。",
  "",
  "## 当前局限",
  "",
  "本阶段尚未使用 NHANES 复杂抽样权重。",
  "当前吸烟变量缺乏有效类别变异，因此未纳入模型。",
  "分层后部分 BMI 组样本量可能较小，估计结果需要谨慎解释。"
)

# 使用自定义函数保存 UTF-8 Markdown 文件
save_text_utf8(
  extension_text,
  "tables/extension_analysis_interpretation.md"
)


# ---------------------------------------------------------
# 16. 最后检查所有输出文件是否生成
# ---------------------------------------------------------

# 将需要检查的文件整理为一个表
file_check <- tibble(
  file = c(
    "tables/stratified_sample_summary_by_sex.csv",
    "tables/stratified_logistic_by_sex.csv",
    "tables/interaction_bmi_sex.csv",
    "tables/sensitivity_bmi_continuous.csv",
    "figures/stratified_logistic_by_sex_forest.png",
    "tables/extension_analysis_interpretation.md"
  ),
  exists = file.exists(file)
)

# 查看每个文件是否成功生成
file_check

# 保存文件检查结果
save_csv_utf8(
  file_check,
  "tables/extension_file_check.csv"
)

# 如果所有文件都存在，返回 TRUE
all(file_check$exists)