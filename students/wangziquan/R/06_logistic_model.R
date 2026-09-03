# =========================================================
# 06_logistic_model.R
# 项目：成年人BMI与高血压患病风险关联分析
# 目的：
# 1. 建立BMI与高血压的Logistic回归模型
# 2. 比较粗模型与调整模型
# 3. 输出OR、95%CI和P值
# =========================================================


# 加载数据处理、绘图和读取功能
library(tidyverse)

# 加载回归结果整理功能
library(broom)

# 设置默认编码为UTF-8，避免中文输出乱码
options(encoding = "UTF-8")


# ---------------------------------------------------------
# 1. 创建输出文件夹
# ---------------------------------------------------------

# 如果tables文件夹不存在，就创建；若已存在则不提示警告
dir.create("tables", showWarnings = FALSE)

# 如果figures文件夹不存在，就创建；若已存在则不提示警告
dir.create("figures", showWarnings = FALSE)


# ---------------------------------------------------------
# 2. 读取清洗后数据
# ---------------------------------------------------------

# 读取已经完成清洗的最终分析数据
data_final <- read_csv(
  "output/nhanes_bmi_hypertension_clean.csv",
  show_col_types = FALSE
)

# 查看行数和列数，确认数据不是空的
dim(data_final)

# 查看变量名称和变量类型
glimpse(data_final)


# ---------------------------------------------------------
# 3. 整理模型所需变量
# ---------------------------------------------------------

# 建立用于回归分析的数据集
data_model <- data_final %>%
  mutate(
    
    # 设置BMI分组顺序，并将正常体重设为第一个水平
    # Logistic回归会自动以第一个水平Normal作为参照组
    bmi_group_clean = factor(
      bmi_group_clean,
      levels = c("Normal", "Underweight", "Overweight", "Obesity")
    ),
    
    # 将高血压变量统一转换为0和1
    # 0表示无高血压，1表示高血压
    hypertension_binary = case_when(
      hypertension_main == 1 ~ 1,
      hypertension_main == 0 ~ 0,
      as.character(hypertension_main) == "1" ~ 1,
      as.character(hypertension_main) == "0" ~ 0,
      as.character(hypertension_main) == "Hypertension" ~ 1,
      as.character(hypertension_main) == "No hypertension" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 将性别转为分类变量
    sex_clean = factor(sex_clean),
    
    # 设置吸烟状态顺序
    # Never smoker作为后续模型中的默认参照组
    smoking_model = factor(
      smoking_model,
      levels = c("Never smoker", "Ever smoker", "Unknown")
    )
  ) %>%
  
  # 保留模型需要的完整观测值
  # BMI、高血压、年龄、性别、吸烟任意缺失者不进入完整调整模型
  filter(
    !is.na(bmi_group_clean),
    !is.na(hypertension_binary),
    !is.na(age),
    !is.na(sex_clean),
    !is.na(smoking_model)
  )

# 查看最终进入模型的样本量
nrow(data_model)

# 查看高血压0和1是否都存在
table(data_model$hypertension_binary, useNA = "ifany")

# 查看BMI分组人数是否正常
table(data_model$bmi_group_clean, useNA = "ifany")


# ---------------------------------------------------------
# 4. 自定义函数：将Logistic回归结果转换为OR表
# ---------------------------------------------------------

# model是输入的glm回归模型；model_name是模型名称
tidy_logistic_or <- function(model, model_name) {
  
  # broom::tidy提取回归结果
  # conf.int=TRUE表示计算95%置信区间
  # exponentiate=TRUE表示将回归系数转换为OR
  broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    
    # 删除截距项，因为截距通常不作为主要研究结果展示
    filter(term != "(Intercept)") %>%
    
    # 添加模型名称并整理列名
    transmute(
      model = model_name,
      variable = term,
      OR = estimate,
      CI_low = conf.low,
      CI_high = conf.high,
      p_value = p.value
    )
}


# ---------------------------------------------------------
# 5. 自定义函数：提取 Logistic 回归的 OR、95%CI 和 P 值
# ---------------------------------------------------------

tidy_logistic_or <- function(model, model_name) {
  
  broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      model = model_name,
      variable = term,
      OR = estimate,
      CI_low = conf.low,
      CI_high = conf.high,
      p_value = p.value
    )
}


# ---------------------------------------------------------
# 6. 模型1：粗模型
# ---------------------------------------------------------

# 仅纳入 BMI 分组，观察未经调整的关联
model_crude <- glm(
  hypertension_binary ~ bmi_group_clean,
  data = data_model,
  family = binomial()
)

summary(model_crude)


# ---------------------------------------------------------
# 7. 模型2：调整年龄和性别的模型
# ---------------------------------------------------------

# 年龄和性别可能同时与 BMI 和高血压相关，
# 因此作为主要混杂因素纳入调整模型
model_adjusted <- glm(
  hypertension_binary ~ bmi_group_clean + age + sex_clean,
  data = data_model,
  family = binomial()
)

summary(model_adjusted)


# ---------------------------------------------------------
# 8. 汇总粗模型和调整模型的 OR、95%CI 和 P 值
# ---------------------------------------------------------

model_or_table <- bind_rows(
  
  tidy_logistic_or(
    model_crude,
    "Crude model"
  ),
  
  tidy_logistic_or(
    model_adjusted,
    "Adjusted for age and sex"
  )
) %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p_value = round(p_value, 4)
  )

# 查看回归结果表
model_or_table

# 保存结果表
write_csv(
  model_or_table,
  "tables/logistic_regression_or_table.csv",
  na = ""
)

# ---------------------------------------------------------
# 9. 提取调整模型中BMI分组的结果，用于绘制森林图
# ---------------------------------------------------------

# 从所有回归结果中，只保留“调整年龄和性别”模型
# 同时只保留BMI分组对应的回归结果
forest_data <- model_or_table %>%
  filter(
    model == "Adjusted for age and sex",
    str_detect(variable, "bmi_group_clean")
  ) %>%
  
  # 把代码里的变量名称转换为汇报时更容易理解的BMI组名称
  mutate(
    bmi_group = case_when(
      variable == "bmi_group_cleanUnderweight" ~ "Underweight",
      variable == "bmi_group_cleanOverweight" ~ "Overweight",
      variable == "bmi_group_cleanObesity" ~ "Obesity",
      TRUE ~ variable
    ),
    
    # 制作图中显示的文字标签：OR和95%CI
    label = paste0(
      "OR = ", OR,
      " (", CI_low, "–", CI_high, ")"
    ),
    
    # 固定图中BMI组从上到下的展示顺序
    bmi_group = factor(
      bmi_group,
      levels = c("Underweight", "Overweight", "Obesity")
    )
  )

# 在Console中查看森林图使用的数据
forest_data


# ---------------------------------------------------------
# 10. 绘制调整模型森林图
# ---------------------------------------------------------

# 创建森林图
p_forest <- ggplot(
  forest_data,
  
  # x轴放OR；y轴放BMI分组
  aes(
    x = OR,
    y = bmi_group
  )
) +
  
  # 绘制OR的点估计
  geom_point(size = 3) +
  
  # 绘制95%置信区间
  # xmin为置信区间下限，xmax为置信区间上限
  geom_errorbar(
    aes(
      xmin = CI_low,
      xmax = CI_high
    ),
    width = 0.15,
    orientation = "y"
  ) +
  
  # OR=1表示“与正常体重组没有差异”
  # 用虚线作为无关联参考线
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  
  # 在每个点右侧标注OR和95%CI
  geom_text(
    aes(label = label),
    hjust = -0.08,
    size = 3
  ) +
  
  # OR通常采用对数坐标展示
  # 因为OR=0.5和OR=2相对于1的偏离在对数尺度上更对称
  scale_x_log10() +
  
  # 在右侧预留空间，防止文字标签被截断
  coord_cartesian(
    xlim = c(
      min(forest_data$CI_low, na.rm = TRUE) * 0.8,
      max(forest_data$CI_high, na.rm = TRUE) * 2.2
    )
  ) +
  
  # 设置标题、坐标轴标题和图注
  labs(
    title = "Association between BMI group and hypertension",
    subtitle = "Logistic regression adjusted for age and sex",
    x = "Odds ratio, log scale",
    y = "BMI group",
    caption = paste(
      "Reference group: Normal BMI.",
      "Smoking was not included because the available smoking variable",
      "had no valid category variation."
    )
  ) +
  
  # 使用简洁的图形主题
  theme_minimal() +
  
  # 调整文字位置，让图更加适合课堂汇报
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    axis.title = element_text(face = "bold")
  )

# 在RStudio右下角Plots窗口显示森林图
p_forest


# ---------------------------------------------------------
# 11. 保存森林图
# ---------------------------------------------------------

# 将森林图保存为PNG文件
# dpi=300适合后续放进PPT或报告
ggsave(
  filename = "figures/06_logistic_regression_forest_plot.png",
  plot = p_forest,
  width = 9,
  height = 5,
  dpi = 300
)
# ---------------------------------------------------------# ---------------------------------------------------------
# 12. 生成模型解释与局限性说明文件
# ---------------------------------------------------------

# 将模型方法、解释原则和局限性写成字符向量
# 每一行将来会写入 Markdown 文件的一行
model_text <- c(
  "# 课前任务：BMI与高血压的Logistic回归分析",
  "",
  "## 研究问题",
  "",
  "在成年人样本中，不同BMI分组与高血压患病状态是否存在关联？",
  "",
  "## 模型设置",
  "",
  "模型1：仅纳入BMI分组，得到未调整的粗OR。",
  "模型2：在模型1基础上调整年龄和性别。",
  "",
  "## 结果解释原则",
  "",
  "OR大于1表示相对于正常体重组，该BMI组高血压患病优势更高。",
  "OR小于1表示相对于正常体重组，该BMI组高血压患病优势更低。",
  "95%CI若不跨越1，说明该关联具有一定统计学证据。",
  "由于本研究为横断面分析，结果应解释为关联，不能直接解释为因果关系。",
  "",
  "## 当前局限",
  "",
  "本阶段模型尚未使用NHANES复杂抽样权重。",
  "协变量数量有限，仍可能存在残余混杂。",
  "横断面数据不能确定BMI与高血压之间的时间先后关系。",
  "当前清洗后数据中的吸烟变量全部为Unknown，缺乏有效类别变异，因此未纳入本阶段调整模型。",
  "后续可重新核查NHANES吸烟问卷变量定义，并在变量有效后进行敏感性分析。"
)

# 打开一个UTF-8编码的Markdown文件
# open = "w" 表示写入；若同名文件已存在，会覆盖旧文件
con <- file(
  "tables/logistic_model_interpretation_0623.md",
  open = "w",
  encoding = "UTF-8"
)

# 将model_text逐行写入文件
# useBytes = TRUE 有助于中文字符按UTF-8正确写出
writeLines(
  model_text,
  con = con,
  useBytes = TRUE
)

# 关闭文件连接，确保内容真正保存
close(con)
# 13. 最后检查文件是否生成
# ---------------------------------------------------------

file.exists("tables/logistic_regression_or_table.csv")
file.exists("figures/06_logistic_regression_forest_plot.png")
file.exists("tables/logistic_model_interpretation_0623.md")