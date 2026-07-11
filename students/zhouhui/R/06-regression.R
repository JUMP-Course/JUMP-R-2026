if (!require("dplyr")) install.packages("dplyr")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("flextable")) install.packages("flextable")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(gtsummary)
library(flextable)
library(ggplot2)

setwd("D:/ukb-jump/gdm-chd-ukb")
df_clean <- readRDS("./data/ukb_cleaned_analysis_final.rds")

# 第一步：计算粗关联 —— 未校正任何混杂的原始效应
cat("===== 第一步：计算粗OR值（未校正混杂）=====\n")
# 构建简单Logistic回归模型，仅纳入暴露变量GDM
mod_crude <- glm(
  formula = chd ~ gdm, 
  data = df_clean, 
  family = binomial(link = "logit")
)
# 整理成规范的结果表
tbl_crude <- tbl_regression(
  mod_crude,
  exponentiate = TRUE,          # 将logOR转换为OR值（比值比）
  pvalue_fun = ~ style_pvalue(., digits = 3),
  label = list(gdm ~ "妊娠期糖尿病病史")
) %>%
  modify_header(
    label = "**变量**", 
    estimate = "**粗OR (95%CI)**", 
    p.value = "**P值**"
  ) %>%
  modify_caption("**粗关联分析：未校正混杂因素**")

print(tbl_crude)
# 第二步：核心分析 —— 逐步校正的多因素Logistic回归
cat("\n===== 第二步：构建逐步校正多因素模型 =====\n")

# 模型1：粗模型（仅GDM，和上面的粗关联一致，用于合并对比）
mod1 <- glm(chd ~ gdm, data = df_clean, family = binomial)

# 模型2：校正年龄（单独校正最强混杂因素）
mod2 <- glm(chd ~ gdm + age, data = df_clean, family = binomial)

# 模型3：校正年龄 + BMI（核心人口学+代谢混杂）
mod3 <- glm(chd ~ gdm + age + bmi, data = df_clean, family = binomial)

# 模型4：全校正模型（纳入所有已知混杂因素）
mod4 <- glm(
  formula = chd ~ gdm + age + bmi + smoking + alcohol + education,
  data = df_clean,
  family = binomial
)

# 合并4个模型结果，生成一张对比总表
tbl_model_compare <- tbl_merge(
  tbls = list(
    tbl_regression(mod1, exponentiate = TRUE, label = list(gdm ~ "妊娠期糖尿病病史")),
    tbl_regression(mod2, exponentiate = TRUE, label = list(gdm ~ "妊娠期糖尿病病史")),
    tbl_regression(mod3, exponentiate = TRUE, label = list(gdm ~ "妊娠期糖尿病病史")),
    tbl_regression(mod4, exponentiate = TRUE, label = list(gdm ~ "妊娠期糖尿病病史"))
  ),
  tab_spanner = c(
    "**模型1：粗模型**", 
    "**模型2：校正年龄**", 
    "**模型3：校正年龄+BMI**", 
    "**模型4：全校正模型**"
  )
) %>%
  modify_header(
    label = "**变量**",
    estimate_1 = "**OR (95%CI)**", p.value_1 = "**P值**",
    estimate_2 = "**OR (95%CI)**", p.value_2 = "**P值**",
    estimate_3 = "**OR (95%CI)**", p.value_3 = "**P值**",
    estimate_4 = "**OR (95%CI)**", p.value_4 = "**P值**"
  ) %>%
  modify_caption("**表3 不同校正模型下GDM与冠心病发病的关联强度**")

print(tbl_model_compare)
tbl_model_compare %>%
  as_flex_table() %>%
  save_as_docx(path = "./output/表3_多因素Logistic回归结果.docx")

cat("✅ 多因素回归分析完成，结果表已导出\n")


# 第三步：可视化 —— 全校正模型森林图
cat("\n===== 第三步：绘制全校正模型森林图 =====\n")

forest_plot <- tbl_regression(
  mod4, 
  exponentiate = TRUE,
  label = list(
    gdm ~ "妊娠期糖尿病病史",
    age ~ "年龄(岁)",
    bmi ~ "身体质量指数(kg/m²)",
    smoking ~ "吸烟状态",
    alcohol ~ "饮酒状态",
    education ~ "教育程度"
  )
) %>%
  plot() +
  labs(
    title = "全校正模型下各因素与冠心病发病的关联",
    x = "OR值 (95%置信区间)",
    y = "变量"
  ) +
  theme_bw(base_size = 12) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#E64B35", linewidth = 1)

print(forest_plot)

# 保存高清图片
ggsave(
  filename = "./figures/多因素回归森林图.png",
  plot = forest_plot,
  width = 8, height = 5, dpi = 300
)

cat("✅ 森林图绘制完成，已保存至figures文件夹\n")
cat("\n🎉 核心主分析全部完成！\n")
