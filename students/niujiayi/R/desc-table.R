# 加载所需分析包
library(gtsummary)
library(dplyr)
library(flextable)

# 读取刚清洗完成的最新数据集
analysis_data <- read.csv(
  "D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_最终清洗数据.csv",
  stringsAsFactors = TRUE
)

# 将原始11档饮酒频次合并为4个临床分组，简化表格、减少单元格小样本警告
analysis_data <- analysis_data %>%
  mutate(
    drink_group = case_when(
      ALQ121 == "从不饮酒" ~ "从不饮酒",
      ALQ121 %in% c("过去一年1-2次", "过去一年3-6次", "过去一年7-11次", "每月1次", "每月2-3次") ~ "少量饮酒（每月不足1次~每月2-3次）",
      ALQ121 %in% c("每周1次", "每周2次", "每周3-4次") ~ "中等饮酒（每周1~4次）",
      ALQ121 %in% c("几乎每天饮酒", "每天饮酒") ~ "频繁饮酒（几乎每天/每日饮酒）"
    ) %>% 
      factor(levels = c("从不饮酒", "少量饮酒（每月不足1次~每月2-3次）", "中等饮酒（每周1~4次）", "频繁饮酒（几乎每天/每日饮酒）"))
  )

# 生成规范基线特征表
baseline_table <- analysis_data %>%
  select(-SEQN) %>%
  tbl_summary(
    by = breast_cancer,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ c(1, 1), all_categorical() ~ c(0, 1)),
    label = list(
      RIDAGEYR ~ "年龄（岁）",
      BMXBMI ~ "体重指数（kg/m²）",
      drink_group ~ "饮酒频次分组"
    ),
    missing = "no" # 隐藏连续变量缺失单独行，表格更整洁
  ) %>%
  add_overall() %>% # 增加总样本列
  add_p() %>% # 自动组间差异检验
  modify_header(label = "**特征**") %>%
  modify_caption("**表1 研究对象基线特征表**")

# 在查看器中预览表格
baseline_table

# 导出Word格式表格（可直接粘贴进论文）
baseline_table %>%
  as_flex_table() %>%
  save_as_docx(path = "D:/JUMP-R-2026/students/niujiayi/doc/基线特征表.docx")

# 导出CSV备用表格
write.csv(
  baseline_table$table_body,
  "D:/JUMP-R-2026/students/niujiayi/doc/基线特征表.csv",
  row.names = FALSE
)