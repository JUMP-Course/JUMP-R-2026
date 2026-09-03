install.packages(c("tidyverse", "data.table", "naniar", "ggplot2", "skimr"), dependencies = TRUE)
library(dplyr)
library(gtsummary)
library(flextable)
setwd("D:/ukb-jump/gdm-chd-ukb")
df_clean <- readRDS("./data/ukb_cleaned_analysis_final.rds")
cat("\n===== 带组间检验的基线对比表 =====\n")
tbl_baseline_p <- df_clean %>%
  # 按逻辑顺序选择要展示的变量：结局→核心混杂→分组变量→其他混杂
  select(chd, age, bmi, age_group, bmi_group, smoking, education, alcohol, gdm) %>%
  
  tbl_summary(
    by = gdm,  # 按是否患GDM分组统计
    # 按变量分布匹配对应的统计格式
    statistic = list(
      age ~ "{mean} ± {sd}",                # 年龄：正态 → 均数±标准差
      bmi ~ "{median} ({p25}, {p75})",      # BMI：偏态 → 中位数(四分位数间距)
      all_categorical() ~ "{n} ({p}%)"      # 所有分类变量 → 例数(百分比)
    ),
    # 统一小数位数
    digits = list(
      age ~ 1,
      bmi ~ 1,
      all_categorical() ~ 1
    ),
    # 变量中文标签，表格直接可用
    label = list(
      chd ~ "冠心病发病情况",
      age ~ "年龄(岁)",
      bmi ~ "身体质量指数(kg/m²)",
      age_group ~ "年龄分组",
      bmi_group ~ "BMI分组",
      smoking ~ "吸烟状态",
      education ~ "教育程度",
      alcohol ~ "饮酒状态"
    )
  ) %>%
  
  add_n() %>%  # 添加总样本量列
  
  # 核心：手动指定每个变量的检验方法
  add_p(
    test = list(
      age ~ "t.test",                    # 正态连续变量 → 两独立样本t检验
      bmi ~ "wilcox.test",               # 偏态连续变量 → Wilcoxon秩和检验
      all_categorical() ~ "chisq.test",  # 二分类/无序分类 → 卡方检验
      c(age_group, bmi_group, education) ~ "wilcox.test"  # 有序分类 → 秩和检验
    ),
    pvalue_fun = ~ style_pvalue(., digits = 3)  # P值统一保留3位小数，<0.001自动显示
  ) %>%
  
  add_stat_label() %>%  # 增加“检验方法”列，表格自带方法说明
  
  # 美化表头，全部替换为规范中文
  modify_header(
    label = "**变量**",
    stat_0 = "**总人群**",
    stat_1 = "**无GDM病史**",
    stat_2 = "**有GDM病史**",
    p.value = "**P值**",
    stat_label = "**检验方法**"
  ) %>%
  modify_caption("**表2 GDM组与非GDM组研究对象基线特征对比**")

# 在右侧Viewer面板显示完整表格
print(tbl_baseline_p)

# ==============================================
# 步骤4：导出为Word文档
# ==============================================
tbl_baseline_p %>%
  as_flex_table() %>%
  save_as_docx(path = "./output/表2_GDM分组基线对比_带P值.docx")

cat("\n✅ 带P值的基线表生成完成，已导出至output文件夹\n")
