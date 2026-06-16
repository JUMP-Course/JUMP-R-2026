install.packages("gtsummary")
install.packages("flextable")
library(survey)
library(gtsummary)
library(dplyr)
library(flextable)

# 加权
svy_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~wt_6yr,
  data = data_final,
  nest = TRUE
)

#表格
tbl1 <- tbl_svysummary(
  svy_design,
  by = hashimoto,
  include = c(RIDAGEYR, BMXBMI, INDFMPIR, URXUCD_cr,
              sex, race, education, smoke, hypertension),
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous() ~ 1,
    all_categorical() ~ c(0, 1)
  ),
  label = list(
    RIDAGEYR ~ "年龄 (岁)",
    BMXBMI ~ "身体质量指数 (kg/m²)",
    INDFMPIR ~ "贫困收入比",
    URXUCD_cr ~ "尿镉 (μg/g 肌酐)",
    sex ~ "性别",
    race ~ "种族",
    education ~ "教育程度",
    smoke ~ "吸烟状况",
    hypertension ~ "高血压"
  )
) %>%
  modify_header(all_stat_cols() ~ "**{level} (加权 n = {n})**") %>%
  modify_caption("表1. 研究人群加权基线特征 (NHANES 2007-2012)") %>%
  bold_labels()

tbl1

# 导出 
tbl1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Table1_加权基线表.docx")
