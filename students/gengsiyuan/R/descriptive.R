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



data_final <- data_final %>%
  mutate(hashimoto_grp = factor(hashimoto, 
                                levels = 0:1, 
                                labels = c("桥本阴性", "桥本阳性")))

# 图1
p1 <- ggplot(data_final, aes(x = URXUCD_cr, fill = hashimoto_grp)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 35, color = "black") +
  scale_x_log10() +
  scale_fill_manual(values = c("#1f77b4", "#d95f02")) +
  labs(title = "尿镉浓度在不同桥本状态下的分布",
       x = "尿镉浓度 (μg/g 肌酐, 对数转换)",
       y = "频数", fill = "组别") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "bottom", legend.title = element_blank())

ggsave("Figure1_尿镉分布直方图.png", p1, width = 8, height = 5, dpi = 300)

# 图2
set.seed(123)
p2 <- ggplot(data_final, aes(x = hashimoto_grp, y = URXUCD_cr, fill = hashimoto_grp)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.15, size = 0.6, color = "gray30") +
  scale_y_log10() +
  scale_fill_manual(values = c("#1f77b4", "#d95f02")) +
  labs(title = "桥本阳性与阴性组尿镉浓度比较",
       x = NULL, y = "尿镉浓度 (μg/g 肌酐, 对数转换)") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "none", axis.text.x = element_text(size = 11))

ggsave("Figure2_尿镉组间箱线图.png", p2, width = 6, height = 5, dpi = 300)

print(p1)
print(p2)