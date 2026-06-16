library(dplyr)
library(ggplot2)
library(scales)  

analysis_data <- read.csv(
  "D:/JUMP-R-2026/students/niujiayi/doc/女性饮酒乳腺癌_最终清洗数据.csv",
  stringsAsFactors = TRUE
)
analysis_data <- analysis_data %>%
  mutate(
    drink_group = case_when(
      ALQ121 == "从不饮酒" ~ "从不饮酒",
      ALQ121 %in% c("过去一年1-2次", "过去一年3-6次", "过去一年7-11次", 
                    "每月1次", "每月2-3次") ~ "少量饮酒",
      ALQ121 %in% c("每周1次", "每周2次", "每周3-4次") ~ "中等饮酒",
      ALQ121 %in% c("几乎每天饮酒", "每天饮酒") ~ "频繁饮酒"
    ) %>% 
      factor(levels = c("从不饮酒", "少量饮酒", "中等饮酒", "频繁饮酒"))
  )
analysis_data$breast_cancer <- factor(
  analysis_data$breast_cancer,
  levels = c("未患乳腺癌", "患乳腺癌")
)
p1 <- ggplot(analysis_data, aes(x = RIDAGEYR, fill = breast_cancer)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "不同乳腺癌患病状态女性的年龄分布",
    x = "年龄（岁）",
    y = "密度",
    fill = "患病状态"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

print(p1)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图1_年龄分布密度图.png",
  plot = p1,
  width = 8, height = 5, dpi = 300
)

plot_data <- analysis_data %>% filter(!is.na(drink_group))
plot_data$drink_group <- factor(
  plot_data$drink_group,
  levels = c("从不饮酒", "少量饮酒", "中等饮酒", "频繁饮酒")
)
p2 <- ggplot(plot_data, aes(x = breast_cancer, fill = drink_group)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    title = "不同乳腺癌患病状态女性的饮酒频次构成",
    x = "患病状态",
    y = "组内占比",
    fill = "饮酒频次分组"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )
print(p2)
ggsave(
  filename = "D:/JUMP-R-2026/students/niujiayi/doc/图2_饮酒频次构成比.png",
  plot = p2,
  width = 8,
  height = 6,
  dpi = 300
)