#研究问题：高血压患病率是否随BMI升高而增加？
# 计算各BMI组的患病率及标准误（用于添加误差线）
bmi_plot_data <- analysis_data %>%
  dplyr::group_by(bmi_cat) %>%  #按BMI分组（正常/超重/肥胖），分成3个小组
  dplyr::summarise(
    n = n(),
    prevalence = mean(hypertension == 1, na.rm = TRUE) * 100,
    se = sqrt(prevalence * (100 - prevalence) / n)  # 标准误
  )

# 确保BMI分组顺序正确
bmi_plot_data$bmi_cat <- factor(bmi_plot_data$bmi_cat, 
                                levels = c("Normal weight", "Overweight", "Obese"))

# 绘制柱状图
p1 <- ggplot(bmi_plot_data, aes(x = bmi_cat, y = prevalence, fill = bmi_cat)) +
  geom_col(width = 0.6, color = "black", size = 0.8) +  #边框粗细
  geom_errorbar(aes(ymin = prevalence - 1.96 * se, 
                    ymax = prevalence + 1.96 * se),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = paste0(round(prevalence, 1), "%")),  #在图上添加文字
            vjust = -1.5, size = 5, fontface = "bold") +  #文字在柱子上方1.5个字符高度
  scale_fill_manual(values = c("Normal weight" = "#2E86AB", 
                               "Overweight" = "#A23B72", 
                               "Obese" = "#F18F01")) +
  labs(
    title = "不同BMI分组的高血压患病率比较",
    subtitle = "NHANES 2021-2023 数据",
    x = "BMI分组",
    y = "高血压患病率 (%)",
    caption = "注：误差线表示95%置信区间"
  ) +
  theme_minimal(base_size = 14) +  #使用简洁主题，基础字号14
  theme(
    legend.position = "none",  #	不显示图例（因为柱子颜色一目了然）
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  #主标题居中、加粗、16号字
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),  #去掉X轴网格线（垂直的）
    panel.grid.minor.y = element_blank()
  )

print(p1)