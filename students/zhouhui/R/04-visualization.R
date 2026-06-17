library(tidyverse)
setwd("D:/ukb-jump/gdm-chd-ukb")
df_clean <- readRDS("./data/ukb_cleaned_analysis_final.rds")
# 查看chd的变量类型和所有类别名称
str(df_clean$chd)
# 统计每个类别的样本量，确认到底有多少病例
table(df_clean$chd)
# ========== 图1：GDM与非GDM人群冠心病粗患病率对比柱状图 ==========
p1 <- df_clean %>%
  group_by(gdm) %>%
  # 因子转数值后减1，得到0=未患、1=患病，再计算患病率百分比
  summarise(chd_rate = mean(as.numeric(chd) - 1) * 100) %>%
  ggplot(aes(x = gdm, y = chd_rate, fill = gdm)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("#E64B35", "#00A087")) +
  labs(
    x = "妊娠期糖尿病患病情况", 
    y = "冠心病患病率(%)", 
    title = "GDM与非GDM人群冠心病粗患病率对比"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

print(p1)
ggsave("./figures/gdm_chd_rate.png", p1, width = 6, height = 4, dpi = 300)

# ========== 图2：GDM组与非GDM组年龄分布分面直方图 ==========
p2_faceted <- ggplot(df_clean, aes(x = age, fill = gdm)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  scale_fill_manual(values = c("#E64B35", "#00A087")) +
  labs(
    x = "入组年龄(岁)", 
    y = "人数", 
    fill = "GDM患病情况",
    title = "GDM组与非GDM组年龄分布对比"
  ) +
  facet_wrap(~ gdm, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

print(p2_faceted)
ggsave("./figures/age_dist.png", p2_faceted, width = 6, height = 4, dpi = 300)

cat("\n✅ 两张图绘制完成，已保存至 figures 文件夹\n")
