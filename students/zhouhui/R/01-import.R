#install.packages(c("tidyverse", "skimr", "naniar"))
library(tidyverse)
library(skimr)
library(naniar)
df <- read_rds("./date/test.rds")
dim(df)
names(df)
df$gdm <- ifelse(!is.na(df$GDM_epistart), 1, 0)
df$chd <- ifelse(!is.na(df$CHD_epistart), 1, 0)
table(df$gdm)
prop.table(table(df$gdm)) * 100
table(df$chd)
prop.table(table(df$chd)) * 100
table(df$chd, df$gdm)
miss_var_summary(df %>% select(gdm, chd, age, sex, bmi))
p1 <- df %>%
  group_by(gdm) %>%
  summarise(chd_rate = mean(chd, na.rm = TRUE) * 100) %>%
  mutate(gdm = factor(gdm, labels = c("无GDM", "有GDM"))) %>%
  ggplot(aes(x = gdm, y = chd_rate, fill = gdm)) +
  geom_col(width = 0.6) +
  labs(x = "妊娠期糖尿病患病情况", y = "冠心病患病率(%)", title = "GDM与非GDM人群冠心病粗患病率对比") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("./figures/gdm_chd_rate.png", p1, width = 6, height = 4, dpi = 300)
p2 <- ggplot(df, aes(x = age, fill = factor(gdm))) +
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(x = "入组年龄(岁)", y = "人数", fill = "GDM患病") +
  theme_bw()
ggsave("./figures/age_dist.png", p2, width = 6, height = 4, dpi = 300)
install.packages("rmarkdown")
