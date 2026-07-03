library(dplyr)
library(ggplot2)

df_2017_2020 <- read.csv(file.choose())
df_2021_2023 <- read.csv(file.choose())

n_raw_2017 <- nrow(df_2017_2020)
n_raw_2021 <- nrow(df_2021_2023)
n_raw_total <- n_raw_2017 + n_raw_2021

df_2017_2020$period <- "2017-2020"
df_2021_2023$period <- "2021-2023"
df_all <- bind_rows(df_2017_2020, df_2021_2023)

df_clean <- df_all %>%
  filter(!is.na(LBXHA) & LBXHA %in% c(1, 2)) %>%
  filter(RIDAGEYR >= 18) %>%
  filter(!is.na(RIAGENDR) & !is.na(DMDEDUC2) & !is.na(RIDRETH3))

n_clean_total <- nrow(df_clean)
n_clean_2017 <- nrow(df_clean[df_clean$period=="2017-2020",])
n_clean_2021 <- nrow(df_clean[df_clean$period=="2021-2023",])

cat("\n===== 数据清洗前后样本量 =====\n")
cat("原始 2017-2020：", n_raw_2017, "\n")
cat("原始 2021-2023：", n_raw_2021, "\n")
cat("原始总计：", n_raw_total, "\n")
cat("清洗后总计：", n_clean_total, "\n")
cat("清洗后 2017-2020：", n_clean_2017, "\n")
cat("清洗后 2021-2023：", n_clean_2021, "\n")

df_clean <- df_clean %>%
  mutate(age_group = cut(RIDAGEYR, breaks = c(18,45,65,Inf), labels = c("18-44","45-64","65+"), right=FALSE)) %>%
  mutate(RIDRETH3 = case_when(RIDRETH3==6~5, RIDRETH3==7~6, TRUE~RIDRETH3)) %>%
  mutate(RIAGENDR = factor(RIAGENDR, levels=c(1,2), labels=c("Male","Female")),
         DMDEDUC2 = factor(DMDEDUC2, levels=1:5, labels=c("Primary","Middle","High","College","University")),
         RIDRETH3 = factor(RIDRETH3, levels=1:6, labels=c("Mexican","Other Hispanic","White","Black","Asian","Other")),
         LBXHA = factor(LBXHA, levels=c(1,2), labels=c("Positive","Negative")))

cat("\n===== 按时期 + 年龄组分布 =====\n")
print(table(df_clean$period, df_clean$age_group))

cat("\n===== 按时期 + 种族分布 =====\n")
print(table(df_clean$period, df_clean$RIDRETH3))

cat("\n===== 按时期 + 教育水平分布 =====\n")
print(table(df_clean$period, df_clean$DMDEDUC2))

cat("\n===== 按时期 + 性别分布 =====\n")
print(table(df_clean$period, df_clean$RIAGENDR))

cat("\n===== 按时期 + LBXHA结果分布 =====\n")
print(table(df_clean$period, df_clean$LBXHA))

age_rates <- df_clean %>%
  group_by(period, age_group) %>%
  summarise(n=n(), pos=sum(LBXHA=="Positive"), rate=pos/n, .groups="drop")

std_pop <- df_clean %>% count(age_group, name="std_n")

age_std <- age_rates %>%
  left_join(std_pop, by="age_group") %>%
  group_by(period) %>%
  summarise(adjusted_rate = sum(rate*std_n)/sum(std_n)*100, .groups="drop")

raw_rates <- df_clean %>%
  group_by(period) %>%
  summarise(raw_rate = round(mean(LBXHA=="Positive")*100,2), .groups="drop")

final <- left_join(raw_rates, age_std, by="period")
cat("\n===== 原始率 vs 标准化率 =====\n")
print(final)

p1 <- ggplot(df_clean, aes(x=period, fill=LBXHA)) +
  geom_bar(position="stack", width=0.7) +
  labs(x="Period", y="Count", fill="Result") + theme_bw()
print(p1)

p2 <- ggplot(final, aes(x=period, y=adjusted_rate, fill=period)) +
  geom_col(show.legend=F, width=0.7) +
  geom_text(aes(label=round(adjusted_rate,1)), vjust=-0.5, size=5) +
  labs(x="Period", y="Adjusted Positive Rate (%)") + theme_bw()
print(p2)
ggsave("adjusted_comparison.png", p2, width=8, height=5, dpi=300)
