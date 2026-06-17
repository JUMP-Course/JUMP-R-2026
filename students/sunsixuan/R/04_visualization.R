#数据可视化分组
library(ggplot2)
library(tidyr)

# 按年龄+门诊利用汇总
doctor_age <- df_analy %>%
  count(age_group, doctor, name = "例数") %>%
  group_by(age_group) %>%
  mutate(百分比 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(doctor == "是") %>% 
  mutate(service="门诊利用")%>%
  select(age_group,service,百分比)
# 按年龄+住院利用汇总
hospital_age <- df_analy %>%
  count(age_group, hospital, name = "例数") %>%
  group_by(age_group) %>%
  mutate(百分比 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(hospital == "是") %>% 
  mutate(service="住院利用")%>%
  select(age_group,service,百分比)
# 合并两个数据框
rate_age <- bind_rows(
  doctor_age %>% mutate(service = "门诊利用") %>% select(age_group, service, 百分比),
  hospital_age %>% mutate(service = "住院利用") %>% select(age_group, service, 百分比)
)
# 绘图
ggplot(rate_age, aes(x = age_group, y = 百分比, color = service, group = service)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(x = "年龄分组", y = "利用率（%）", color = "服务类型",
       title = "不同年龄组卫生服务利用率") +
  theme_minimal()

# 按收入五分位分组+门诊汇总
income_doctor <- df_analy %>%
  count(consume_group, doctor, name = "例数") %>%
  group_by(consume_group) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(doctor == "是") %>%
  mutate(service = "门诊利用") %>%
  select(consume_group, service, 利用率)
# 按收入五分位分组+住院汇总
income_hospital <- df_analy %>%
  count(consume_group, hospital, name = "例数") %>%
  group_by(consume_group) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(hospital == "是") %>%
  mutate(service = "住院利用") %>%
  select(consume_group, service, 利用率)
# 合并绘图数据框
income_plot <- bind_rows(income_doctor, income_hospital)
#绘图
ggplot(income_plot, aes(x = factor(consume_group), y = 利用率)) +
  geom_line(aes(color = service, group = service), linewidth = 1.2) +
  geom_point(aes(color = service), size = 3.5) +
  geom_text(aes(label = paste0(利用率, "%"), color = service), vjust = -0.6, size = 3) +
  labs(
    title = "不同家庭年人均消费五分位卫生服务利用率",
    x = "家庭年人均消费五分位",
    y = "卫生服务利用率（%）",
    color = "服务类型"
  ) +
  scale_color_manual(values = c("#d62728", "#1f77b4"), labels = c("门诊利用", "住院利用")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  )

#按慢性病数量+门诊汇总
chronic_doctor <- df_analy %>%
  count(chronic_group, doctor, name = "例数") %>%
  group_by(chronic_group) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(doctor == "是") %>%
  mutate(service = "门诊利用") %>%
  select(chronic_group, service, 利用率)
#按慢性病数量+住院汇总
chronic_hospital <- df_analy %>%
  count(chronic_group, hospital, name = "例数") %>%
  group_by(chronic_group) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(hospital == "是") %>%
  mutate(service = "住院利用") %>%
  select(chronic_group, service, 利用率)
# 合并绘图数据，清除缺失空行
chronic_plot <- bind_rows(chronic_doctor, chronic_hospital) %>% drop_na()
#绘图
ggplot(chronic_plot, aes(x = factor(chronic_group), y = 利用率)) +
  geom_line(aes(color = service, group = service), linewidth = 1.2) +
  geom_point(aes(color = service), size = 3.5) +
  geom_text(aes(label = paste0(利用率, "%"), color = service), vjust = -0.8, size = 3) +
  labs(
    title = "不同慢性病数量卫生服务利用率",
    x = "患慢性病数量",
    y = "卫生服务利用率（%）",
    color = "服务类型"
  ) +
  scale_color_manual(
    values = c("#d62728", "#1f77b4"),
    labels = c("门诊利用", "住院利用")
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#性别+门诊汇总
gender_doctor <- df_analy %>%
  count(gender, doctor, name = "例数") %>%
  group_by(gender) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(doctor == "是") %>%
  mutate(service = "门诊利用") %>%
  select(gender, service, 利用率)
# 性别+住院汇总
gender_hospital <- df_analy %>%
  count(gender, hospital, name = "例数") %>%
  group_by(gender) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(hospital == "是") %>%
  mutate(service = "住院利用") %>%
  select(gender, service, 利用率)
# 合并数据
gender_plot <- bind_rows(gender_doctor, gender_hospital)
#绘图
ggplot(gender_plot, aes(x = gender, y = 利用率, fill = service)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(利用率, "%")), position = position_dodge(0.7), vjust = -0.3, size = 3) +
  labs(
    title = "不同性别老年人卫生服务利用率",
    x = "性别",
    y = "卫生服务利用率（%）",
    fill = "服务类型"
  ) +
  scale_fill_manual(
    values = c("#d62728", "#1f77b4"),
    labels = c("门诊利用", "住院利用")
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#城乡+门诊汇总
urban_doctor <- df_analy %>%
  count(rural, doctor, name = "例数") %>%
  group_by(rural) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(doctor == "是") %>%
  mutate(service = "门诊利用") %>%
  select(rural, service, 利用率)
#城乡+住院汇总
urban_hospital <- df_analy %>%
  count(rural, hospital, name = "例数") %>%
  group_by(rural) %>%
  mutate(利用率 = round(例数 / sum(例数) * 100, 2)) %>%
  filter(hospital == "是") %>%
  mutate(service = "住院利用") %>%
  select(rural, service, 利用率)
# 合并数据集
urban_plot <- bind_rows(urban_doctor, urban_hospital)
#绘图
ggplot(urban_plot, aes(x = rural, y = 利用率, fill = service)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(利用率, "%")), position = position_dodge(0.7), vjust = -0.3, size = 3) +
  labs(
    title = "不同城乡居住地老年人卫生服务利用率",
    x = "居住地类型",
    y = "卫生服务利用率（%）",
    fill = "服务类型"
  ) +
  scale_fill_manual(
    values = c("#d62728", "#1f77b4"),
    labels = c("门诊利用", "住院利用")
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))