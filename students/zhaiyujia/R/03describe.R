#6. 描述性统计

##6.1 年龄、睡眠时间、高血压患病情况的统计摘要
summary(data$xrage)
summary(data$sleep_hours)
summary(data$hypertension)

##6.2 按高血压分组进行比较
data%>%
  select(xrage,gender,residence,drinking,sleep_group,hypertension)%>%
  tbl_summary(
    by=hypertension,
    statistic=list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1,  
    label = list(xrage ~ "Age (years)",gender ~ "Sex",residence ~ "Residence",drinking ~ "Alcohol drinking",sleep_group ~ "Sleep hours (categorical)")) %>%
  add_overall() %>%
  add_p() 

##6.3 描述性图形

###6.3.1 年龄分布直方图
ggplot(data, aes(x = xrage)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age distribution of participants", x = "Age (years)", y = "Count")

###6.3.2 睡眠时长分布直方图
ggplot(data, aes(x = sleep_hours)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Sleep hours distribution (binwidth = 1)", 
       x = "Hours per night", y = "Count")

###6.3.3 按高血压分组的睡眠时间箱线图
ggplot(data, aes(x = hypertension, y = sleep_hours, fill = hypertension)) +
  geom_boxplot()+
  labs(title = "Sleep hours by hypertension status", x = "Hypertension", y = "Sleep hours") 

###6.3.4睡眠分组与高血压的堆叠柱状图
group_data <- data %>%
  group_by(sleep_group, hypertension) %>%  
  summarise(count = n(), .groups = "drop") %>% 
  group_by(sleep_group) %>%                  
  mutate(prop = count / sum(count))

ggplot(group_data, aes(x = sleep_group, y = prop, fill = hypertension)) +
  geom_col(position = "stack") +              
  labs(x = "Sleep hours group", 
       y = "Proportion", 
       fill = "Hypertension") +
  scale_y_continuous(labels = scales::percent) 