df_2018<-subset(charls,iwy==2018&age>=65)#保留2018年且年龄大于等于65岁的调查对象

chronic_vars <- c(
  "hibpe",    # 高血压
  "diabe",    # 糖尿病
  "cancre",   # 癌症
  "lunge",    # 肺病
  "hearte",   # 心脏病
  "stroke",   # 中风
  "psyche",   # 精神疾病
  "arthre",   # 关节炎
  "dyslipe",  # 血脂异常
  "livere",   # 肝脏疾病
  "kidneye",  # 肾脏疾病
  "digeste",  # 胃病
  "asthmae",  # 哮喘病
  "memrye"    # 记忆疾病
)
df_2018$chronic_count <- rowSums(df_2018[, chronic_vars], na.rm = TRUE)# 生成慢性病数量变量 chronic_count
df_2018$chronic_group <- cut(
  df_2018$chronic_count,
  breaks = c(-Inf, 0, 1, 2, 3, 4, Inf),  # 分界点
  labels = c("0种", "1种", "2种", "3种", "4种", "5种及以上"),  # 对应标签
  right = TRUE  # 区间为左开右闭：(-Inf,0], (0,1], ..., (4,Inf)
)#将慢性病数量分为0种，1种，2种,3种,4种,5种及以上
df_2018 <- df_2018 %>%
  mutate(
    age_group = case_when(
      age >= 65 & age < 75 ~ "65-74岁",
      age >= 75 & age < 85 ~ "75-84岁",
      age >= 85 ~ "85岁及以上"
    )
  )#将年龄分为65-74岁，75-84岁，85岁及以上
df_2018 <- df_2018 %>%
  mutate(
    consume_group = cut(
      hhcperc,
      breaks = quantile(hhcperc, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels = c("前20%", "20%-40%", "40%-60%", "60%-80%", "后20%"),
      include.lowest = TRUE,
      right = FALSE
    )
  )#将家庭年人均消费按大小分成五等分组

# 定义全部需要完整无缺失的变量
key_var <- c("doctor","hospital","gender","marry","rural","ins","smoken","age_group","edu","consume_group","srh","chronic_group")
# 删除关键变量含缺失的样本
df_analy <- df_2018 %>% drop_na(all_of(key_var))

# 查看筛选前后样本量
nrow(df_2018)
nrow(df_analy)