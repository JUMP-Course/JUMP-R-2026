#7. 验证睡眠时间与高血压是否呈“V”型（或U型）关系---------------------------------------

##7.1 按原始睡眠时间绘制患病率曲线======
data$hypertension_num <- ifelse(data$hypertension == "Yes", 1, 0)  #将是否患高血压转换成0/1
data_filtered <- subset(data, sleep_hours >= 2 & sleep_hours <= 14)  #去除睡眠时间＜2和＞14的极端值

ggplot(data_filtered, aes(x = sleep_hours, y = hypertension_num)) +
  geom_smooth(method = "loess", se = TRUE) +   #局部加权拟合曲线，显示置信区间
  labs(x = "Sleep hours per night", 
       y = "Probability of hypertension",
       title = "Smooth curve of hypertension by sleep hours") +
  scale_y_continuous(labels = scales::percent)

##7.2 二次项logistic回归分析验证======

###7.2.1 Model 1（未调整）：只加睡眠时间
data$hypertension_num <- ifelse(data$hypertension == "Yes", 1, 0)  #将是否患高血压转换成0/1
data_filtered <- subset(data, sleep_hours >= 2 & sleep_hours <= 14)   #筛选睡眠时间在 2 到 14 小时之间的样本
model_quad_unadj <- glm(hypertension_num ~ sleep_hours + I(sleep_hours^2), 
                        data = data_filtered, 
                        family = binomial)

summary(model_quad_unadj)    #查看系数、标准误、z 值、p 值  #X_min = -β₁ / (2 × β₂)拐点为6.56
exp(cbind(OR = coef(model_quad_unadj), confint(model_quad_unadj)))  #计算 Odds Ratio（OR）及其 95% 置信区间


###7.2.2 Model 2（完全调整）：睡眠时间+年龄+性别+ 居住地 + 饮酒
model_quad_filtered <- glm(hypertension_num ~ sleep_hours + I(sleep_hours^2) + 
                             xrage + gender + residence + drinking,
                           data = data_filtered,
                           family = binomial)  #因变量：hypertension_num（0/1）#自变量：sleep_hours（线性项） + I(sleep_hours^2)（平方项） + 协变量

summary(model_quad_filtered)  
OR_CI <- exp(cbind(OR = coef(model_quad_filtered), confint(model_quad_filtered)))
print(OR_CI)  

###7.2.3 Model 3（只调整年龄）
model_quad_ageadj <- glm(hypertension_num ~ sleep_hours + I(sleep_hours^2) + xrage,
                         data = data_filtered,
                         family = binomial)

summary(model_quad_ageadj)
OR_CI <- exp(cbind(OR = coef(model_quad_ageadj), confint(model_quad_ageadj)))

#将年龄分组后用model 1再次验证U/V关系
data_filtered$age_group <- ifelse(data_filtered$xrage < 65, "<65", ">=65")
#年龄 < 65 组
model_young <- glm(hypertension_num ~ sleep_hours + I(sleep_hours^2),
                   data = subset(data_filtered, age_group == "<65"),
                   family = binomial)
summary(model_young)     #X_min = -β₁ / (2 × β₂)拐点为6.52
#年龄 >= 65 组
model_old <- glm(hypertension_num ~ sleep_hours + I(sleep_hours^2),
                 data = subset(data_filtered, age_group == ">=65"),
                 family = binomial)
summary(model_old)