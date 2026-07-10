#8.回归分析--------------------------------------------------------------------------

data_filtered$sleep_group <- factor(data_filtered$sleep_group,
                                    levels = c("6-<7", "<6", "7-<8", "8-<9", ">=9"))
##8.1 总人群回归模型======
model_effect_overall <- glm(hypertension ~ sleep_group + xrage + gender + residence + drinking,
                            data = data_filtered, 
                            family = binomial)
summary(model_effect_overall)
exp(cbind(OR = coef(model_effect_overall), confint(model_effect_overall)))

##8.2 按年龄分层分析======

###8.2.1 <65岁人群
model_effect_young <- glm(hypertension ~ sleep_group + gender + residence + drinking,
                          data = subset(data_filtered, age_group == "<65"),
                          family = binomial)
summary(model_effect_young)
exp(cbind(OR = coef(model_effect_young), confint(model_effect_young)))

###8.2.3 >=65岁人群
model_effect_old <- glm(hypertension ~ sleep_group + gender + residence + drinking,
                        data = subset(data_filtered, age_group == ">=65"),
                        family = binomial)
summary(model_effect_old)
exp(cbind(OR = coef(model_effect_old), confint(model_effect_old)))

##8.3 按性别分层分析======

###8.3.1 男性
model_male <- glm(hypertension ~ sleep_group + xrage + residence + drinking,
                  data = subset(data_filtered, gender == "Male"),
                  family = binomial)
summary(model_male)
exp(cbind(OR = coef(model_male), confint(model_male)))

###8.3.2 女性
model_female <- glm(hypertension ~ sleep_group + xrage + residence + drinking,
                    data = subset(data_filtered, gender == "Female"),
                    family = binomial)
summary(model_male)
exp(cbind(OR = coef(model_female), confint(model_female)))

##8.4 绘制森林图======

###8.2.1 整理数据
#总人群
tidy_overall <- model_effect_overall %>%
  broom::tidy( exponentiate = TRUE, conf.int = TRUE) %>%  #从总体模型中提取睡眠分组的系数、OR、CI 和 P 值
  filter(grepl("sleep_group", term)) %>%   #只保留睡眠分组相关的行
  mutate(
    group = case_when(
      term == "sleep_group<6"     ~ "< 6 小时",
      term == "sleep_group7-<8"   ~ "7 - <8 小时", 
      term == "sleep_group8-<9"   ~ "8 - <9 小时",
      term == "sleep_group>=9"    ~ "≥ 9 小时"
    ),    #设置分组标签
    group = factor(group, levels = c("< 6 小时", "7 - <8 小时", "8 - <9 小时", "≥ 9 小时"))
  ) %>%    #将分组因子按从短到长的顺序排列
  select(group, OR = estimate, CI_low = conf.low, CI_high = conf.high, p.value)
mutate(Model = "总体")   # 添加模型标签

#按年龄分层
tidy_young <- tidy(model_effect_young, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("sleep_group", term)) %>%
  mutate(
    group = case_when(
      term == "sleep_group<6"     ~ "< 6 小时",
      term == "sleep_group7-<8"   ~ "7 - <8 小时",
      term == "sleep_group8-<9"   ~ "8 - <9 小时",
      term == "sleep_group>=9"    ~ "≥ 9 小时"
    ),
    group = factor(group, levels = c("< 6 小时", "7 - <8 小时", "8 - <9 小时", "≥ 9 小时"))
  ) %>%
  select(group, OR = estimate, CI_low = conf.low, CI_high = conf.high, p.value) %>%
  mutate(Model = "<65 岁")

tidy_old <- tidy(model_effect_old, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("sleep_group", term)) %>%
  mutate(
    group = case_when(
      term == "sleep_group<6"     ~ "< 6 小时",
      term == "sleep_group7-<8"   ~ "7 - <8 小时",
      term == "sleep_group8-<9"   ~ "8 - <9 小时",
      term == "sleep_group>=9"    ~ "≥ 9 小时"
    ),
    group = factor(group, levels = c("< 6 小时", "7 - <8 小时", "8 - <9 小时", "≥ 9 小时"))
  ) %>%
  select(group, OR = estimate, CI_low = conf.low, CI_high = conf.high, p.value) %>%
  mutate(Model = ">=65 岁")

forest_age <- bind_rows(tidy_young, tidy_old)
forest_age$Model <- factor(forest_age$Model, levels = c("<65 岁", ">=65 岁"))  #合并年龄分层数据


#按性别分层
tidy_male <- tidy(model_male, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("sleep_group", term)) %>%
  mutate(
    group = case_when(
      term == "sleep_group<6"     ~ "< 6 小时",
      term == "sleep_group7-<8"   ~ "7 - <8 小时",
      term == "sleep_group8-<9"   ~ "8 - <9 小时",
      term == "sleep_group>=9"    ~ "≥ 9 小时"
    ),
    group = factor(group, levels = c("< 6 小时", "7 - <8 小时", "8 - <9 小时", "≥ 9 小时"))
  ) %>%
  select(group, OR = estimate, CI_low = conf.low, CI_high = conf.high, p.value) %>%
  mutate(Model = "男性")

tidy_female <- tidy(model_female, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("sleep_group", term)) %>%
  mutate(
    group = case_when(
      term == "sleep_group<6"     ~ "< 6 小时",
      term == "sleep_group7-<8"   ~ "7 - <8 小时",
      term == "sleep_group8-<9"   ~ "8 - <9 小时",
      term == "sleep_group>=9"    ~ "≥ 9 小时"
    ),
    group = factor(group, levels = c("< 6 小时", "7 - <8 小时", "8 - <9 小时", "≥ 9 小时"))
  ) %>%
  select(group, OR = estimate, CI_low = conf.low, CI_high = conf.high, p.value) %>%
  mutate(Model = "女性")

forest_gender <- bind_rows(tidy_male, tidy_female)
forest_gender$Model <- factor(forest_gender$Model, levels = c("男性", "女性"))

# 总体数据单独作为一个数据框（只含总体）
forest_overall <- tidy_overall

###8.2.2 定义绘图函数

plot_forest <- function(data, title, caption = "")  #定义函数，数据框、标题、脚注
{
  dodge <- position_dodge(width = 0.5)   #使亚组载水平方向错开0.5个单位
  
  p <- ggplot(data, aes(x = group, y = OR, ymin = CI_low, ymax = CI_high, color = Model)) +
    geom_errorbar(position = dodge, width = 0.2, linewidth = 0.8) +  #设置误差线
    geom_point(position = dodge, size = 3) +   #设置点（OR的估计值）
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.6) +  #在OR=1设置一条线
    coord_flip() +   #翻转坐标轴，睡眠分组为纵轴，OR为横轴
    scale_y_log10(breaks = c(0.5, 0.7, 1, 1.4, 1.8), 
                  labels = scales::number_format(accuracy = 0.1)) +
    labs(
      title = title,
      x = "睡眠时长分组（参照组：6-<7 小时）",
      y = "比值比 (OR) 及 95% 置信区间",
      color = "亚组",
      caption = caption
    ) 
  return(p)
}

###8.2.3 分别绘图

#总人群森林图（只含一条线）
plot_overall <- plot_forest(
  data = forest_overall,
  title = "总人群睡眠时间与高血压的关联",
  caption = "调整了年龄、性别、居住地和饮酒"
)
print(plot_overall)

#年龄分层森林图（两条线：<65岁和>=65岁）
plot_age <- plot_forest(
  data = forest_age,
  title = "按年龄分层的睡眠-高血压关联",
  caption = "分层模型调整了性别、居住地和饮酒（未调整年龄）"
)
print(plot_age)

#性别分层森林图（两条线：男性和女性）
plot_gender <- plot_forest(
  data = forest_gender,
  title = "按性别分层的睡眠-高血压关联",
  caption = "分层模型调整了年龄、居住地和饮酒（未调整性别）"
)
print(plot_gender)