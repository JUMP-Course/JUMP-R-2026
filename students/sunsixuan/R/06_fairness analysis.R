
install.packages("rineq")
library(rineq)
library(dplyr)
#门诊集中指数
df_analy$doctor_num <- as.numeric(df_analy$doctor) - 1#将因子转为数值：“否”=1→0，“是”=2→1
ci_doc1<-ci(
  ineqvar = df_analy$hhcperc,
  outcome = df_analy$doctor_num,
  type = "CI"
)
summary(ci_doc1)
#门诊"Erreygers"校正指数
y_bar <- mean(df_analy$doctor_num)
E_ci <- (4 * y_bar / (1 - y_bar)) * 0.09445796
E_ci

#门诊集中曲线图
# 1. 按消费从小到大排序数据
df_sort <- df_analy[order(df_analy$hhcperc), ]
# 2. 计算累计人群占比（横轴x）
df_sort$cum_pop <- seq(1:nrow(df_sort)) / nrow(df_sort)
# 3. 计算门诊就医累计占比（纵轴y）
df_sort$cum_doc <- cumsum(df_sort$doctor_num) / sum(df_sort$doctor_num)
ggplot() +
  # 集中曲线
  geom_line(data = df_sort, aes(x = cum_pop, y = cum_doc),
            linewidth = 1.2, color = "#2E86AB") +
  # 45°公平对角线
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 1, color = "black") +
  labs(
    x = "累计人口占比",
    y = "累计门诊服务利用占比",
    title = "门诊卫生服务利用集中曲线"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#住院集中指数
df_analy$hospital_num <- as.numeric(df_analy$hospital) - 1#将因子转为数值：“否”=1→0，“是”=2→1
ci_hosp1<-ci(
  ineqvar = df_analy$hhcperc,
  outcome = df_analy$hospital_num,
  type = "CI"
)
summary(ci_hosp1)
#住院"Erreygers"校正指数
y_bar <- mean(df_analy$hospital_num)
E_ci <- (4 * y_bar / (1 - y_bar)) * 0.09349178
E_ci

#住院集中曲线图
# 1. 按消费从小到大排序数据
df_sort <- df_analy[order(df_analy$hhcperc), ]
# 2. 计算累计人群占比（横轴x）
df_sort$cum_pop <- seq(1:nrow(df_sort)) / nrow(df_sort)
# 3.计算住院就医累计占比（纵轴y）
df_sort$cum_hosp <- cumsum(df_sort$hospital_num) / sum(df_sort$hospital_num)
ggplot() +
  # 集中曲线
  geom_line(data = df_sort, aes(x = cum_pop, y = cum_hosp),
            linewidth = 1.2, color = "#2E86AB") +
  # 45°公平对角线
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 1, color = "black") +
  labs(
    x = "累计人口占比",
    y = "累计住院服务利用占比",
    title = "住院卫生服务利用集中曲线"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))