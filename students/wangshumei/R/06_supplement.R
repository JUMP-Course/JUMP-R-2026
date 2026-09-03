#==================================6.补充分析====================================
# 1.比例风险假设
ph_test <- cox.zph(multiv_model)
print(ph_test)

# 可视化PH假设检验结果
par(mfrow = c(2, 2))  # 2x2布局
plot(ph_test)

---------------------------------------------------------------------
  # 2.时间依赖ROC曲线
  library(timeROC)

# 计算多因素模型的线性预测值（风险评分）
df_cox$risk_score <- predict(multiv_model, type = "lp")

# 定义时间点：1年、3年、5年
time_points <- c(365, 1095, 1825)  # 1年=365天, 3年=1095天, 5年=1825天

# 计算时间依赖ROC
roc_result <- timeROC(
  T = df_cox$time,
  delta = df_cox$status,
  marker = df_cox$risk_score,
  cause = 1,  # 事件状态为1（死亡）
  times = time_points,
  iid = TRUE
)

# 输出AUC值
cat("时间依赖ROC的AUC值：\n")
cat(sprintf("1年 AUC = %.3f\n", roc_result$AUC[1]))
cat(sprintf("3年 AUC = %.3f\n", roc_result$AUC[2]))
cat(sprintf("5年 AUC = %.3f\n", roc_result$AUC[3]))

# 绘制ROC曲线
plot(roc_result, time = time_points[1], col = "red", lwd = 2, 
     title = "Time-dependent ROC Curves")
plot(roc_result, time = time_points[2], col = "blue", lwd = 2, add = TRUE)
plot(roc_result, time = time_points[3], col = "green", lwd = 2, add = TRUE)
legend("bottomright", 
       legend = c(sprintf("1年 AUC=%.3f", roc_result$AUC[1]),
                  sprintf("3年 AUC=%.3f", roc_result$AUC[2]),
                  sprintf("5年 AUC=%.3f", roc_result$AUC[3])),
       col = c("red", "blue", "green"), lwd = 2)
