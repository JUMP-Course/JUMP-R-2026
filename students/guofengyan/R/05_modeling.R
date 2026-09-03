# ============================================
# TCGA-PAAD胰腺癌预后预测模型
# ============================================

# ---- 加载工具包 ----
library(dplyr)       # 数据清洗
library(survival)    # 生存分析核心包：Cox回归、生存结局
library(survminer)   # 生存分析画图辅助
library(rms)         # 列线图、校准曲线、C-index
library(timeROC)     # 时间依赖ROC
library(broom)       # 整理回归结果为干净表格
library(ggplot2)     # 森林图、统计图

# ============================================================
# 第一部分：数据导入
# ============================================================

# ---- 1.1 设置工作目录 ----
setwd("C:/Users/86156/Desktop/")

# ---- 1.2 读取原始数据 ----
clin_raw <- read.delim("paad_tcga_gdc_clinical_data.tsv")  # 临床数据
mut_raw  <- read.delim("mutations.txt")                     # 基因突变数据

# ---- 1.3 查看原始数据情况 ----
cat("原始数据规模：\n")
cat("  临床数据：", nrow(clin_raw), "行 ×", ncol(clin_raw), "列\n")
cat("  突变数据：", nrow(mut_raw),  "行 ×", ncol(mut_raw),  "列\n")


# ============================================================
# 第二部分：数据清洗整理
# ============================================================

# ---- 2.1 临床数据整理 ----
df_clin <- clin_raw %>%
  mutate(
    ID     = substr(Patient.ID, 1, 12),
    time   = as.numeric(Overall.Survival..Months.),
    status = as.numeric(Overall.Survival.Status == "1:DECEASED"),
    Age    = as.numeric(Diagnosis.Age),
    Sex    = factor(Sex, levels = c("Male", "Female")),
    N      = AJCC.Pathologic.N.Stage
  ) %>%
  select(ID, time, status, Age, Sex, N)  # 保留分析所需列

# ---- 2.2 临床数据清洗 ----
df_clin_clean <- df_clin %>%
  filter(
    !is.na(time) & !is.na(Age) & !is.na(status) & !is.na(Sex) & !is.na(N),
    Age >= 18 & Age <= 100,
    time >= 0,
    N != "" & N != "NX"
  ) %>%
  mutate(
    # 合并N1b到N1
    N = factor(ifelse(N == "N1b", "N1", N), 
               levels = c("N0", "N1"))
  )

cat("\n临床数据清洗后：", nrow(df_clin_clean), "例\n")

# ---- 2.3 突变数据整理 ----
mut <- mut_raw %>%
  mutate(
    ID   = substr(SAMPLE_ID, 1, 12),
    KRAS = ifelse(KRAS == "WT", "Wild-type", "Mutant")
  )

# ---- 2.4 突变数据去重（同一患者只要有突变即标记为Mutant）----
kras <- aggregate(KRAS ~ ID, mut, FUN = function(x) {
  ifelse(any(x != "Wild-type"), "Mutant", "Wild-type")
})

# ---- 2.5 突变数据清洗 ----
kras_clean <- kras %>%
  filter(!is.na(KRAS)) %>%
  mutate(
    KRAS = factor(KRAS, levels = c("Wild-type", "Mutant"))
  )

cat("突变数据清洗后：", nrow(kras_clean), "例\n")
cat("\nKRAS突变分布：\n"); print(table(kras_clean$KRAS))

# ---- 2.6 合并两份数据（仅保留双方匹配的患者）----
df <- merge(df_clin_clean, kras_clean, by = "ID", all = FALSE)

cat("\n合并后最终数据集：", nrow(df), "例\n")
cat("N分期：");  print(table(df$N))
cat("KRAS：");   print(table(df$KRAS))
cat("性别：");   print(table(df$Sex))

# ---- 2.7 样本基线特征----
library(gtsummary)

# 生成基线表：连续变量展示中位数(IQR)，分类变量n(%)
table1 <- df %>%
  select(Age, Sex, N, KRAS) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",  # 连续年龄：中位数(四分位距)
      all_categorical() ~ "{n} ({p}%)"               # 分类变量：例数(百分比)
    ),
    label = list(
      Age ~ "Age, years",
      Sex ~ "Gender",
      N ~ "AJCC Pathologic N Stage",
      KRAS ~ "KRAS Gene Status"
    ),
    missing = "no"  # 已提前清洗无缺失，不展示缺失值
  ) %>%
  modify_header(label ~ "Clinical characteristic", stat_0 ~ "Total cohort (n = {N})") %>%
  bold_labels()

# 在R控制台/Viewer窗口展示表格
print(table1)

# ============================================================
# 第三部分：数据分析
# ============================================================

# ---- 3.1 单因素Cox回归----
uni_vars <- c("Age", "Sex", "N", "KRAS")

cat("\n========== 单因素Cox回归 ==========\n")
for (v in uni_vars) {
  cat("\n---", v, "---\n")
  fit <- coxph(as.formula(paste("Surv(time, status) ~", v)), data = df)
  print(summary(fit))
}

# ---- 3.2 多因素Cox回归（校正混杂因素）----
cat("\n========== 多因素Cox回归 ==========\n")
multi <- coxph(Surv(time, status) ~ Age + Sex + N + KRAS, data = df)
print(summary(multi))
# 提取HR、95%CI、P值
res <- tidy(multi, exponentiate = TRUE, conf.int = TRUE)
cat("\nHR (95%CI) & P值：\n")
print(res[, c("term", "estimate", "conf.low", "conf.high", "p.value")])

# ---- 3.3 森林图可视化 ----
forest <- data.frame(
  Variable = c("Age (每增加1岁)", 
               "Sex (Female vs Male)", 
               "N1 vs N0", 
               "KRAS Mutant vs Wild-type"),
  HR       = res$estimate,
  Lower    = res$conf.low,
  Upper    = res$conf.high,
  P        = format(res$p.value, digits = 3, scientific = TRUE)
)

ggplot(forest, aes(x = HR, y = reorder(Variable, HR), 
                   xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(height = 0.2, color = "blue") +
  scale_x_continuous(trans = "log10", breaks = c(0.5, 1, 2, 3, 4)) +
  geom_text(aes(label = paste0(round(HR, 2), " (", 
                               round(Lower, 2), "-", 
                               round(Upper, 2), ")  P=", P)), 
            x = max(forest$Upper) * 1.1, hjust = 0, size = 3.5) +
  labs(x = "Hazard Ratio (log scale)", y = "", 
       title = "多因素Cox回归森林图") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---- 3.4 列线图（Nomogram）----
dd <- datadist(df); options(datadist = "dd")

f <- cph(Surv(time, status) ~ Age + Sex + N + KRAS, 
         data = df, x = TRUE, y = TRUE, surv = TRUE, time.inc = 12)

surv <- Survival(f)

nom <- nomogram(f, 
                fun = list(function(x) surv(12, x), 
                           function(x) surv(36, x)),
                funlabel = c("1年生存概率", "3年生存概率"),
                lp = FALSE, maxscale = 100,
                fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9))

plot(nom, lmgp = 0.3, cex.axis = 0.9, cex.var = 1,
     col.grid = gray(c(0.8, 0.95)))
title("TCGA-PAAD胰腺癌预后Nomogram", line = 2, cex.main = 1.2)

