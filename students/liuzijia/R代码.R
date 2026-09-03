library(tidyverse)

df <- read.csv("C:/Users/hp/Desktop/论文数据/数据库的最终版.csv", fileEncoding = "UTF-8")

colnames(df) <- c(
  "group", "agegroup1", "agegroup2", "age", "gender", "mel1", "mel",
  "sleep_weeknight", "sleep_total_weekday", "sleep_weekend_night", "sleep_total_weekend",
  "CSHQ_total", "bedtime_resistance", "sleep_onset_delay", "sleep_duration",
  "sleep_anxiety", "night_waking", "parasomnia", "sleep_breathing", "daytime_sleepiness",
  "filter", "bedtime_abnormal", "onset_abnormal", "duration_abnormal",
  "anxiety_abnormal", "nightwaking_abnormal", "parasomnia_abnormal",
  "breathing_abnormal", "daytime_abnormal", "sleep_standard"
)

df <- df %>% select(-mel1, -filter) %>%
  mutate(
    group_f = factor(group, 1:2, c("伴睡眠障碍","不伴睡眠障碍")),
    gender_f = factor(gender, 1:2, c("男","女")),
    agegroup_f = factor(agegroup1, 1:4, c("2-3岁","4-5岁","6-7岁","8-15岁")),
    sleep_standard_f = factor(sleep_standard, 1:0, c("达标","未达标"))
  )

dims <- c("bedtime_resistance","sleep_onset_delay","sleep_duration","sleep_anxiety",
          "night_waking","parasomnia","sleep_breathing","daytime_sleepiness")
dims_cn <- c("就寝习惯","入睡潜伏","睡眠持续时间","睡眠焦虑","夜醒","异态睡眠","睡眠呼吸障碍","白天嗜睡")
abn <- c("bedtime_abnormal","onset_abnormal","duration_abnormal","anxiety_abnormal",
         "nightwaking_abnormal","parasomnia_abnormal","breathing_abnormal","daytime_abnormal")
sleep_vars <- c("sleep_weeknight","sleep_total_weekday","sleep_weekend_night","sleep_total_weekend")
sleep_cn <- c("工作日夜晚睡眠时长","工作日总睡眠时长","周末夜晚睡眠时长","周末总睡眠时长")

desc <- function(x) data.frame(中位数=round(median(x,na.rm=T),2), IQR=round(IQR(x,na.rm=T),2),
                               最小值=round(min(x,na.rm=T),2), 最大值=round(max(x,na.rm=T),2))

mw <- function(x,g){
  t <- wilcox.test(x~g, exact=F)
  U <- as.numeric(t$statistic)
  n1 <- sum(g==levels(g)[1]); n2 <- sum(g==levels(g)[2])
  Z <- (U - n1*n2/2) / sqrt(n1*n2*(n1+n2+1)/12)
  c(U=U, Z=Z, P=t$p.value)
}

kw <- function(x,g){
  t <- kruskal.test(x~g)
  c(H=as.numeric(t$statistic), P=t$p.value)
}

cf <- function(tab){
  if(any(chisq.test(tab)$expected<5)) p <- fisher.test(tab)$p.value else p <- chisq.test(tab)$p.value
  p
}

# 表3-1
tab1 <- map2_dfr(dims, dims_cn, function(col,name) desc(df[[col]]) %>% mutate(维度=name,.before=1))
print(tab1)

# 表3-2 
tab2 <- map2_dfr(dims, dims_cn, function(col,name){
  rank_vals <- rank(df[[col]], na.last="keep", ties.method="average")
  rk <- tapply(rank_vals, df$gender_f, mean, na.rm=TRUE)
  res <- mw(df[[col]], df$gender_f)
  data.frame(维度=name, 男秩平均=round(rk["男"],2), 女秩平均=round(rk["女"],2),
             U=round(res["U"],2), Z=round(res["Z"],3), P=round(res["P"],4))
})
print(tab2)

# 表3-3
tab3 <- map2_dfr(dims, dims_cn, function(col,name){
  res <- kw(df[[col]], df$agegroup_f)
  data.frame(维度=name, H=round(res["H"],3), P=round(res["P"],4))
})
print(tab3)

# 表3-4
tab4 <- map2_dfr(abn, dims_cn, function(col,name){
  tab <- table(df$gender_f, df[[col]])
  if(ncol(tab)!=2) return(data.frame(维度=name,男率=NA,女率=NA,P=NA))
  n_m <- rowSums(tab)["男"]; n_f <- rowSums(tab)["女"]
  ab_m <- tab["男","1"]; ab_f <- tab["女","1"]
  data.frame(维度=name, 男异常率=sprintf("%.1f (%.0f/%.0f)", ab_m/n_m*100, ab_m, n_m),
             女异常率=sprintf("%.1f (%.0f/%.0f)", ab_f/n_f*100, ab_f, n_f), P=round(cf(tab),4))
})
print(tab4)

# 表3-5
tab5 <- map2_dfr(abn, dims_cn, function(col,name){
  tab <- table(df$agegroup_f, df[[col]])
  idx <- which(colnames(tab)=="1")
  ab_cnt <- if(length(idx)==0) rep(0,nrow(tab)) else tab[,idx]
  tot <- rowSums(tab)
  rat <- ab_cnt/tot*100
  ages <- c("2-3岁","4-5岁","6-7岁","8-15岁")
  out <- data.frame(维度=name)
  for(i in 1:4) out[[ages[i]]] <- sprintf("%.1f (%.0f/%.0f)", rat[i], ab_cnt[i], tot[i])
  out$P <- round(fisher.test(tab)$p.value,4)
  out
})
print(tab5)

# 表3-6
tab6 <- map2_dfr(sleep_vars, sleep_cn, function(col,name) desc(df[[col]]) %>% mutate(睡眠时长指标=name,.before=1))
print(tab6)

# 表3-7
tab7 <- map2_dfr(sleep_vars, sleep_cn, function(col,name){
  res <- mw(df[[col]], df$gender_f)
  data.frame(指标=name, U=round(res["U"],2), Z=round(res["Z"],3), P=round(res["P"],4))
})
print(tab7)

# 表3-8
tab8 <- map2_dfr(sleep_vars, sleep_cn, function(col,name){
  res <- kw(df[[col]], df$agegroup_f)
  data.frame(指标=name, H=round(res["H"],3), P=round(res["P"],4))
})
print(tab8)

# 表3-9
tab_g <- table(df$gender_f, df$sleep_standard_f)
tab9 <- data.frame(
  性别=c("男","女"),
  未达标=c(tab_g["男","未达标"], tab_g["女","未达标"]),
  达标=c(tab_g["男","达标"], tab_g["女","达标"]),
  达标率=sprintf("%.1f%%", c(tab_g["男","达标"]/sum(tab_g["男",])*100, tab_g["女","达标"]/sum(tab_g["女",])*100)),
  P=round(cf(tab_g),4)
)
print(tab9)

# 表3-10
tab_a <- table(df$agegroup_f, df$sleep_standard_f)
tab10 <- data.frame(
  年龄组=rownames(tab_a),
  未达标=tab_a[,"未达标"], 达标=tab_a[,"达标"],
  达标率=sprintf("%.1f%%", tab_a[,"达标"]/rowSums(tab_a)*100),
  P=round(cf(tab_a),4)
)
print(tab10)

# 表3-11
comp_vars <- c(dims, "mel")
comp_cn <- c(dims_cn, "血清褪黑素(pg/mL)")
tab11 <- map2_dfr(comp_vars, comp_cn, function(col,name){
  med_iqr <- df %>% group_by(group_f) %>%
    summarise(m=round(median(!!sym(col),na.rm=T),1), i=round(IQR(!!sym(col),na.rm=T),1)) %>%
    mutate(desc=paste0(m,"(",i,")")) %>% pull(desc)
  res <- mw(df[[col]], df$group_f)
  data.frame(变量=name, 伴障碍组=med_iqr[1], 不伴障碍组=med_iqr[2],
             U=round(res["U"],2), Z=round(res["Z"],3), P=round(res["P"],4))
})
print(tab11)

# 表3-12
corr_vars <- c("CSHQ_total", dims, sleep_vars)
corr_cn <- c("CSHQ总分", dims_cn, sleep_cn)
tab12 <- map2_dfr(corr_vars, corr_cn, function(col,name){
  ct <- cor.test(df$mel, df[[col]], method="spearman", exact=F)
  data.frame(参数=name, r=round(ct$estimate,3), P=round(ct$p.value,4))
})
print(tab12)

# 表3-13
df_age <- df %>% filter(!is.na(agegroup_f))
age_lab <- c("2-3岁","4-5岁","6-7岁","8-15岁")
tab13 <- map_dfr(age_lab, function(ag){
  sub <- df_age %>% filter(agegroup_f==ag)
  if(nrow(sub)>=3){
    ct <- cor.test(sub$mel, sub$CSHQ_total, method="spearman", exact=F)
    data.frame(年龄组=ag, n=nrow(sub), r=round(ct$estimate,3), P=round(ct$p.value,4))
  } else data.frame(年龄组=ag, n=nrow(sub), r=NA, P=NA)
})
print(tab13)

# 表3-14
tab14_raw <- df_age %>%
  select(agegroup_f, mel, all_of(dims)) %>%
  pivot_longer(cols=all_of(dims), names_to="维度", values_to="得分") %>%
  group_by(agegroup_f, 维度) %>%
  summarise(n=n(), r=if(n()>=3) cor(mel,得分,method="spearman") else NA,
            p=if(n()>=3) cor.test(mel,得分,method="spearman",exact=F)$p.value else NA, .groups="drop") %>%
  mutate(r=round(r,3), p=round(p,4))

tab14 <- tab14_raw %>%
  select(维度, agegroup_f, r, p) %>%
  pivot_wider(names_from=agegroup_f, values_from=c(r,p), names_sep="_") %>%
  mutate(维度=factor(维度, levels=dims, labels=dims_cn)) %>% arrange(维度)
print(tab14)
