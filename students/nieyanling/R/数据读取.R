library(haven)
library(dplyr)
setwd("D:/NHANES-2017-2018")

#1 基础数据
demo   <- read_xpt("DEMO_J.XPT")
bmx    <- read_xpt("BMX_J.XPT")
dr1tot <- read_xpt("DR1TOT_J.XPT")

#2 读取血脂
tc_dat    <- read_xpt("TCHOL_J.XPT")   #LBXTC总胆固醇
tgldl_dat <- read_xpt("TRIGLY_J.XPT")  #LBXTR甘油三酯、LBDLDL低密度
hdl_dat   <- read_xpt("HDL_J.XPT")     #LBDHDD高密度

#合并四项血脂
lipid <- tc_dat %>% select(SEQN, LBXTC) %>%
  inner_join(tgldl_dat %>% select(SEQN, LBXTR, LBDLDL), by = "SEQN") %>%
  inner_join(hdl_dat %>% select(SEQN, LBDHDD), by = "SEQN") %>%
  rename(LBXHD = LBDHDD)

#全天添加糖三分组：低/中/高
sugar_data <- dr1tot[,c("SEQN","DR1TSUGR","DR1TKCAL")]
cut_q <- quantile(sugar_data$DR1TSUGR, probs = c(0,1/3,2/3,1), na.rm=T)
sugar_data$exposure_3 <- cut(sugar_data$DR1TSUGR,
                             breaks = cut_q,
                             labels = c("低添加糖","中添加糖","高添加糖"),
                             include.lowest = TRUE)
sugar_data$exposure_3 <- factor(sugar_data$exposure_3,levels=c("低添加糖","中添加糖","高添加糖"))
#数据合并+筛选≥18岁+剔除缺失
df_all <- inner_join(demo,bmx,by="SEQN") %>% inner_join(sugar_data,by="SEQN")
df <- inner_join(df_all,lipid,by="SEQN")

keep_var = c("SEQN","exposure_3","RIDAGEYR","RIAGENDR","RIDRETH1","DMDEDUC2","INDFMPIR",
             "BMXBMI","DR1TKCAL","DR1TSUGR","LBXTC","LBXTR","LBXHD","LBDLDL")
df_final = subset(df, RIDAGEYR>=18)
df_final = na.omit(df_final[,keep_var])
