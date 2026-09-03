#===========导入文件===========
library(haven)
library(readxl)
library(dplyr)
library(survey)
library(broom)
library(tidyverse)

# 读取2015-2016数据
# 人口学
demo_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DEMO_I.xpt")
#生活方式
smq_1516 <- read_xpt("D:/NHANES数据库/2015-2016/SMQ_I.XPT")
alq_1516 <- read_xpt("D:/NHANES数据库/2015-2016/ALQ_I.XPT")
paq_1516 <- read_xpt("D:/NHANES数据库/2015-2016/PAQ_I.XPT")
# 人体测量
bmx_1516 <- read_xpt("D:/NHANES数据库/2015-2016/BMX_I.XPT")
# 疾病史
bpq_1516 <- read_xpt("D:/NHANES数据库/2015-2016/BPQ_I.XPT")
diq_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DIQ_I.XPT")
tchol_1516 <- read_xpt("D:/NHANES数据库/2015-2016/TCHOL_I.XPT")
# 膳食
dr1tot_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DR1TOT_I.XPT")
dr2tot_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DR2TOT_I.XPT")
dr1iff_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DR1IFF_I.XPT")
dr2iff_1516 <- read_xpt("D:/NHANES数据库/2015-2016/DR2IFF_I.XPT")
fped_dr1_1516 <- read_sas("D:/NHANES数据库/2015-2016/fped_dr1iff_1516.sas7bdat")
fped_dr2_1516 <- read_sas("D:/NHANES数据库/2015-2016/fped_dr2iff_1516.sas7bdat")
fndds_1516 <- read_excel("D:/NHANES数据库/2015-2016/MainFoodDesc.xlsx")
# 读取2017-2018周期数据
# 人口学
demo_1718 <- read_xpt("D:/NHANES数据库/DEMO_J（2017-2018）.xpt")
#生活方式
smq_1718 <- read_xpt("D:/NHANES数据库/SMQ_J（2017-2018）.xpt")
alq_1718 <- read_xpt("D:/NHANES数据库/ALQ_J（2017-2018）.XPT")
paq_1718 <- read_xpt("D:/NHANES数据库/PAQ_J（2017-2018）.XPT")
# 人体测量
bmx_1718 <- read_xpt("D:/NHANES数据库/BMX_J（2017-2018）.XPT")
# 疾病史
bpq_1718 <- read_xpt("D:/NHANES数据库/BPQ_J（2017-2018）.XPT")
diq_1718 <- read_xpt("D:/NHANES数据库/DIQ_J（2017-2018）.XPT")
tchol_1718 <- read_xpt("D:/NHANES数据库/TCHOL_J（2017-2018）.XPT")
# 膳食
dr1tot_1718 <- read_xpt("D:/NHANES数据库/DR1TOT_J（2017-2018）.XPT")
dr2tot_1718 <- read_xpt("D:/NHANES数据库/DR2TOT_J（2017-2018）.XPT")
dr1iff_1718 <- read_xpt("D:/NHANES数据库/DR1IFF_J（2017-2018）.XPT")
dr2iff_1718 <- read_xpt("D:/NHANES数据库/DR2IFF_J（2017-2018）.XPT")
fped_dr1_1718 <- read_sas("D:/NHANES数据库/fped_dr1iff_1718.sas7bdat")
fped_dr2_1718 <- read_sas("D:/NHANES数据库/fped_dr2iff_1718.sas7bdat")
fndds_1718 <- read_excel("D:/NHANES数据库/MainFoodDesc_1718.xlsx")

#合并两个周期
demo <- bind_rows(demo_1516,demo_1718)
smq <- bind_rows(smq_1516,smq_1718)
alq <- bind_rows(alq_1516,alq_1718)
paq <- bind_rows(paq_1516,paq_1718)
bmx <- bind_rows(bmx_1516,bmx_1718)
bpq <- bind_rows(bpq_1516,bpq_1718)
diq <- bind_rows(diq_1516,diq_1718)
tchol <- bind_rows(tchol_1516,tchol_1718)
dr1tot <- bind_rows(dr1tot_1516,dr1tot_1718)
dr2tot <- bind_rows(dr2tot_1516,dr2tot_1718)
dr1iff <- bind_rows(dr1iff_1516,dr1iff_1718)
dr2iff <- bind_rows(dr2iff_1516,dr2iff_1718)
fped_dr1 <- bind_rows(fped_dr1_1516,fped_dr1_1718)
fped_dr2 <- bind_rows(fped_dr2_1516,fped_dr2_1718)
fooddesc <- bind_rows(fndds_1516, fndds_1718) %>%
  distinct(`Food code`, .keep_all = TRUE)

#=============建立食物分类=================
#建立动物性食物分类
fooddesc <- fooddesc %>%
  mutate(
    ELD_group = case_when(
      grepl("BEEF|VEAL|LAMB", toupper(`Main food description`)) ~ "Beef_Lamb",
      grepl("PORK|HAM|BACON", toupper(`Main food description`)) ~ "Pork",
      grepl("CHICKEN|TURKEY|DUCK", toupper(`Main food description`)) ~ "Poultry",
      grepl("FISH|SALMON|TUNA|COD|TROUT", toupper(`Main food description`)) ~ "Fish",
      grepl("EGG", toupper(`Main food description`)) ~ "Egg",
      TRUE ~ NA_character_
    )
  )
#第一天动物性食物摄入
day1 <- fped_dr1 %>%
  left_join(
    fooddesc,
    by=c(
      "DR1IFDCD"="Food code"
    )
  )#把 NHANES 第一天膳食记录数据（fped_dr1和食物描述表（fooddesc）
#通过食物代码关联起来
animal_day1 <- day1 %>%
  group_by(SEQN) %>%
  summarise(
    Beef_Lamb=sum(
      DR1IGRMS[ ELD_group=="Beef_Lamb"],na.rm=T),
    Pork=sum(
      DR1IGRMS[ ELD_group=="Pork"],na.rm=T),
    Poultry=sum(
      DR1IGRMS[ ELD_group=="Poultry"],na.rm=T),
    Fish=sum(
      DR1IGRMS[ ELD_group=="Fish"],na.rm=T),
    Egg=sum(
      DR1IGRMS[ELD_group=="Egg"],na.rm=T)
  )
#第二天动物性食物摄入
day2 <- fped_dr2 %>%
  left_join(
    fooddesc,
    by=c(
      "DR2IFDCD"="Food code"
    )
  )
animal_day2 <- day2 %>%
  group_by(SEQN) %>%
  summarise(
    Beef_Lamb   = sum(ifelse(ELD_group=="Beef_Lamb", DR2IGRMS, 0), na.rm=TRUE),
    Pork        = sum(ifelse(ELD_group=="Pork", DR2IGRMS, 0), na.rm=TRUE),
    Poultry     = sum(ifelse(ELD_group=="Poultry", DR2IGRMS, 0), na.rm=TRUE),
    Fish        = sum(ifelse(ELD_group=="Fish", DR2IGRMS, 0), na.rm=TRUE),
    Egg         = sum(ifelse(ELD_group=="Egg", DR2IGRMS, 0), na.rm=TRUE)
  )
#第一天植物性食物摄入
fped_day1 <- fped_dr1 %>%
  group_by(SEQN) %>%
  summarise(
    Vegetables = sum(DR1I_V_TOTAL, na.rm = TRUE),
    Fruits = sum(DR1I_F_TOTAL, na.rm = TRUE),
    WholeGrains = sum(DR1I_G_WHOLE, na.rm = TRUE),
    Legumes = sum(DR1I_PF_LEGUMES, na.rm = TRUE),
    Nuts = sum(DR1I_PF_NUTSDS, na.rm = TRUE),
    Dairy = sum(DR1I_D_TOTAL, na.rm = TRUE),
    AddedSugar = sum(DR1I_ADD_SUGARS, na.rm = TRUE),
    UnsaturatedOil = sum(DR1I_OILS, na.rm = TRUE),
    Potatoes = sum(DR1I_V_STARCHY_POTATO, na.rm = TRUE)
  )
#第二天植物性食物摄入
fped_day2 <- fped_dr2 %>%
  group_by(SEQN) %>%
  summarise(
    Vegetables = sum(DR2I_V_TOTAL, na.rm = TRUE),
    Fruits = sum(DR2I_F_TOTAL, na.rm = TRUE),
    WholeGrains = sum(DR2I_G_WHOLE, na.rm = TRUE),
    Legumes = sum(DR2I_PF_LEGUMES, na.rm = TRUE),
    Nuts = sum(DR2I_PF_NUTSDS, na.rm = TRUE),
    Dairy = sum(DR2I_D_TOTAL, na.rm = TRUE),
    AddedSugar = sum(DR2I_ADD_SUGARS, na.rm = TRUE),
    UnsaturatedOil = sum(DR2I_OILS, na.rm = TRUE),
    Potatoes = sum(DR2I_V_STARCHY_POTATO, na.rm = TRUE)
  )
#FPED单位换算
fped_day1 <- fped_day1 %>%
  mutate(
    Vegetables=Vegetables*128,
    Fruits=Fruits*150,
    WholeGrains=WholeGrains*28.35,
    Legumes=Legumes*28.35,
    Nuts=Nuts*28.35,
    Dairy=Dairy*245,
    AddedSugar=AddedSugar*4.2,
    Potatoes=Potatoes*128
  )
fped_day2 <- fped_day2 %>%
  mutate(
    Vegetables=Vegetables*128,
    Fruits=Fruits*150,
    WholeGrains=WholeGrains*28.35,
    Legumes=Legumes*28.35,
    Nuts=Nuts*28.35,
    Dairy=Dairy*245,
    AddedSugar=AddedSugar*4.2,
    Potatoes=Potatoes*128
  )
#计算两次平均值（自定义函数）
avg2 <- function(x1, x2) {
  case_when(
    !is.na(x1) & !is.na(x2) ~ (x1 + x2)/2,
    !is.na(x1) & is.na(x2)  ~ x1,
    is.na(x1) & !is.na(x2)  ~ x2,
    TRUE ~ NA_real_
  )
}
#将第一天和第二天的食物，按SEQN做全连接
animal_food <- full_join(animal_day1, animal_day2, by="SEQN", suffix=c("_1","_2")) %>%
  mutate(
    Beef_Lamb   = avg2(Beef_Lamb_1, Beef_Lamb_2),
    Pork        = avg2(Pork_1, Pork_2),
    Poultry     = avg2(Poultry_1, Poultry_2),
    Fish        = avg2(Fish_1, Fish_2),
    Egg         = avg2(Egg_1, Egg_2)
  ) %>%
  select(SEQN, Beef_Lamb, Pork, Poultry, Fish, Egg)
plant_food <- full_join(fped_day1, fped_day2, by="SEQN", suffix=c("_1","_2")) %>%
  mutate(
    Vegetables     = avg2(Vegetables_1, Vegetables_2),
    Fruits         = avg2(Fruits_1, Fruits_2),
    WholeGrains    = avg2(WholeGrains_1, WholeGrains_2),
    Legumes        = avg2(Legumes_1, Legumes_2),
    Nuts           = avg2(Nuts_1, Nuts_2),
    Dairy          = avg2(Dairy_1, Dairy_2),
    AddedSugar     = avg2(AddedSugar_1, AddedSugar_2),
    UnsaturatedOil = avg2(UnsaturatedOil_1, UnsaturatedOil_2),
    Potatoes       = avg2(Potatoes_1, Potatoes_2)
  ) %>%
  select(SEQN, Vegetables, Fruits, WholeGrains, Legumes, Nuts,
         Dairy, AddedSugar, UnsaturatedOil, Potatoes)
eld_food <- left_join(plant_food, animal_food, by="SEQN")
#加入能量
energy_day1 <- dr1tot %>%
  select(SEQN,DR1TKCAL)
energy_day2 <- dr2tot %>%
  select(SEQN,DR2TKCAL)
energy <- full_join(
  energy_day1,
  energy_day2,
  by="SEQN"
)
avg2 <- function(x1,x2){
  case_when(
    !is.na(x1) & !is.na(x2) ~ (x1+x2)/2,
    !is.na(x1) & is.na(x2) ~ x1,
    is.na(x1) & !is.na(x2) ~ x2,
    TRUE ~ NA_real_
  )
}
energy <- energy %>%
  mutate(
    Energy =
      avg2(DR1TKCAL,DR2TKCAL )
  ) %>%
  select(SEQN,Energy)
eld_food <- eld_food %>%
  left_join(energy,by="SEQN")

#=========计算ELD得分================
library(dplyr)
eld_with_scores <- eld_food %>%
  mutate(
    # 1. 蔬菜 Vegetables：>300=3分, 200-300=2分, 100-200=1分, <100=0分
    Veg_score = case_when(
      Vegetables > 300 ~ 3,
      Vegetables >= 200 & Vegetables <= 300 ~ 2,
      Vegetables >= 100 & Vegetables < 200 ~ 1,
      Vegetables < 100 ~ 0,
      is.na(Vegetables) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 2. 水果 Fruits：>200=3分, 100-200=2分, 50-100=1分, <50=0分
    Fruit_score = case_when(
      Fruits > 200 ~ 3,
      Fruits >= 100 & Fruits <= 200 ~ 2,
      Fruits >= 50 & Fruits < 100 ~ 1,
      Fruits < 50 ~ 0,
      is.na(Fruits) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 3. 不饱和油脂 UnsaturatedOil：>40=3分, 20-40=2分, 10-20=1分, <10=0分
    Oil_score = case_when(
      UnsaturatedOil > 40 ~ 3,
      UnsaturatedOil >= 20 & UnsaturatedOil <= 40 ~ 2,
      UnsaturatedOil >= 10 & UnsaturatedOil < 20 ~ 1,
      UnsaturatedOil < 10 ~ 0,
      is.na(UnsaturatedOil) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 4. 豆类 Legumes：>75=3分, 37.5-75=2分, 18.75-37.5=1分, <18.75=0分
    Legume_score = case_when(
      Legumes > 75 ~ 3,
      Legumes >= 37.5 & Legumes <= 75 ~ 2,
      Legumes >= 18.75 & Legumes < 37.5 ~ 1,
      Legumes < 18.75 ~ 0,
      is.na(Legumes) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 5. 坚果 Nuts：>50=3分, 25-50=2分, 12.5-25=1分, <12.5=0分
    Nut_score = case_when(
      Nuts > 50 ~ 3,
      Nuts >= 25 & Nuts <= 50 ~ 2,
      Nuts >= 12.5 & Nuts < 25 ~ 1,
      Nuts < 12.5 ~ 0,
      is.na(Nuts) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 6. 全谷物 WholeGrains：>232=3分, 116-232=2分, 58-116=1分, <58=0分
    WholeGrain_score = case_when(
      WholeGrains > 232 ~ 3,
      WholeGrains >= 116 & WholeGrains <= 232 ~ 2,
      WholeGrains >= 58 & WholeGrains < 116 ~ 1,
      WholeGrains < 58 ~ 0,
      is.na(WholeGrains) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 7. 鱼类 Fish：>28=3分, 14-28=2分, 7-14=1分, <7=0分
    Fish_score = case_when(
      Fish > 28 ~ 3,
      Fish >= 14 & Fish <= 28 ~ 2,
      Fish >= 7 & Fish < 14 ~ 1,
      Fish < 7 ~ 0,
      is.na(Fish) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 8. 牛肉和羊肉 Beef_Lamb：<7=3分, 7-14=2分, 14-28=1分, >28=0分
    Beef_Lamb_score = case_when(
      Beef_Lamb < 7 ~ 3,
      Beef_Lamb >= 7 & Beef_Lamb <= 14 ~ 2,
      Beef_Lamb > 14 & Beef_Lamb <= 28 ~ 1,
      Beef_Lamb > 28 ~ 0,
      is.na(Beef_Lamb) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 9. 猪肉 Pork：<7=3分, 7-14=2分, 14-28=1分, >28=0分
    Pork_score = case_when(
      Pork < 7 ~ 3,
      Pork >= 7 & Pork <= 14 ~ 2,
      Pork > 14 & Pork <= 28 ~ 1,
      Pork > 28 ~ 0,
      is.na(Pork) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 10. 禽肉 Poultry：<29=3分, 29-58=2分, 58-116=1分, >116=0分
    Poultry_score = case_when(
      Poultry < 29 ~ 3,
      Poultry >= 29 & Poultry <= 58 ~ 2,
      Poultry > 58 & Poultry <= 116 ~ 1,
      Poultry > 116 ~ 0,
      is.na(Poultry) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 11. 蛋类 Egg：<13=3分, 13-25=2分, 25-50=1分, >50=0分
    Egg_score = case_when(
      Egg < 13 ~ 3,
      Egg >= 13 & Egg <= 25 ~ 2,
      Egg > 25 & Egg <= 50 ~ 1,
      Egg > 50 ~ 0,
      is.na(Egg) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 12. 乳制品 Dairy：<250=3分, 250-500=2分, 500-1000=1分, >1000=0分
    Dairy_score = case_when(
      Dairy < 250 ~ 3,
      Dairy >= 250 & Dairy <= 500 ~ 2,
      Dairy > 500 & Dairy <= 1000 ~ 1,
      Dairy > 1000 ~ 0,
      is.na(Dairy) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 13. 土豆 Potatoes：<50=3分, 50-100=2分, 100-200=1分, >200=0分
    Potato_score = case_when(
      Potatoes < 50 ~ 3,
      Potatoes >= 50 & Potatoes <= 100 ~ 2,
      Potatoes > 100 & Potatoes <= 200 ~ 1,
      Potatoes > 200 ~ 0,
      is.na(Potatoes) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # 14. 添加糖 AddedSugar：<31=3分, 31-62=2分, 62-124=1分, >124=0分
    Sugar_score = case_when(
      AddedSugar < 31 ~ 3,
      AddedSugar >= 31 & AddedSugar <= 62 ~ 2,
      AddedSugar > 62 & AddedSugar <= 124 ~ 1,
      AddedSugar > 124 ~ 0,
      is.na(AddedSugar) ~ NA_real_,
      TRUE ~ 0
    )
  )

# 计算两类小分和总分
eld_with_scores <- eld_with_scores %>%
  mutate(
    # 鼓励摄入型小计（7项，最高21分）
    Emphasized_subtotal = Veg_score + Fruit_score + Oil_score + 
      Legume_score + Nut_score + WholeGrain_score + Fish_score,
    
    
    # 限制摄入型小计（7项，最高21分）
    Limited_subtotal = Beef_Lamb_score + Pork_score + Poultry_score + 
      Egg_score + Dairy_score + Potato_score + Sugar_score,
    
    # 总分（14项，范围0-42）
    ELD_total_score = Emphasized_subtotal + Limited_subtotal
  )

#============心衰结局==============
library(dplyr)
library(haven)
mcq_1516 <- read_xpt("D:/NHANES数据库/MCQ_J（2017-2018）.xpt")
mcq_1718 <- read_xpt("D:/NHANES数据库/2015-2016/MCQ_I.xpt")
mcq <- bind_rows(mcq_1516,mcq_1718)
hf <- mcq %>%
  transmute(
    SEQN,
    hf = case_when(
      MCQ160B == 1 ~ 1,
      MCQ160B == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
sum(hf == 0, na.rm = TRUE)
sum(hf == 1, na.rm = TRUE)
sum(is.na(hf))
#=============合并协变量=====================
demo_all <- bind_rows(demo_1516, demo_1718) %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR,
         SDMVPSU, SDMVSTRA, WTMEC2YR)
smoking_all <- bind_rows(smq_1516, smq_1718) %>% select(SEQN, SMQ020)
alcohol_all <- bind_rows(alq_1516, alq_1718) %>% select(SEQN, ALQ101)
pa_all <- bind_rows(paq_1516, paq_1718) %>% select(SEQN, PAQ650, PAQ665)
bmx_all <- bind_rows(bmx_1516, bmx_1718) %>% select(SEQN, BMXBMI)
bpq_all <- bind_rows(bpq_1516, bpq_1718) %>% select(SEQN, BPQ020)
diq_all <- bind_rows(diq_1516, diq_1718) %>% select(SEQN, DIQ010)
tchol_all <- bind_rows(tchol_1516, tchol_1718) %>% select(SEQN, LBXTC)

#==========合并分析数据============
analysis_data <- eld_food %>%
  left_join(demo_all, by="SEQN") %>%
  left_join(smoking_all, by="SEQN") %>%
  left_join(alcohol_all, by="SEQN") %>%
  left_join(pa_all, by="SEQN") %>%
  left_join(bmx_all, by="SEQN") %>%
  left_join(bpq_all, by="SEQN") %>%
  left_join(diq_all, by="SEQN") %>%
  left_join(tchol_all, by="SEQN")%>%
  left_join(hf,by="SEQN")

final_data <- eld_with_scores %>%
  left_join(demo_all, by="SEQN") %>%
  left_join(smoking_all, by="SEQN") %>%
  left_join(alcohol_all, by="SEQN") %>%
  left_join(pa_all, by="SEQN") %>%
  left_join(bmx_all, by="SEQN") %>%
  left_join(bpq_all, by="SEQN") %>%
  left_join(diq_all, by="SEQN") %>%
  left_join(tchol_all, by="SEQN")%>%
  left_join(hf,by="SEQN")
#计算四年权重
final_data <- final_data %>%
  mutate(WT4YR = WTMEC2YR / 2)
#Survey设计
library(survey)
design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT4YR,
  nest = TRUE,
  data = final_data
)
#检查代码
str(final_data)
summary(final_data$WT4YR)
