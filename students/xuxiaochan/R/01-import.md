library(haven)
library(dplyr)
setwd("C:/Users/86156/Desktop/NHANES_data")
files <- list(
  demo   = "DEMO_L.xpt",acq    = "ACQ_L.xpt",alq    = "ALQ_L.xpt", bmx    = "BMX_L.xpt",bpq    = "BPQ_L.xpt", bpx    = "BPXO_L.xpt", 
  cbc    = "CBC_L.xpt",dbq    = "DBQ_L.xpt",diq    = "DIQ_L.xpt",dpq    = "DPQ_L.xpt",fastqx = "FASTQX_L.xpt",ghb    = "GHB_L.xpt",
  glu    = "GLU_L.xpt",hdl    = "HDL_L.xpt", hscrp  = "HSCRP_L.xpt", paq    = "PAQ_L.xpt", slq    = "SLQ_L.xpt", smq    = "SMQ_L.xpt",
  smqfam = "SMQFAM_L.xpt",smqrtu = "SMQRTU_L.xpt",tchol  = "TCHOL_L.xpt",trigly = "TRIGLY_L.xpt"
)
data_list <- list() #新建空列表，用来存读完的所有数据表
for (name in names(files)) {
  df <- read_xpt(files[[name]]) #读取对应的xpt文件
  df <- df %>% 
    rename_with(~ifelse(.x != "SEQN", paste0(.x, "_", name), .x)) #变量名加上下划线和xpt名称
  data_list[[name]] <- df
}
merged <- data_list$demo #基线人口学做主表
for (name in names(data_list)[-1]) {  
  merged <- merged %>% left_join(data_list[[name]], by = "SEQN")
} #除去基线人口资料，按相同SEQN循环匹配其他列表
write.csv(merged, "C:/Users/86156/Desktop/糖尿病.csv", row.names = FALSE)