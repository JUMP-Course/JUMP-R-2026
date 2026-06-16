dim(female)
table(female$breast_cancer)
table(female$ALQ120)
str(female)
colSums(is.na(female))

setwd("D:/JUMP-R-2026/students/niujiayi")

library(ggplot2)

p1 <- ggplot(female,aes(x=factor(ALQ121),fill=factor(breast_cancer)))+
  geom_bar(position="dodge")+
  labs(x="饮酒编码",y="人数",title="不同饮酒频次女性分布")+
  theme_bw()
ggsave("figures/01_饮酒频次分布图.png",p1,width=6,height=4)

p2 <- ggplot(female,aes(x=factor(breast_cancer),fill=factor(breast_cancer)))+
  geom_bar()+
  labs(x="0=无乳腺癌，1=患乳腺癌",y="人数",title="乳腺癌患病构成")+
  theme_bw()
ggsave("figures/02_乳腺癌患病构成图.png",p2,width=5,height=4)