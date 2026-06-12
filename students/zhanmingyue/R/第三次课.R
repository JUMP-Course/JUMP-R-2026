dat<-read.csv("D:\\shuju\\clean_dat1.csv")
model1<-glm(diabetes~waist,data=dat,family=binomial)
summary(model1)
exp(coef(model1))
exp(confint(model1))
model2<-glm(diabetes~waist+age+gender+chol,data=dat,family = binomial)
summary(model2)
exp(coef(model2))
exp(confint(model2))
library(sjPlot)
plot_model(model2,type="est",
           title="腰围与糖尿病关联的森林图",
           show.values=TRUE,
           value.offset=0.3)
