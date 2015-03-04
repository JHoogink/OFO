#This file is used to analyse data with maxLik or optim
#analyze data using maxLik
require(maxLik)

test <- maxLik(beta.odds.PSm,start=c(.01,10,.01),data=sub.data,grad=NULL,hess=NULL,method="BFGS")
test
hist(sub.data$duursubf)

est <- optim(par=c(-1,10,-8),fn=beta.odds.PSo,data=sub.data, hessian=FALSE,method="BFGS")
est
exp(est$par[1])
1/(1+exp(-est$par[3]))


test$estimate[1]*test$estimate[2]

dist <- rbeta(100000,test$estimate[1]*test$estimate[2],test$estimate[2])
hist(dist)
dist2 <-rbeta(10000,2,6)
hist(dist2)

hist(Data_OFO_a$duursubf)
hist(Data_OFO_a$t_fusp_t_new)
mean(Data_OFO_a$t_fusp_t_new)*12
hist(K)
median

test2 <- maxLik(geom.ster.trunc,start=c(.1,.1),data=Data_OFO_a,grad=NULL,hess=NULL,method="BFGS")
test2
test2$estimate[1]/test2$estimate[2]

dist <- rbeta(10000,test2$estimate[1]/test2$estimate[2],(1-test2$estimate[1])/test2$estimate[2])
hist(dist)
dist2 <- rbeta(10000,test$estimate[1]*test$estimate[2],test$estimate[2])
hist(dist2)



