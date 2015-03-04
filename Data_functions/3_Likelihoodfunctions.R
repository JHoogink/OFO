require(maxLik)

beta.odds.PSm <- function(par,data){
  #b0.ce <- par[1]
  #b0.be <- par[2]
  #b0.PS <- par[3]
  ce <- par[1]
  be <- par[2]
  PS <- par[3]
  
  
  t <- ifelse(data$einddrtt_new==1,round(data$t_fusp_t_new*12),ceiling(data$t_fusp_t_new*12)) #Follow-up since entering study
  
  Y <- round(data$duursubf*12) #Start of attempts
  
  K <- Y+t #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
  d <- ifelse(data$einddrtt_new==1,0,1)
  
  #ce <- exp(b0.ce)  
  #be <- exp(b0.be)
  #PS <- 1/(1+exp(-b0.PS))
  

  #log likelihood contributions: density if couple is pregnant, survival when couples is censored.
  #Both conditional on truncation until certain timepoint
  #Using exp(log) to try to reduce function time by overcoming log gamma problems
  
 

# new try
dens <- d*(log(1-PS)+lgamma(be+(K-1))-lgamma(be)-lgamma((ce*be)+be+(K-1))+lgamma((ce*be)+be)+log(ce)+log(exp(-log(1+((K-1)/be))))-log(ce*(exp(-log(1+((K-1)/be))))+1))
surv <- (1-d)*(log(PS+(1-PS)*exp(lgamma(be+K)-lgamma(be)-lgamma((be*ce)+be+K)+lgamma((be*ce)+be))))
trunc <- log(PS+(1-PS)*exp(lgamma(be+Y)+lgamma((ce*be)+be)-lgamma(be)-lgamma((ce*be)+be+Y)))
#trunc <- 0
#Used for check 4
  


  
  logl <- sum(dens)+sum(surv)-sum(trunc)
  #logl <- sum(density)+sum(survival)-sum(truncation)
  return(logl)
}

beta.odds.PSo <- function(par,data){
  b0.ce <- par[1]
  #b0.be <- par[2]
  b0.PS <- par[3]
  #ce <- par[1]
  be <- par[2]
  #PS <- par[3]
  
  t <- ifelse(data$einddrtt_new==1,round(data$t_fusp_t_new*12),ceiling(data$t_fusp_t_new*12)) #Follow-up since entering study
  
   Y <- round(data$duursubf*12) #Start of attempts
  
   K <- Y+t #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
   d <- ifelse(data$einddrtt_new==1,0,1) # 1= no pregnancy at end of follow up, so for these couples we need the survival, 2 = pregnancy and we need density
  
   #K <- ifelse(data$delta==0,round(data$K),ceiling(data$K)) #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
  #d <- data$delta # 1= no pregnancy at end of follow up, so for these couples we need the survival, 2 = pregnancy and we need density
  #Y <- data$Y
  
  
  ce <- exp(b0.ce)  
  #be <- exp(b0.be)
  PS <- 1/(1+exp(-b0.PS))
  
  #log likelihood contributions: density if couple is pregnant, survival when couples is censored.
  #Both conditional on truncation until certain timepoint
  
  # new try
  dens <- d*(log(1-PS)+lgamma(be+(K-1))-lgamma(be)-lgamma((ce*be)+be+(K-1))+lgamma((ce*be)+be)+log(ce)+log(exp(-log(1+((K-1)/be))))-log(ce*(exp(-log(1+((K-1)/be))))+1))
  surv <- (1-d)*(log(PS+(1-PS)*exp(lgamma(be+K)-lgamma(be)-lgamma((be*ce)+be+K)+lgamma((be*ce)+be))))
  trunc <- log(PS+(1-PS)*exp(lgamma(be+Y)+lgamma((ce*be)+be)-lgamma(be)-lgamma((ce*be)+be+Y)))
  
  logl <- sum(dens)+sum(surv)-sum(trunc)
  return(-logl)
}

beta.PSm <- function(par,data){
  ae <- par[1]
  be <- par[2]
  PS <- par[3]
  t <- ifelse(data$einddrtt_new==1,round(data$t_fusp_t_new*12),ceiling(data$t_fusp_t_new*12)) #Follow-up since entering study
  
  Y <- round(data$duursubf*12) #Start of attempts
  
  K <- t+Y #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
  d <- ifelse(data$einddrtt_new==1,0,1) # 1= 
  
 
  #Using exp(log) to try to reduce function time by overcoming log gamma problems
  dens <- d*(log(1-PS)+lgamma(be+(K-1))-lgamma(be)-lgamma(ae+be+(K-1))+lgamma(ae+be)+log(ae)-log(ae+be+(K-1)))
  surv <- (1-d)*(log(PS+(1-PS)*exp(lgamma(be+K)+lgamma(ae+be)-lgamma(be)-lgamma(ae+be+K))))
  trunc <- (log(PS+(1-PS)*exp(lgamma(be+Y)+lgamma(ae+be)-lgamma(be)-lgamma(ae+be+Y))))
  
  
  
  #logl <- sum(density)+sum(survival)-sum(truncation)
  
  logl <- sum(dens)+sum(surv)-sum(trunc)
  #logl <- sum(density)+sum(survival)-sum(truncation)
  return(logl)
}

betaWG.PSm <- function(par,data){
  mu <- par[1]
  theta <- par[2]
  PS <- par[3]
  t <- ifelse(data$einddrtt_new==1,round(data$t_fusp_t_new*12),ceiling(data$t_fusp_t_new*12)) #Follow-up since entering study
  
  Y <- round(data$duursubf*12) #Start of attempts
  
  K <- t+Y #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
  d <- ifelse(data$einddrtt_new==1,0,1) # 1= 
  
  
  ae <- (mu/theta)
  be <- ((1-mu)/theta)
  

  #Using exp(log) to try to reduce function time by overcoming log gamma problems
  dens <- d*(log(1-PS)+lgamma(be+(K-1))-lgamma(be)-lgamma(ae+be+(K-1))+lgamma(ae+be)+log(ae)-log(ae+be+(K-1)))
  surv <- (1-d)*(log(PS+(1-PS)*exp(lgamma(be+K)+lgamma(ae+be)-lgamma(be)-lgamma(ae+be+K))))
  trunc <- (log(PS+(1-PS)*exp(lgamma(be+Y)+lgamma(ae+be)-lgamma(be)-lgamma(ae+be+Y))))
  
  
  
  #logl <- sum(density)+sum(survival)-sum(truncation)
  
  logl <- sum(dens)+sum(surv)-sum(trunc)
  #logl <- sum(density)+sum(survival)-sum(truncation)
  return(logl)
}

geom.ster.trunc <- function(par,data){
  theta <- par[1]
  PS <- par[2]
  t <- ifelse(data$einddrtt_new==1,round(data$t_fusp_t_new*12),ceiling(data$t_fusp_t_new*12)) #Follow-up since entering study
  
  Y <- round(data$duursubf*12) #Start of attempts
  
  K <- t+Y #Total duration of subfertility until end follow-up (K=Y+t), either succesfull or ending in censoring
  
  d <- ifelse(data$einddrtt_new==1,0,1) # 1= 

    
    trunc <- log(PS+(1-PS)*(1-theta)^(Y))
    #trunc <- log(1-theta)*sum(y)
    dens <- d*(log(PS+(1-PS)*(1-theta)^(K-1))+log(1-(PS/(PS+(1-PS)*(1-theta)^(K))))+log(theta))
    survival <- (1-d)*(log(PS+(1-PS)*(1-theta)^(K)))
    
    
    logl <- sum(dens)+sum(survival)-sum(trunc)
    
    
  
}
