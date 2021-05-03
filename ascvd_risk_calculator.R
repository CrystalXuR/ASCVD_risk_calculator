## ASCVD risk calculator for any given time (modified based on Syalowsky's revised-pooled-ascvd code)
## 1) In the following code, the only thing we used from the PCE and Framingham paper is their 
## coefficient estimates for the predictors. All the other values are computed from a given data
## 2) Current code can compute the ascvd risk at an arbitrary time
library(glmnet)

# Function for computing S0 using penalized Cox model 
base_surv <- function(Y, D, x, alpha, lambda, t){
  
  fit <- glmnet(x, Surv(Y, D), family="cox", alpha = alpha)
  data <- data.frame(t_event=Y, event=D, x)
  
  tab <- data.frame(table(data[data$event == 1, "t_event"])) 
  y <- as.numeric(as.character(sort(unique(tab[,1]))))
  d <- tab[,2]  # number of events at each unique time                               
  
  betaHat <- as.vector((fit$beta)[,which(abs(fit$lambda - lambda)==min(abs(fit$lambda - lambda)))])
  h0 <- rep(NA, length(y))
  for(l in 1:length(y)){
    # h0(t)=h(t)/exp(X*beta), where h(t)={# of event}/{# at risk} for a given time point
    h0[l] <- d[l] / sum(exp(x[data$t_event >= y[l], rownames(fit$beta)] %*% betaHat))    
  }    
  
  S0 <- exp(-cumsum(h0))
  outcome <- list(fit, survival=data.frame(time=y,survival=S0))
  
  S0_t <- outcome$survival[which.min(abs(outcome$survival$time-t)),]$survival
  return(S0_t)
}

# PCE: compute X*beta-mean(X*beta)
original.link <- function(x, group) {
  age = x$age
  totchol = x$totchol
  hdl = x$hdl
  sysbp = x$sysbp
  dm = x$dm
  rxbp = x$rxbp
  cursmoke = x$cursmoke
  
  vars = matrix(c(log(age),
                  log(age)^2,
                  log(totchol),
                  log(age)*log(totchol),
                  log(hdl),
                  log(age)*log(hdl),
                  rxbp*log(sysbp),
                  rxbp*log(age)*log(sysbp),
                  (1-rxbp)*log(sysbp),
                  (1-rxbp)*log(age)*log(sysbp),
                  cursmoke,
                  log(age)*cursmoke,
                  dm), ncol=13)
  
  coefs = list(
    c(17.114, 0, 0.94, 0, -18.920, 4.475, 29.291, -6.432, 27.820, -6.087, 0.691, 0, 0.874),      # African American women 
    c(-29.799, 4.884, 13.54, -3.114, -13.578, 3.149, 2.019, 0, 1.957, 0, 7.574, -1.665, 0.661),  # white women 
    c(2.469, 0, 0.302, 0, -0.307, 0, 1.916, 0, 1.809, 0, 0.549, 0, 0.645),                       # African American men
    c(12.344, 0, 11.853, -2.664, -7.990, 1.769, 1.797, 0, 1.7864, 0, 7.837, -1.795, 0.658))      # white men
  
  # Crystal modified this by replacing the mean(X*beta) from the PCE paper with
  # the mean of individual.risk that calculated with the current given data 
  individual.risk = unlist(vars %*% coefs[[group]])
  link = individual.risk - mean(individual.risk)
  return(link)
}

# PCE: compute 1-S_0(t)^exp(X*beta-mean(X*beta))
original.model <- function(x, Y, D, group, t, alpha=0.75, nfold=10) {
  
  link <- rep(NA,dim(x)[1])
  for (i in 1:length(unique(group))){
    tmp <- x[group==i,]
    index <- as.numeric(rownames(tmp))
    link[index] <- original.link(tmp,i)
  }
  relative.risk = exp(link)
  
  # Penalized cox PH model 
  fit <- cv.glmnet(x=as.matrix(x[,c("FEMALE","RACE_BLACK")]),
                   y=Surv(Y,D),
                   family="cox",
                   alpha=alpha,
                   nfolds=nfold)
  
  # compute baseline survival for any given time and given data 
  baseline.survival <- base_surv(Y=Y, 
                                 D=D, 
                                 x=as.matrix(x[,c("FEMALE","RACE_BLACK")]), 
                                 alpha=alpha, 
                                 lambda=fit$lambda.1se, 
                                 t=t)
  
  risk = 1 - baseline.survival ^ relative.risk
  return(risk)
}

# Framingham: compute X*beta-mean(X*beta)
framingham.link <- function(x, group) {
  age = x$age
  totchol = x$totchol
  hdl = x$hdl
  sysbp = x$sysbp
  dm = x$dm
  rxbp = x$rxbp
  cursmoke = x$cursmoke
  
  vars = matrix(c(log(age),
                  log(totchol),
                  log(hdl),
                  (1-rxbp)*log(sysbp),
                  rxbp*log(sysbp),
                  cursmoke,
                  dm), ncol=7)
  
  coefs = list(
    c(2.32888, 1.20904, -0.70833, 2.76157, 2.82263, 0.52873, 0.69154),
    c(3.06117, 1.12370, -0.93263, 1.93303, 1.99881, 0.65451, 0.57367))
  coefs <- coefs[[group]]
  individual.risk = unlist(vars %*% coefs)
  link = individual.risk - mean(individual.risk)
  return(link)
}

# Framingham: compute 1-S_0(t)^exp(X*beta-mean(X*beta))
framingham.model <- function(x, Y, D, group, t, alpha=0.75, nfold=10) {
  link <- rep(NA,dim(x)[1])
  for (i in 1:length(unique(group))){
    tmp <- x[group==i,]
    index <- as.numeric(rownames(tmp))
    link[index] <- framingham.link(tmp,i)
  }
  relative.risk = exp(link)
  
  # Penalized cox PH model 
  ones <- rep(1, nrow(x))
  fit <- cv.glmnet(x=as.matrix(cbind(ones,group)),
                   y=Surv(Y,D),
                   family="cox",
                   alpha=alpha,
                   nfolds=nfold)
  
  # compute baseline survival for any given time and given data 
  baseline.survival <- base_surv(Y=Y, 
                                 D=D, 
                                 x=as.matrix(cbind(ones,group)), 
                                 alpha=alpha, 
                                 lambda=fit$lambda.1se, 
                                 t=t)
  risk = 1 - baseline.survival ^ relative.risk
  return(risk)
}
