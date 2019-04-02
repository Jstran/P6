rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

start.time <- Sys.time()

rmse <- numeric(576)
aic  <- numeric(576)
for (i in 0:729) {
  len <- c(1:(1461 + i))
  glob.lm <- lm(DK1$A[len] ~ t[len] + I(t[len]^2) + I(t[len]^3) +
                  sin((2*pc)*t[len]) + cos((2*pc)*t[len]) + 
                  sin((4*pc)*t[len]) + cos((4*pc)*t[len]) +
                  sin((8*pc)*t[len]) + cos((8*pc)*t[len]) +
                  sin((24*pc)*t[len]) + cos((24*pc)*t[len]) +
                  DK1$sat[len] + DK1$sun[len] + DK1$hol[len] , na.action = "na.fail")
  
  lm.combinations <- sapply(dredge(glob.lm , 
                                   evaluate = FALSE,
                                   subset = dc(sin(( 2*pc)*t[len])  , 
                                               cos(( 2*pc)*t[len])  ,
                                               sin(( 4*pc)*t[len])  , 
                                               cos(( 4*pc)*t[len])  ,
                                               sin(( 8*pc)*t[len])  , 
                                               cos(( 8*pc)*t[len])  ,
                                               sin((24*pc)*t[len]) , 
                                               cos((24*pc)*t[len]) ) ),
                            eval)
  aic  <- aic + sapply(lm.combinations, AIC)
  pred.inter <- data.frame(t = 1461 + i + 1)
  for (l in 1:576) {
    rmse[l] <- rmse[l] + sqrt((DK1$A[pred.inter$t] - predict(lm.combinations[[l]], newdata = pred.inter))^2)
  }
  print(i)  
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Gennemsnitter
rmse1 <- rmse/730
aic1 <- aic/730

lm.combinations[[which.min(aic1)]]
lm.combinations[[which.min(rmse1)]]

save(rmse1, aic1, 
     file = "./Workspaces/nested.Rdata")
