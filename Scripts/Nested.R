rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

pc    <- pi/365.25

start.time <- Sys.time()

rmse <- numeric(576)
aic  <- numeric(576)
for (i in 0:729) {
  
  len <- c(1:(1461 + i))
  df <- data.frame(DK1 = DK1$A[len] , t = t[len] , sat = DK1$sat[len] , sun = DK1$sun[len] , hol = DK1$hol[len])
  glob.lm <- lm(DK1 ~ t + I(t^2) + I(t^3) +
                      sin((2*pc)*t) + cos((2*pc)*t) + 
                      sin((4*pc)*t) + cos((4*pc)*t) +
                      sin((8*pc)*t) + cos((8*pc)*t) +
                      sin((24*pc)*t) + cos((24*pc)*t) +
                      sat + sun + hol , na.action = "na.fail" , data = df)
  
  lm.combinations <- sapply(dredge(glob.lm , 
                                   evaluate = FALSE,
                                   subset = c(dc(sin((2*pc)*t) , cos((2*pc)*t)) ,
                                              dc(sin((4*pc)*t) , cos((4*pc)*t)) ,
                                              dc(sin((8*pc)*t) , cos((8*pc)*t)) , 
                                              dc(sin((24*pc)*t) , cos((24*pc)*t))  ) ),
                            eval)
  
  aic  <- aic + sapply(lm.combinations, AIC)
  ind <- 1461 + i + 1
  pred.inter <- data.frame(t = ind , sat = DK1$sat[ind] , sun = DK1$sun[ind] , hol = DK1$hol[ind])
  for (l in 1:2000) {
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


### ¤¤ Det Vilde Vesten ¤¤ ### ----------------------------------------------------------
x <- 1:10
t <- rnorm(10)

glob.lm.test <- lm(x ~ t + I(t^2) + I(t^3) + I(t^4) , na.action = "na.fail")

lm.combinations.test <- dredge(glob.lm.test , evaluate = FALSE , subset = ("t" & "I(t^2)") | ("I(t^3)" & "I(t^4)") )

lm.combinations.test <- sapply(dredge(glob.lm.test , evaluate = FALSE , subset = ("t" & "I(t^2)") | ("I(t^3)" & "I(t^4)") ) , eval )

lm.combinations.test
lm.combinations.test[[4]]
