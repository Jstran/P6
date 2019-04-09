rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

pc    <- pi/365.25
nmods <- 1024

mse <- rmse <- mae <- aic <- numeric(nmods)

npred <- 730

df <- data.frame(DK1 = DK1$A , t = t , sat = DK1$sat , sun = DK1$sun , hol = DK1$hol ,
                 sin2  = sin((2*pc)*t)  , cos2  = cos((2*pc)*t) ,
                 sin4  = sin((4*pc)*t)  , cos4  = cos((4*pc)*t) ,
                 sin8  = sin((8*pc)*t)  , cos8  = cos((8*pc)*t) ,
                 sin24 = sin((24*pc)*t) , cos24 = cos((24*pc)*t) )

for (i in 0:(npred - 1)) {
  dfins  <- subset(df, t <= 1461 + i)
  dfpred <- subset(df, t == 1461 + i + 1)
  glob.lm <- lm(DK1 ~ t + I(t^2) + I(t^3) +
                      sin2 + cos2 + sin4  + cos4  + 
                      sin8 + cos8 + sin24 + cos24 +
                      sat  + sun  + hol , 
                      na.action = "na.fail" , data = dfins )
  
  lm.combinations <- lapply(dredge(glob.lm , 
                                   evaluate = FALSE , 
                                   subset = ( (sin2 == cos2) & (sin4  == cos4)  &
                                              (sin8 == cos8) & (sin24 == cos24) ) ) , 
                            eval)
  
  yhat <- sapply(lm.combinations , predict , newdata = dfpred)
  
  aic  <- aic  + sapply(lm.combinations, AIC)
  mse  <- mse  +       ( dfpred$DK1 - yhat )^2
  rmse <- rmse + sqrt( ( dfpred$DK1 - yhat )^2)
  mae  <- mae  + abs(    dfpred$DK1 - yhat)
  print(i)  
}


# Gennemsnitter
mse  <- mse  /npred
rmse <- rmse /npred
mae  <- mae  /npred
aic  <- aic  /npred

lm.combinations[[which.min(aic)]]
lm.combinations[[which.min(mse)]]
lm.combinations[[which.min(rmse)]]
lm.combinations[[which.min(mae)]]


