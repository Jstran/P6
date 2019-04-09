rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

pc    <- pi/365.25
nmods <- 1024

mad.n <- mse.n <- rmse.n <- mape.n <- theil.n <- aic.n <- numeric(nmods)

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
  ydiff <- dfpred$DK1 - yhat
  
  aic.n   <- aic.n   + sapply(lm.combinations, AIC)
  mad.n   <- mad.n   +  abs(   ydiff )
  mse.n   <- mse.n   +     (   ydiff )^2
  rmse.n  <- rmse.n  + sqrt( ( ydiff )^2)
  mape.n  <- mape.n  +  abs( ( ydiff )/dfpred$DK1*100 )
  theil.n <- theil.n + sqrt( ( ydiff )^2)/(sqrt(dfpred$DK1^2)*sqrt(yhat^2) )
  print(i)  
}


# Gennemsnitter
aic   <- aic.n   / npred
mad   <- mad.n   / npred
mse   <- mse.n   / npred
rmse  <- rmse.n  / npred
mape  <- mape.n  / npred
theil <- theil.n / npred

lm.combinations[[ sort(aic   , index = TRUE)$ix[1:3] ]]
lm.combinations[[ sort(mad   , index = TRUE)$ix[1:3] ]]
lm.combinations[[ sort(mse   , index = TRUE)$ix[1:3] ]]
lm.combinations[[ sort(rmse  , index = TRUE)$ix[1:3] ]]
lm.combinations[[ sort(mape  , index = TRUE)$ix[1:3] ]]
lm.combinations[[ sort(theil , index = TRUE)$ix[1:3] ]]