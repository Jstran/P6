rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

pc    <- pi/365.25
nmods <- 128

start.time <- Sys.time()

mse <- rmse <- mae <- numeric(nmods)
aic  <- numeric(nmods)

df <- data.frame(DK1 = DK1$A , t = t , sat = DK1$sat , sun = DK1$sun , hol = DK1$hol ,
                 sin2  = sin((2*pc)*t)  , cos2  = cos((2*pc)*t) ,
                 sin4  = sin((4*pc)*t)  , cos4  = cos((4*pc)*t) ,
                 sin8  = sin((8*pc)*t)  , cos8  = cos((8*pc)*t) ,
                 sin24 = sin((24*pc)*t) , cos24 = cos((24*pc)*t) )

for (i in 0:729) {
  dfins  <- subset(df, t <= 1461 + i)
  dfpred <- subset(df, t == 1461 + i + 1)
  glob.lm <- lm(DK1 ~ t + I(t^2) + I(t^3) +
                      sin2 + cos2 + sin4  + cos4  + 
                      sin8 + cos8 + sin24 + cos24 +
                      sat  + sun  + hol , 
                      na.action = "na.fail" , data = dfins )
  
  lm.combinations <- lapply(dredge(glob.lm , 
                                   evaluate = FALSE , 
                                   fixed = c("sat" , "sun" , "hol") ,
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

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Gennemsnitter
mse1  <- mse/730
rmse1 <- rmse/730
mae1  <- mae/730
aic1  <- aic/730

lm.combinations[[which.min(aic1)]]
lm.combinations[[which.min(mse1)]]
lm.combinations[[which.min(rmse1)]]
lm.combinations[[which.min(mae1)]]

save(rmse1, aic1, 
     file = "./Workspaces/nested.Rdata")


### ¤¤ Det Vilde Vesten ¤¤ ### ----------------------------------------------------------
x  <- 1:10
y  <- rnorm(10)
z  <- rnorm(10)
y2 <- rnorm(10)
z2 <- rnorm(10)
df <- data.frame(x = x , y = y , y2 =  y2 , z = z , z2 = z2)
glob.lm.test <- lm(x ~ y + y2 + z + z2 , na.action = "na.fail" , data = df)


lm.combinations.test <- dredge(glob.lm.test , evaluate = FALSE , subset = (y == y2 ) & (z == z2) )
lm.combinations.test
