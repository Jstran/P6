### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())

# Størrelse til downloads af grafer: 700 x 250
### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(lubridate)
library(ggplot2)
library(forecast)
library(astsa)

### ¤¤ Funktioner ¤¤ ### --------------------------------------------

# Escribano koefficient -> dataframe
esccoef <- function(mod){
  coef <- as.numeric(mod$coefficients)
  
  # Koefficienter til årlig periode
  c1 <- sqrt(coef[4]^2 + coef[5]^2)
  c2 <- atan(coef[5]/coef[4]) * 365.25/(2*pi)
  
  # Koefficienter til halvårlig periode
  c3 <- sqrt(coef[6]^2 + coef[7]^2)
  c4 <- atan(coef[7]/coef[6]) * 365.25/(4*pi)
  
  # Dataframe med alt info
  df <- data.frame(b0 = coef[1] , b1 = coef[2] , b2 = coef[3] , c1 = c1 , c2 = c2 ,
                   c3 = c3 , c4 = c4 , d1 = coef[8] , d2 = coef[9] , d3 = coef[10]) 
  return(df)
}

# Konfidensinterval
ci <- function(n = numeric(2191)){
  qnorm((1 + 0.95)/2)/sqrt(length(n))
}

### ¤¤ Farver + tema til brug i plots ¤¤ ### -------------------------------------------

# Farver til grafer
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black")

# Grå farve der matcher projektet
myGray <- rgb(245/255, 245/255, 245/255)

# Tema til plots
p6 <- theme(panel.background = element_rect(fill = myGray, colour = myGray,
                                  size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
            )

### ¤¤ Indlæsning af data ¤¤ ### --------------------------------------------------------

# Indlaeser csv filerne som "dat20xx".
path <- file.path("./Data")
years <- list.files(path,pattern =".csv",full.names = 1)
years_names <- seq(2013, length.out = length(years))

for (l in 1:length(years_names)) {
  assign(paste("dat", years_names[l], sep = ""), read.csv2(years[l], skip = 2))
}

# Fjerner skudaar i 2016.
dat2016LY <- dat2016
dat2016 <- dat2016[-60,] 


### ¤¤ Data frame og lister med data ¤¤ ### ---------------------------------------------

# Laver datoer.
datesY <- seq(ymd("2013-01-01"), ymd("2018-12-31"), by="days")
dates<-format(datesY, format="%d-%m")[1:365]

# Samler hvert aar for DK1 i en data frame.
DK1f <- data.frame(DK1 = numeric(365))
row.names(DK1f) <- dates
l <- 1
for (i in 2013:2018) {
  DK1f[,l] <- get(paste("dat", i, sep=""))[,8]
  names(DK1f)[l] <- i
  l <- l + 1
}

# Data frama der indeholder alle år for DK1
dfDK1 <- data.frame(DK1 = numeric(2191), sat = numeric(2191), sun = numeric(2191))
row.names(dfDK1) <- datesY
dfDK1[,1] <- c(DK1f[,1], 
               DK1f[,2],
               DK1f[,3],
               dat2016LY[,8],
               DK1f[,5],
               DK1f[,6])

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(nrow(dfDK1))
sat[seq(5, 2191, by = 7)] <- 1
dfDK1[,2] <- sat

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(nrow(dfDK1))
sun[seq(6, 2191, by = 7)] <- 1
dfDK1[,3] <- sun

# Danske helligdage
hol <- numeric(nrow(dfDK1))
hol[c(1,    83,   87,   88,   90,   91,   116,  129,  139,  140,  359,  360,  # 2013
      366,  468,  472,  473,  475,  476,  501,  514,  524,  525,  724,  725,  # 2014
      731,  818,  822,  823,  825,  826,  851,  864,  874,  875,  1089, 1090, # 2015
      1096, 1175, 1179, 1180, 1182, 1183, 1208, 1221, 1231, 1232, 1455, 1456, # 2016
      1462, 1560, 1564, 1565, 1567, 1568, 1593, 1606, 1661, 1662, 1820, 1821, # 2017
      1827, 1910, 1914, 1915, 1917, 1918, 1943, 1956, 1966, 1967, 2185, 2186  # 2018
      )] <- 1
dfDK1[,4] <- hol

# Fjerner alle lørdage der også er helligdage
for (i in 1:2191) {
  if (sat[i] == 1 && sat[i] == hol[i]){
    sat[i] <- 0
  } 
}

# Fjerner alle søndage der også er helligdage
for (i in 1:2191) {
  if (sun[i] == 1 && sun[i] == hol[i]){
    sun[i] <- 0
  } 
}


# Samler hvert aar samt alle år sammen for DK1 i en liste
fq <- 1
DK1 <- list(A   = ts(dfDK1[,1], frequency = fq),
            Y13 = ts(DK1f[,1],  frequency = fq),
            Y14 = ts(DK1f[,2],  frequency = fq), 
            Y15 = ts(DK1f[,3],  frequency = fq), 
            Y16 = ts(DK1f[,4],  frequency = fq), 
            Y17 = ts(DK1f[,5],  frequency = fq), 
            Y18 = ts(DK1f[,6],  frequency = fq),
            Raw = ts(dfDK1[,1], frequency = fq),
            sat = sat,
            sun = sun,
            hol = hol)

# Fjerner 07-06-13 og erstatter med gennesnit af dagen før og efter. (Spike-dagen)

DK1$A[which.max(DK1$A)] <- mean(c((DK1$A[which.max(DK1$A)-1]), 
                                  (DK1$A[which.max(DK1$A)+1])))

# Fjerner midlertidige variable
rm("dat2013","dat2014","dat2015","dat2016","dat2016LY","dat2017","dat2018","dat2019",
   "dfDK1","DK1f","fq","i","l","myGray","path","sat","sun","years","years_names")

### ¤¤ Mean, sd, acf, pacf ¤¤ ### ------------------------------

# Nogle plots.
par(mfrow = c(2,1))

acf(DK1$A)
pacf(DK1$A)

par(mfrow = c(1,1))


### ¤¤ ggplot af rå data ¤¤ ### ---------------------------------------------------------

# Plot af ukorrigerede data
pRaw <-  ggplot(data.frame(X1 = datesY, 
                           X2 = DK1$Raw), 
                aes(x = X1 , y = X2)) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK", 
       color = "") +
  scale_x_date(date_labels = "%Y", breaks = pretty(datesY, n = 6))  +
  p6
pRaw

# Plot af korrigerede data
pClean <-  ggplot(data.frame(X1 = datesY, 
                             X2 = DK1$A), 
                  aes(x = X1 , y = X2)) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK", 
       color = "") +
  scale_x_date(date_labels = "%Y", breaks = pretty(datesY, n = 6))  +
  p6
pClean

# Histogram for priserne
pHist <- ggplot(data.frame(X1 = datesY, 
                           X2 = DK1$A),
                aes(x = X2)) +
         geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
         stat_function(fun = 
                       function(x){dnorm(x = x, 
                                         mean = mean(DK1$A), 
                                         sd = sd(DK1$A))*length(DK1$A)*24.1
                                   }, 
                       color = colors[2]) +
         labs(x = "Spotpris i DKK", y = "") +
         p6
pHist

# Plot af acf
pAcfA <- ggplot(data = data.frame(X1 = acf(DK1$A, plot = FALSE)$lag,
                                  X2 = acf(DK1$A, plot = FALSE)$acf), 
                aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p6
pAcfA


### ¤¤ Regression ¤¤ ### ----------------------------------------------------------------

# Regression på Escribano model med kvadratisk led (t^2)
t     <- time(DK1$A)
lmEsc <- lm(DK1$A ~ t + I(t^2) + 
                     sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
                     sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) + 
                     DK1$sat + DK1$sun + DK1$hol) ; summary(lmEsc)

EscMod  <- predict(lmEsc)
EscCoef <- esccoef(lmEsc)

DK1[[length(DK1)+1]] <- c(EscMod)
names(DK1)[[length(DK1)]] <- "EscMod"

DK1[[length(DK1)+1]] <- c(DK1$A - EscMod)
names(DK1)[[length(DK1)]] <- "Decomposed"

### ¤¤ Regressions plot ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
pDecomposed <- ggplot(data = data.frame(X1 = datesY,
                                        X2 = DK1$Decomposed),
                      aes(x = X1, y = X2)) +
               geom_line(color = colors[1]) +
               scale_x_date(breaks = pretty(datesY, n = 6)) +
               labs(x = "", y = "Spotpris i DKK") + 
               p6
pDecomposed

# Plotter spotpriser med den determinstiske model lagt ovenpå
pObsVEsc <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$A), 
                     aes(x = X1 , y = X2)) +
  geom_line(aes(col = "Obs")) +
  geom_point(data = data.frame(X1 = datesY, 
                              X2 = DK1$EscMod), 
            aes(col = "Esc model"))+
  scale_x_date(breaks = pretty(datesY, n = 6))  +
  labs(x = "", y = "Spotpris i DKK", title = "Observationer vs. Escribano", color = "") +
  scale_color_manual(values = colors[1:2]) +
  p6
pObsVEsc

# Plot af acf for decomposed
pAcfDecomposed <- ggplot(data = data.frame(X1 = acf(DK1$Decomposed, plot = FALSE)$lag,
                                           X2 = acf(DK1$Decomposed, plot = FALSE)$acf), 
                         aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p6
pAcfDecomposed

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------



