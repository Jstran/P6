rm(list=ls())

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(dplyr)
# DK1 : Jylland og Fyn, DK2: Sjælland , DK : Det hele 

### Farver + thema til brug i plots -----------------------------------------------------
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

### Indlæsning af data ------------------------------------------------------------------
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


### Data frame og lister med data -------------------------------------------------------
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

# Samler hvert aar samt alle år sammen for DK1 i en liste
fq <- 1
DK1 <- list(Y13 = ts(DK1f[,1],  frequency = fq),
            Y14 = ts(DK1f[,2],  frequency = fq), 
            Y15 = ts(DK1f[,3],  frequency = fq), 
            Y16 = ts(DK1f[,4],  frequency = fq), 
            Y17 = ts(DK1f[,5],  frequency = fq), 
            Y18 = ts(DK1f[,6],  frequency = fq),
            Y   = ts(dfDK1[,1], frequency = fq),
            Raw = ts(dfDK1[,1], frequency = fq),
            sat = sat,
            sun = sun)

# Fjerner 07-06-13 og erstatter med gennesnit af dagen før og efter.

DK1$Y[which.max(DK1$Y)] <- mean(c((DK1$Y[which.max(DK1$Y)-1]), 
                                  (DK1$Y[which.max(DK1$Y)+1])))

### Mean, sd, acf, pacf, decompose, plot med lag ----------------------------------------

# Mean
mean(DK1$Y)

# var
var(DK1$Y)

# Nogle plots.
par(mfrow = c(2,1))
acf(DK1$Y)

pacf(DK1$Y)

#plot(decompose(DK1$YAll))


# Scatter plot med lag.
par(mfrow = c(2,1))
plot(DK1$Y14, lag(DK1$Y14,2),main = "2014", xlab = "x_t", ylab = "x_{t-1}")
plot(DK1$Y15, lag(DK1$Y15,2),main = "2015", xlab = "x_t", ylab = "x_{t-1}")
par(mfrow = c(1,1))


### ggplot af rå data -------------------------------------------------------------------
pRaw <-  ggplot(data.frame(X1 = datesY, 
                           X2 = DK1$Raw), 
                aes(x = X1 , y = X2)) +
  geom_line(aes(col = "Spotpris"), show.legend = FALSE) +
  labs(x = "", y = "DKK", 
       color = "") +
  scale_color_manual(values = colors[1]) +
  scale_x_date(breaks = pretty(datesY, n = 6))  +
  p6

pRaw



#pHist <- ggplot(data.frame(X1 = datesY, 
#                           X2 = DK1$Y),
#                aes(x = X2)) +
#         geom_histogram(binwidth = 25, color = "white", fill = colors[1]) + 
#  geom_density(alpha=.2, fill="#FF6666")
#pHist

x <- seq(-300, 300, length = 3000)
hist(DK1$Y - mean(DK1$Y), probability = TRUE, breaks = 50)
lines(x, dnorm(x, mean = -10, sd = 68), lty = 2, lwd = 1)

### Regression --------------------------------------------------------------------------

# DK1 regression på Escribano model  
t <- time(DK1$Y)

# DK1 regression på Escribano model 
lmEsc <- lm(DK1$Y ~ t + 
              sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
              sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) + 
              DK1$sat + DK1$sun) ; summary(lmEsc)

EscMod <- predict(lmEsc)

DK1[[length(DK1)+1]] <- c(EscMod)
names(DK1)[[length(DK1)]] <- "EscMod"

DK1[[length(DK1)+1]] <- c(DK1$Y - EscMod)
names(DK1)[[length(DK1)]] <- "Decomposed"

# Regression på Escribano model med kvadratisk led (t^2)
lmEsc2 <- lm(DK1$Y ~ t + I(t^2) + 
               sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
               sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) + 
               DK1$sat + DK1$sun) ; summary(lmEsc)


EscMod2 <- predict(lmEsc2)

DK1[[length(DK1)+1]] <- c(EscMod2)
names(DK1)[[length(DK1)]] <- "EscMod2"

DK1[[length(DK1)+1]] <- c(DK1$Y - EscMod2)
names(DK1)[[length(DK1)]] <- "Decomposed2"


### Regressions plot --------------------------------------------------------------------
pEsc <- ggplot(data.frame(X1 = datesY,
                          X2 = DK1$Y),
               aes(x = X1,
                   y = X2)) +
  geom_line(aes(col = "Obs")) +
  geom_line(data = data.frame(X1 = datesY,
                              X2 = DK1$Decomposed2),
            aes(col = "Decomposed med Esc(+t^2)"),
            alpha = 0.8) +
  scale_colour_manual(values = colors[1:2]) +
  labs(col = "", x = "", y = "DKK") + 
  p6
pEsc


pObsVEsc2 <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$EscMod2), 
                    aes(x = X1 , y = X2)) +
  geom_point(aes(col = "Esc model(t^2)")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$Y), 
            aes(col = "Obs"))+
  labs(x = "Tid", y = "DKK", title = "Observationer vs. Escribano", color = "") +
  scale_color_manual(values = colors[1:2]) +
  p6
  
pObsVEsc2
