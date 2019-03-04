library(stats)
library(lubridate)
library(ggplot2)
# DK1 : Jylland og Fyn, DK2: Sjælland , DK : Det hele 

### Farver til brug i plots -------------------------------------------------------------
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black")

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

# Samler hvert aar for DK2 i en data frame.
DK2f <- data.frame(numeric(365))
row.names(DK2f) <- dates
l <- 1
for (i in 2013:2018) {
  DK2f[,l] <- get(paste("dat", i, sep=""))[,9]
  names(DK2f)[l] <- i
  l <- l + 1
}

# Samler alle aar for DK2 i samme data frama.
dfDK2 <- data.frame(DK2 = numeric(2191), sat = numeric(2191), sun = numeric(2191))
row.names(dfDK2) <- datesY
dfDK2[,1] <- c(DK2f[,1], 
               DK2f[,2],
               DK2f[,3],
               dat2016LY[,9],
               DK2f[,5],
               DK2f[,6])

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(nrow(dfDK2))
sat[seq(5, 2191, by = 7)] <- 1
dfDK2[,2] <- sat

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(nrow(dfDK2))
sun[seq(6, 2191, by = 7)] <- 1
dfDK2[,3] <- sun

# Samler hvert aar samt alle år sammen for DK1 i en liste
fq <- 1 
DK2 <- list(Y13  = ts(DK2f[,1],  frequency = fq),
            Y14  = ts(DK2f[,2],  frequency = fq), 
            Y15  = ts(DK2f[,3],  frequency = fq), 
            Y16  = ts(DK2f[,4],  frequency = fq), 
            Y17  = ts(DK2f[,5],  frequency = fq), 
            Y18  = ts(DK2f[,6],  frequency = fq),
            YAll = ts(dfDK2[,1], frequency = fq),
            sat  = sat,
            sun  = sun)


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
DK1 <- list(Y13   = ts(DK1f[,1],  frequency = fq),
            Y14   = ts(DK1f[,2],  frequency = fq), 
            Y15   = ts(DK1f[,3],  frequency = fq), 
            Y16   = ts(DK1f[,4],  frequency = fq), 
            Y17   = ts(DK1f[,5],  frequency = fq), 
            Y18   = ts(DK1f[,6],  frequency = fq),
            YAll  = ts(dfDK1[,1], frequency = fq),
            Clean = ts(dfDK1[,1], frequency = fq),
            sat   = sat,
            sun   = sun)

# Fjerner 07-06-13 og erstatter med gennesnit af dagen før og efter.

DK1$Clean[which.max(DK1$Clean)] <- mean(c((DK1$Clean[which.max(DK1$Clean)-1]), 
                                          (DK1$Clean[which.max(DK1$Clean)+1])))

### Mean, sd, acf, pacf, decompose, plot med lag ----------------------------------------

# Mean
mean(DK1$YAll)

# var
var(DK1$YAll)

# Nogle plots.
par(mfrow = c(2,1))
acf(DK1$YAll)

pacf(DK1$YAll)

#plot(decompose(DK1$YAll))


# Scatter plot med lag.
par(mfrow = c(2,1))
plot(DK1$Y14, lag(DK1$Y14,2),main = "2014", xlab = "x_t", ylab = "x_{t-1}")
plot(DK1$Y15, lag(DK1$Y15,2),main = "2015", xlab = "x_t", ylab = "x_{t-1}")
par(mfrow = c(1,1))


### ggplot af rå data -------------------------------------------------------------------
pDK1YAll <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$YAll), 
                    aes(x = X1 , y = X2)) +
  geom_line(aes(col = "DK1")) +
  labs(x = "Tid", y = "Spotpris i DKK", title = "DK1 2013-2018: Spotpriser", 
       color = "") +
  scale_color_manual(values = colors[1])

pDK1YAll

pDK1CleanVRaw <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$Clean), 
                    aes(x = X1 , y = X2)) +
  geom_line(aes(col = "Clean")) +
  geom_line(data = data.frame(X1 = datesY,
                              X2 = DK1$YAll),
            aes(col = "Raw"), alpha = 0.5) +
  labs(x = "Tid", y = "Spotpris i DKK", title = "DK1 2013-2018:Raw", 
       color = "") +
  scale_color_manual(values = colors[1:2])

pDK1CleanVRaw

### Regression --------------------------------------------------------------------------

# DK1 regression på Escribano model  
t <- time(DK1$YAll)
df <- data.frame(Obs = DK1$YAll,
                 t   = time(DK1$YAll),
                 sat = DK1$sat,
                 sun = DK1$sun)

modEsc <- nls(Obs ~ B0 + BT * t + C1 * sin((C2 + t) * 2*pi/365.25) + C3 * 
                   sin((C4 + t) * 4*pi/365.25) + D1 * sat + D2 * sun,
                 start = c(B0 = 1, BT = 1, C1 = 1, C2 = 1, C3 = 1, C4 = 1, D1 = 1, D2 = 1), data = df)

modEscCoef <- coefficients(modEsc)

EscMod <- invisible(modEscCoef[1] + modEscCoef[2] * t + modEscCoef[3] * 
                         sin((modEscCoef[4] + t) * 2*pi/365.25) + modEscCoef[5] * 
                         sin((modEscCoef[6] + t) * 4*pi/365.25) + modEscCoef[7] * 
                         DK1$sat + modEscCoef[8] * DK1$sun)

DK1[[length(DK1)+1]] <- c(EscMod)
names(DK1)[[length(DK1)]] <- "EscMod"

DK1[[length(DK1)+1]] <- c(DK1$YAll - EscMod)
names(DK1)[[length(DK1)]] <- "Decomposed"

# DK1$Clean regression på Escribano model 
dfClean <- data.frame(Obs = DK1$Clean,
                 t   = time(DK1$Clean),
                 sat = DK1$sat,
                 sun = DK1$sun)


modEscClean <- nls(Obs ~ B0 + BT * t + C1 * sin((C2 + t) * 2*pi/365.25) + C3 * 
                sin((C4 + t) * 4*pi/365.25) + D1 * sat + D2 * sun,
              start = c(B0 = 1, BT = 1, C1 = 1, C2 = 1, C3 = 1, C4 = 1, D1 = 1, D2 = 1), data = dfClean)

modEscCoefClean <- coefficients(modEscClean)

EscModClean <- invisible(modEscCoefClean[1] + modEscCoefClean[2] * t + modEscCoefClean[3] * 
                      sin((modEscCoefClean[4] + t) * 2*pi/365.25) + modEscCoefClean[5] * 
                      sin((modEscCoefClean[6] + t) * 4*pi/365.25) + modEscCoefClean[7] * 
                      DK1$sat + modEscCoefClean[8] * DK1$sun)

DK1[[length(DK1)+1]] <- c(EscModClean)
names(DK1)[[length(DK1)]] <- "EscModClean"

DK1[[length(DK1)+1]] <- c(DK1$YAll - EscModClean)
names(DK1)[[length(DK1)]] <- "Decomposed Clean"


# Plots
pEscModS <- ggplot(data.frame(X1 = datesY, 
                              X2 = DK1$EscMod), 
                      aes(x = X1 , y = X2)) +
  geom_point(colour = colors[1], size = 0.7) +
  labs(x = "Tid", y = "DKK", title = "Escribano model DK1", color = "") +
  scale_x_date(breaks = pretty(datesY, n = 12))
pEscModS



pObsVEsc <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$EscMod), 
                       aes(x = X1 , y = X2)) +
  geom_point(aes(col = "Escribano model")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$YAll), 
            aes(col = "Raw"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Observationer vs. Escribane", color = "") +
  scale_color_manual(values = colors[1:2]) +
  ylim(-150,600)

pObsVEsc


pObsVDec <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$Decomposed), 
                       aes(x = X1 , y = X2)) +
  geom_line(aes(col = "Decomposed")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$YAll), 
            aes(col = "Raw"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Raw vs. Decomposed", color = "") +
  scale_color_manual(values = colors[1:2]) +
  ylim(-200,600)

pObsVDec

pRawVClean <-  ggplot(data.frame(X1 = datesY, 
                                 X2 = DK1$EscMod), 
                    aes(x = X1 , y = X2)) +
  geom_point(aes(col = "Raw")) +
  geom_point(data = data.frame(X1 = datesY, 
                              X2 = DK1$EscModClean), 
            aes(col = "Clean"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Raw vs. clean", color = "") +
  scale_color_manual(values = colors[1:2])

pRawVClean

pObsVEscClean <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$EscModClean), 
                    aes(x = X1 , y = X2)) +
  geom_point(aes(col = "Escribano model")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$Clean), 
            aes(col = "Raw"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Observationer vs. Escribano (clean)", color = "") +
  scale_color_manual(values = colors[1:2])

pObsVEscClean

