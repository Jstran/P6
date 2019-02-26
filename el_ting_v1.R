library(stats)
library(lubridate)
library(ggplot2)
library(pracma)
library(forecast)

# Indlaeser csv filerne som "dat20xx".
path <- file.path("./Data")
years <- list.files(path,pattern =".csv",full.names = 1)
years_names <- seq(2014, length.out = length(years))

for (l in 1:length(years_names)) {
  assign(paste("dat", years_names[l], sep = ""), read.csv2(years[l], skip = 2))
}

# Fjerner skudaar i 2016.
dat2016 <- dat2016[-60,] 

# Laver datoer.
dates_temp <- seq(ymd("1000-01-01"), ymd("1000-12-31"), by="days")
dates<-format(dates_temp, format="%d-%m")

# Samler hvert aar for DK2 i en data frame.
DK2f <- data.frame(numeric(365))
row.names(DK2f) <- dates
l <- 1
for (i in 2014:2018) {
  DK2f[,l] <- get(paste("dat", i, sep=""))[,9]
  names(DK2f)[l] <- i
  l <- l + 1
}

fq <- 7
DK2 <- data.frame(Y14 = ts(DK2f[,1], frequency = fq), Y15 = ts(DK2f[,2], frequency = fq), Y16 = ts(DK2f[,3], frequency = fq), Y17 = ts(DK2f[,4], frequency = fq), Y18 = ts(DK2f[,5], frequency = fq))


# Mean, sd og andre gode sager
apply(DK2f, 2, mean)

apply(DK2f, 2, sd)

# Nogle plots.
plot(DK2$Y14, type = "l")
lines(DK2$Y15, col = "red")

plot(decompose(DK2$Y14))

diffDK2Y14 <- diff(DK2$Y14, differences = 1)

detrend = detrend(dat2016[,9], 'linear'); plot(ts(detrend, frequency = 365))
trend2016 = ma(DK2$Y16, order = 7); plot(DK2$Y16 - trend2016)

data_ts = c(dat2014[,9],dat2015[,9],dat2016[,9],dat2017[,9],dat2018[,9],dat2019[,9])
timeseries = ts(data_ts, frequency = 365, start= c(2014,1))
timeseries
  
plot(ts(dat2018[,9]))
par(new = T)
plot(ts(dat2018[,10]),col="red")

plot(stl(timeseries, s.window = "periodic"))
plot(decompose(timeseries))

lol = auto.arima(timeseries)
plot(lol)
