library(stats)
library(lubridate)

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
DK2 <- list(Y14 = ts(DK2f[,1], frequency = fq), Y15 = ts(DK2f[,2], frequency = fq), Y16 = ts(DK2f[,3], frequency = fq), Y17 = ts(DK2f[,4], frequency = fq), Y18 = ts(DK2f[,5], frequency = fq))

# Nogle plots.
plot(DK2$Y14, type = "l")
lines(DK2$Y15, col = "red")

par(mfrow = c(2,2))
acf(DK2$Y16)
acf(DK2$Y17)

pacf(DK2$Y16)
pacf(DK2$Y16)
par(mfrow = c(1,1))

plot(decompose(DK2$Y14))


# Scatter plot med lag.
par(mfrow = c(2,1))
plot(DK2$Y14, lag(DK2$Y14,2),main = "2014", xlab = "x_t", ylab = "x_{t-1}")
plot(DK2$Y15, lag(DK2$Y15,2),main = "2015", xlab = "x_t", ylab = "x_{t-1}")
par(mfrow = c(1,1))

diffDK2Y14 <- diff(DK2$Y14, differences = 1)
par(mfrow = c(2,1))
plot(diffDK2Y14)
plot(DK2$Y14, type = "l")
par(mfrow = c(1,1))
