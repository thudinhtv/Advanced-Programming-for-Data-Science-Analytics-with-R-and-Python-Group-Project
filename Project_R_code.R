# Read in library
library(psych)      #for descriptive analysis (describe, plot, boxplot)
library(ggplot2)
library(nortest)    #Anderson-Darling normality test
library(tidyverse)
library(tseries)
library(forecast)
library(lmtest)


# Read in data
##2015
NYC_Q1_2015 = read.table('nyc-park-crime-stats-q1-2015.csv', sep = ',', header = T, quote = "")
dim(NYC_Q1_2015)
names(NYC_Q1_2015)
str(NYC_Q1_2015)

NYC_Q2_2015 = read.table('nyc-park-crime-stats-q2-2015.csv', sep = ',', header = T, quote = "")
dim(NYC_Q2_2015)
names(NYC_Q2_2015)
str(NYC_Q2_2015)

NYC_Q3_2015 = read.table('nyc-park-crime-stats-q3-2015.csv', sep = ',', header = T, quote = "")
dim(NYC_Q3_2015)
names(NYC_Q3_2015)
str(NYC_Q3_2015)

NYC_Q4_2015 = read.table('nyc-park-crime-stats-q4-2015.csv', sep = ',', header = T, quote = "")
dim(NYC_Q4_2015)
names(NYC_Q4_2015)
str(NYC_Q4_2015)

##2016
NYC_Q1_2016 = read.table('nyc-park-crime-stats-q1-2016.csv', sep = ',', header = T, quote = "")
dim(NYC_Q1_2016)
names(NYC_Q1_2016)
str(NYC_Q1_2016)

NYC_Q2_2016 = read.table('nyc-park-crime-stats-q2-2016.csv', sep = ',', header = T, quote = "")
dim(NYC_Q2_2016)
names(NYC_Q2_2016)
str(NYC_Q2_2016)

NYC_Q3_2016 = read.table('nyc-park-crime-stats-q3-2016.csv', sep = ',', header = T, quote = "")
dim(NYC_Q3_2016)
names(NYC_Q3_2016)
str(NYC_Q3_2016)

NYC_Q4_2016 = read.table('nyc-park-crime-stats-q4-2016.csv', sep = ',', header = T, quote = "")
dim(NYC_Q4_2016)
names(NYC_Q4_2016)
str(NYC_Q4_2016)

##2017
NYC_Q1_2017 = read.table('nyc-park-crime-stats-q1-2017.csv', sep = ',', header = T, quote = "")
dim(NYC_Q1_2017)
names(NYC_Q1_2017)
str(NYC_Q1_2017)

NYC_Q2_2017 = read.table('nyc-park-crime-stats-q2-2017.csv', sep = ',', header = T, quote = "")
dim(NYC_Q2_2017)
names(NYC_Q2_2017)
str(NYC_Q2_2017)

NYC_Q3_2017 = read.table('nyc-park-crime-stats-q3-2017.csv', sep = ',', header = T, quote = "")
dim(NYC_Q3_2017)
names(NYC_Q3_2017)
str(NYC_Q3_2017)

NYC_Q4_2017 = read.table('nyc-park-crime-stats-q4-2017.csv', sep = ',', header = T, quote = "")
dim(NYC_Q4_2017)
names(NYC_Q4_2017)
str(NYC_Q4_2017)

##2018
NYC_Q1_2018 = read.table('nyc-park-crime-stats-q1-2018.csv', sep = ',', header = T, quote = "")
dim(NYC_Q1_2018)
names(NYC_Q1_2018)
str(NYC_Q1_2018)

NYC_Q2_2018 = read.table('nyc-park-crime-stats-q2-2018.csv', sep = ',', header = T, quote = "")
dim(NYC_Q2_2018)
names(NYC_Q2_2018)
str(NYC_Q2_2018)

NYC_Q3_2018 = read.table('nyc-park-crime-stats-q3-2018.csv', sep = ',', header = T, quote = "")
dim(NYC_Q3_2018)
names(NYC_Q3_2018)
str(NYC_Q3_2018)

NYC_Q4_2018 = read.table('nyc-park-crime-stats-q4-2018.csv', sep = ',', header = T, quote = "")
dim(NYC_Q4_2018)
names(NYC_Q4_2018)
str(NYC_Q4_2018)

#DERIVE NEW VARIABLES: Year, Quarter for each data set to prepare for data joinning
##2015
NYC_Q1_2015$Year = 2015
NYC_Q1_2015$Quarter = 'Q1'
NYC_Q2_2015$Year = 2015
NYC_Q2_2015$Quarter = 'Q2'
NYC_Q3_2015$Year = 2015
NYC_Q3_2015$Quarter = 'Q3'
NYC_Q4_2015$Year = 2015
NYC_Q4_2015$Quarter = 'Q4'

##2016
NYC_Q1_2016$Year = 2016
NYC_Q1_2016$Quarter = 'Q1'
NYC_Q2_2016$Year = 2016
NYC_Q2_2016$Quarter = 'Q2'
NYC_Q3_2016$Year = 2016
NYC_Q3_2016$Quarter = 'Q3'
NYC_Q4_2016$Year = 2016
NYC_Q4_2016$Quarter = 'Q4'

##2017
NYC_Q1_2017$Year = 2017
NYC_Q1_2017$Quarter = 'Q1'
NYC_Q2_2017$Year = 2017
NYC_Q2_2017$Quarter = 'Q2'
NYC_Q3_2017$Year = 2017
NYC_Q3_2017$Quarter = 'Q3'
NYC_Q4_2017$Year = 2017
NYC_Q4_2017$Quarter = 'Q4'

##2018
NYC_Q1_2018$Year = 2018
NYC_Q1_2018$Quarter = 'Q1'
NYC_Q2_2018$Year = 2018
NYC_Q2_2018$Quarter = 'Q2'
NYC_Q3_2018$Year = 2018
NYC_Q3_2018$Quarter = 'Q3'
NYC_Q4_2018$Year = 2018
NYC_Q4_2018$Quarter = 'Q4'

#Rename columns for matching
colnames(NYC_Q1_2018)[colnames(NYC_Q1_2018) == "Murder"] = "MURDER"
colnames(NYC_Q3_2018)[colnames(NYC_Q3_2018) == "Murder"] = "MURDER"
colnames(NYC_Q4_2017)[colnames(NYC_Q4_2017) == "Murder"] = "MURDER"

#Append datasets for each year
NYC_2015 = do.call("rbind", list(NYC_Q1_2015, NYC_Q2_2015, NYC_Q3_2015, NYC_Q4_2015))
NYC_2016 = do.call("rbind", list(NYC_Q1_2016, NYC_Q2_2016, NYC_Q3_2016, NYC_Q4_2016))
NYC_2017 = do.call("rbind", list(NYC_Q1_2017, NYC_Q2_2017, NYC_Q3_2017, NYC_Q4_2017))
NYC_2018 = do.call("rbind", list(NYC_Q1_2018, NYC_Q2_2018, NYC_Q3_2018, NYC_Q4_2018))

# Combina data set of all years into 1 single data set
NYC_AllYears = do.call("rbind", list(NYC_2015, NYC_2016, NYC_2017, NYC_2018))
dim(NYC_AllYears)
names(NYC_AllYears)
str(NYC_AllYears)
head(NYC_AllYears)

#Rename columns to match naming conventions
colnames(NYC_AllYears)[colnames(NYC_AllYears) == "SIZE..ACRES."] = "SIZE"
colnames(NYC_AllYears)[colnames(NYC_AllYears) == "FELONY.ASSAULT"] = "FELONY_ASSAULT"
colnames(NYC_AllYears)[colnames(NYC_AllYears) == "GRAND.LARCENY"] = "GRAND_LARCENY"
colnames(NYC_AllYears)[colnames(NYC_AllYears) == "GRAND.LARCENY.OF.MOTOR.VEHICLE"] = "GRAND_LARCENY__MOTOR"

#Data cleaning - remove missing values
NYC_AllYears[!complete.cases(NYC_AllYears),]    #list of rows that have missing values
NYC_Final_Data = na.omit(NYC_AllYears)
NYC_Final_Data[!complete.cases(NYC_Final_Data),]

write.csv(NYC_Final_Data, file = "NYC_Consolidated.csv")

#Check for outliers
par(mfrow = c(1, 1)) #i.e., display 2 graphics with 1 row and 2 columns

hist(NYC_Final_Data$MURDER, main = "Histogram of Murder")
boxplot(NYC_Final_Data$MURDER, main = "Boxplot of Murder")

hist(NYC_Final_Data$RAPE, main = "Histogram of Rape")
boxplot(NYC_Final_Data$RAPE, main = "Boxplot of Rape")

hist(NYC_Final_Data$ROBBERY, main = "Histogram of Robbery")
boxplot(NYC_Final_Data$ROBBERY, main = "Boxplot of Robbery")

hist(NYC_Final_Data$FELONY_ASSAULT, main = "Histogram of Felony Assault")
boxplot(NYC_Final_Data$FELONY_ASSAULT, main = "Boxplot of Felony Assault")

hist(NYC_Final_Data$BURGLARY, main = "Histogram of Burglary")
boxplot(NYC_Final_Data$BURGLARY, main = "Boxplot of Burglary")

hist(NYC_Final_Data$GRAND_LARCENY, main = "Histogram of Grand Larceny")
boxplot(NYC_Final_Data$GRAND_LARCENY, main = "Boxplot of Grand Larceny")

hist(NYC_Final_Data$GRAND_LARCENY__MOTOR, main = "Histogram of Grand Larceny of Motor Vehicle")
boxplot(NYC_Final_Data$GRAND_LARCENY__MOTOR, main = "Boxplot of Grand Larceny of Motor Vehicle")

describe(NYC_Final_Data$SIZE)
describe(NYC_Final_Data$TOTAL)

hist(NYC_Final_Data$SIZE, main = "Histogram of Size")
boxplot(NYC_Final_Data$SIZE, main = "Boxplot of Size")

boxplot(NYC_Final_Data$TOTAL, main = "Boxplot of Total Number of Crimes")
hist(NYC_Final_Data$TOTAL, main = "Histogram of Total Number of Crimes")

plot(NYC_Final_Data$SIZE, NYC_Final_Data$TOTAL, main = "Scatterplot 
                           of Total Number of Crimes and Park Size")

#Adjustment for data types
str(NYC_Final_Data$TOTAL)
NYC_Final_Data$TOTAL = as.numeric(NYC_Final_Data$TOTAL)
str(NYC_Final_Data$TOTAL)


#Data transformation
# Derive a new variable name Size_thou_acre with unit= thousand acres
NYC_Final_Data$Size_thou_acre = NYC_Final_Data$SIZE / 1000

# Derive new variable: Crimes_per_Size which calculate 
# the total number of crimes per thousand acres
NYC_Final_Data$Crimes_per_Size = NYC_Final_Data$TOTAL / NYC_Final_Data$Size_thou_acre


boxplot(NYC_Final_Data$Size_thou_acre, main = "Boxplot of Size (thousand acres)")
hist(NYC_Final_Data$Size_thou_acre, main = "Histogram of Size (thousand acres)")
describe(NYC_Final_Data$Size_thou_acre)


boxplot(NYC_Final_Data$Crimes_per_Size, main = "Boxplot of Total Number of Crimes per thousand acres")
hist(NYC_Final_Data$Crimes_per_Size, main = "Histogram of Total Number of Crimes per thousand acres")
describe(NYC_Final_Data$Crimes_per_Size)

#Transforming text in to categories
str(NYC_Final_Data$Year)
str(NYC_Final_Data$Quarter)

NYC_Final_Data$Year = as.factor(NYC_Final_Data$Year)
NYC_Final_Data$Quarter = as.factor(NYC_Final_Data$Quarter)
str(NYC_Final_Data$Year)
str(NYC_Final_Data$Quarter)

#Check for normality
qqnorm(NYC_Final_Data$SIZE, main = "Size of the Park QQ-Plot")
qqline(NYC_Final_Data$SIZE, lty = 2)
#Anderson-Darling normality test
ad.test(NYC_Final_Data$SIZE)$p.value


# Data Reduction: PCA
NYC_data_pca = NYC_Final_Data[c("SIZE", "MURDER", "RAPE","ROBBERY", 
"FELONY_ASSAULT", "BURGLARY", "GRAND_LARCENY", "GRAND_LARCENY__MOTOR")]

NYC_pcamodel = princomp(NYC_data_pca, cor = TRUE)
NYC_pcamodel$sdev ^ 2

#Create a scree plot
plot(NYC_pcamodel, main = "Scree plot of crimes")

# Data Reduction: FA
NYC_data_FA = factanal(~SIZE + MURDER + RAPE + ROBBERY + FELONY_ASSAULT
                       + BURGLARY + GRAND_LARCENY + GRAND_LARCENY__MOTOR,
              factors = 3,
              rotation = "varimax",
              scores = "none",
              data = NYC_Final_Data)

NYC_data_FA

# Descriptive statistics
summary(NYC_Final_Data)
describe(NYC_Final_Data)
pairs(NYC_Final_Data, panel = panel.smooth)
barplot(c(NYC_Final_Data$BOROUGH, NYC_Final_Data$TOTAL), main = "Crimes across Borough")

##Summary of categorical variables
### Frequency count of each categorical variable
ggplot(NYC_Final_Data, aes(CATEGORY)) +
    geom_bar(fill = "#FC4E07") +
    coord_flip() +
    ggtitle("Frequency of Park Category")

ggplot(NYC_Final_Data, aes(BOROUGH)) +
    geom_bar(fill = "#0073C2FF") +
    ggtitle("Frequency of Park Borough")

### Distribution of Total crimes per each level of Borough
ggplot(NYC_Final_Data, aes(BOROUGH, TOTAL)) +
    geom_bar(stat = "sum", fill = "#00AFBB") +
    ggtitle("Number of Crimes per Borough")


#**************************************************************************
Sum_RAPE$Year_Quarter = paste(Sum_RAPE$Year, Sum_RAPE$Quarter)
Sum_RAPE$Num = c(1:16)
plot(Sum_RAPE$Num, Sum_RAPE$RAPE_TOTAL, type = 'l',
     main = "Trend of Rapes across all quarters in 2015-2018")
axis(1, at = Sum_RAPE$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))

Sum_ROBBERY$Year_Quarter = paste(Sum_ROBBERY$Year, Sum_ROBBERY$Quarter)
Sum_ROBBERY$Num = c(1:16)
plot(Sum_ROBBERY$Num, Sum_ROBBERY$ROBBERY_TOTAL, type = 'l',
     main = "Trend of Robberies across all quarters in 2015-2018")
axis(1, at = Sum_ROBBERY$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))

Sum_FELONY_ASSAULT$Year_Quarter = paste(Sum_FELONY_ASSAULT$Year, Sum_FELONY_ASSAULT$Quarter)
Sum_FELONY_ASSAULT$Num = c(1:16)
plot(Sum_FELONY_ASSAULT$Num, Sum_FELONY_ASSAULT$FELONY_ASSAULT_TOTAL, type = 'l',
     main = "Trend of Felony Assaults across all quarters in 2015-2018")
axis(1, at = Sum_FELONY_ASSAULT$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))

Sum_BURGLARY$Year_Quarter = paste(Sum_BURGLARY$Year, Sum_BURGLARY$Quarter)
Sum_BURGLARY$Num = c(1:16)
plot(Sum_BURGLARY$Num, Sum_BURGLARY$BURGLARY_TOTAL, type = 'l',
     main = "Trend of Burglaries across all quarters in 2015-2018")
axis(1, at = Sum_BURGLARY$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))

Sum_MURDER$Year_Quarter = paste(Sum_MURDER$Year, Sum_MURDER$Quarter)
Sum_MURDER$Num = c(1:16)
plot(Sum_MURDER$Num, Sum_MURDER$MURDER_TOTAL, type = 'l', xaxt = "n",
     main = "Trend of Murders across all quarters in 2015-2018")
axis(1, at = Sum_MURDER$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))


Sum_GRAND_LARCENY$Year_Quarter = paste(Sum_GRAND_LARCENY$Year, Sum_GRAND_LARCENY$Quarter)
Sum_GRAND_LARCENY$Num = c(1:16)
plot(Sum_GRAND_LARCENY$Num, Sum_GRAND_LARCENY$GRAND_LARCENY_TOTAL, type = 'l',
     main = "Trend of Grand Larceny across all quarters in 2015-2018")
axis(1, at = Sum_GRAND_LARCENY$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))


Sum_GRAND_LARCENY__MOTOR$Year_Quarter = paste(Sum_GRAND_LARCENY__MOTOR$Year,
                                              Sum_GRAND_LARCENY__MOTOR$Quarter)
Sum_GRAND_LARCENY__MOTOR$Num = c(1:16)
plot(Sum_GRAND_LARCENY__MOTOR$Num, Sum_GRAND_LARCENY__MOTOR$GRAND_LARCENY__MOTOR_TOTAL,
     type = 'l', main = "Trend of Grand Larceny of Motor Vehicles across all quarters in 2015-2018")
axis(1, at = Sum_GRAND_LARCENY__MOTOR$Num, labels = c("2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
                                       "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
                                       "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4",
                                       "2018-Q1", "2018-Q2", "2018-Q3", "2018-Q4"))

#**************************************************************************


Sum_RAPE <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(RAPE_TOTAL = sum(RAPE))

Sum_ROBBERY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(ROBBERY_TOTAL = sum(ROBBERY))

Sum_FELONY_ASSAULT <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(FELONY_ASSAULT_TOTAL = sum(FELONY_ASSAULT))

Sum_BURGLARY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(BURGLARY_TOTAL = sum(BURGLARY))

Sum_GRAND_LARCENY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(GRAND_LARCENY_TOTAL = sum(GRAND_LARCENY))

Sum_GRAND_LARCENY__MOTOR <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(GRAND_LARCENY__MOTOR_TOTAL = sum(GRAND_LARCENY__MOTOR))


## DEFINE FUNCTION TO FORECAST ERROR
plotForecastErrors = function(forecasterrors, forecasttitle) {
    #Function provided by Avril Coghlan
    forecasterrors = na.omit(forecasterrors)
    # make a histogram of the forecast errors:
    mybinsize = IQR(forecasterrors) / 4
    mysd = sd(forecasterrors)
    mymin = min(forecasterrors) - mysd * 5
    mymax = max(forecasterrors) + mysd * 3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean = 0, sd = mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main = forecasttitle)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}


#time series TOTAL
Sum_TOTAL <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(TOTAL = sum(TOTAL))

TOTAL_ts = ts(Sum_TOTAL$TOTAL,frequency = 4,start=2015)


plot(TOTAL_ts, main = 'Time Series of Total variable')

boxplot(TOTAL_ts ~ cycle(TOTAL_ts), main = 'Seasonality of Total variable')

TOTAL_ts_dc = decompose(TOTAL_ts)
plot(TOTAL_ts_dc)

TOTAL_hw = HoltWinters(TOTAL_ts)
TOTAL_hw

plot(TOTAL_hw)

#Forecast the model beyond the known range of data
TOTAL_hw_fore = forecast:::forecast.HoltWinters(TOTAL_hw)
TOTAL_hw_fore
#Look at forecasted values
plot(TOTAL_hw_fore)

#Assess constant variance
plot(TOTAL_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(TOTAL_hw_fore$residuals, 'Assessing Normal Distribution')

TOTAL_trendcomp = TOTAL_ts_dc$trend

TOTAL_trend_data = data.frame(trend = c(TOTAL_trendcomp), time = c(time(TOTAL_trendcomp)))

TOTAL_trend_reg = lm(TOTAL_trend_data$trend ~ TOTAL_trend_data$time)
summary(TOTAL_trend_reg)

adf.test(TOTAL_ts_trend, k = 1, alternative = "stationary")
kpss.test(TOTAL_ts_trend)

TOTAL_ts_trend = TOTAL_ts - TOTAL_ts_dc$seasonal

acf(na.omit(TOTAL_trendcomp), lag.max = 20)
pacf(na.omit(TOTAL_trendcomp), lag.max = 20)

plot(TOTAL_ts_trend, main = 'No Seasonality, only trend')

TOTAL_ts_diff = diff(TOTAL_ts_trend, differences = 4)
adf.test(TOTAL_ts_diff,k=0, alternative = "stationary")
kpss.test(TOTAL_ts_diff)
layout(1:1)
acf(TOTAL_ts_diff, lag.max = 20)
pacf(TOTAL_ts_diff, lag.max = 20)

TOTAL_arima = auto.arima(TOTAL_ts_diff)
TOTAL_arima
TOTAL_arima_fore = forecast(TOTAL_arima)
TOTAL_arima_fore
plot(TOTAL_arima_fore)

accuracy(TOTAL_arima_fore)
accuracy(TOTAL_hw_fore)


#**********************************************************************
#time series murder
Sum_MURDER <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(MURDER_TOTAL = sum(MURDER))

MURDER_ts = ts(Sum_MURDER$MURDER_TOTAL, frequency = 4, start = 2015)
plot(MURDER_ts, main = 'Time Series of MURDER variable')

boxplot(MURDER_ts ~ cycle(MURDER_ts), main = 'Seasonality of MURDER variable')

MURDER_ts_dc = decompose(MURDER_ts)
plot(MURDER_ts_dc)

MURDER_hw = HoltWinters(MURDER_ts)
MURDER_hw
plot(MURDER_hw)

#Forecast the model beyond the known range of data
MURDER_hw_fore = forecast:::forecast.HoltWinters(MURDER_hw)
MURDER_hw_fore
#Look at forecasted values
plot(MURDER_hw_fore)

#Assess constant variance
plot(MURDER_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(MURDER_hw_fore$residuals, 'Assessing Normal Distribution')

MURDER_trendcomp = MURDER_ts_dc$trend
MURDER_trend_data = data.frame(trend = c(MURDER_trendcomp), time = c(time(MURDER_trendcomp)))
MURDER_trend_reg = lm(MURDER_trend_data$trend ~ MURDER_trend_data$time)
summary(MURDER_trend_reg)

MURDER_ts_trend = MURDER_ts - MURDER_ts_dc$seasonal
adf.test(MURDER_ts_trend, k = 0, alternative = "stationary")
kpss.test(MURDER_ts_trend)

MURDER_ts_diff = diff(MURDER_ts_trend, differences = 4)
adf.test(MURDER_ts_diff, k = 0, alternative = "stationary")
kpss.test(MURDER_ts_diff)
layout(1:2)
acf(MURDER_ts_diff, lag.max = 20)
pacf(MURDER_ts_diff, lag.max = 20)


MURDER_arima = auto.arima(MURDER_ts_diff)
MURDER_arima
MURDER_arima_fore = forecast(MURDER_arima)
MURDER_arima_fore
layout(1:1)
plot(MURDER_arima_fore)

accuracy(MURDER_arima_fore)
accuracy(MURDER_hw_fore)

#*********************************************************************
#time series RAPE
Sum_RAPE <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(RAPE_TOTAL = sum(RAPE))

RAPE_ts = ts(Sum_RAPE$RAPE_TOTAL, frequency = 4, start = 2015)
plot(RAPE_ts, main = 'Time Series of RAPE variable')

boxplot(RAPE_ts ~ cycle(RAPE_ts), main = 'Seasonality of RAPE variable')

RAPE_ts_dc = decompose(RAPE_ts)
plot(RAPE_ts_dc)

RAPE_hw = HoltWinters(RAPE_ts)
RAPE_hw
plot(RAPE_hw)

#Forecast the model beyond the known range of data
RAPE_hw_fore = forecast:::forecast.HoltWinters(RAPE_hw)
RAPE_hw_fore
#Look at forecasted values
plot(RAPE_hw_fore)

#Assess constant variance
plot(RAPE_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(RAPE_hw_fore$residuals, 'Assessing Normal Distribution')

RAPE_trendcomp = RAPE_ts_dc$trend
RAPE_trend_data = data.frame(trend = c(RAPE_trendcomp), time = c(time(RAPE_trendcomp)))
RAPE_trend_reg = lm(RAPE_trend_data$trend ~ RAPE_trend_data$time)
summary(RAPE_trend_reg)

RAPE_ts_trend = RAPE_ts - RAPE_ts_dc$seasonal
adf.test(RAPE_ts_trend, k = 1, alternative = "stationary")
kpss.test(RAPE_ts_trend)

RAPE_ts_diff = diff(RAPE_ts_trend, differences = 3)
adf.test(RAPE_ts_diff, k = 1, alternative = "stationary")
kpss.test(RAPE_ts_diff)

plot(RAPE_ts_trend, main = 'No Seasonality, only trend')

layout(1:2)
acf(RAPE_ts_diff, lag.max = 20)
pacf(RAPE_ts_diff, lag.max = 20)

RAPE_arima = auto.arima(RAPE_ts_diff)
RAPE_arima
RAPE_arima_fore = forecast(RAPE_arima)
RAPE_arima_fore
layout(1:1)
plot(RAPE_arima_fore)

accuracy(RAPE_arima_fore)
accuracy(RAPE_hw_fore)
#********************************************************************

#time series ROBBERY
Sum_ROBBERY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(ROBBERY_TOTAL = sum(ROBBERY))

ROBBERY_ts = ts(Sum_ROBBERY$ROBBERY_TOTAL, frequency = 4, start = 2015)
plot(ROBBERY_ts, main = 'Time Series of ROBBERY variable')

boxplot(ROBBERY_ts ~ cycle(ROBBERY_ts), main = 'Seasonality of ROBBERY variable')

ROBBERY_ts_dc = decompose(ROBBERY_ts)
plot(ROBBERY_ts_dc)

ROBBERY_hw = HoltWinters(ROBBERY_ts)
ROBBERY_hw
plot(ROBBERY_hw)

#Forecast the model beyond the known range of data
ROBBERY_hw_fore = forecast:::forecast.HoltWinters(ROBBERY_hw)
ROBBERY_hw_fore
#Look at forecasted values
plot(ROBBERY_hw_fore)

#Assess constant variance
plot(ROBBERY_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(ROBBERY_hw_fore$residuals, 'Assessing Normal Distribution')

ROBBERY_trendcomp = ROBBERY_ts_dc$trend
ROBBERY_trend_data = data.frame(trend = c(ROBBERY_trendcomp), time = c(time(ROBBERY_trendcomp)))
ROBBERY_trend_reg = lm(ROBBERY_trend_data$trend ~ ROBBERY_trend_data$time)
summary(ROBBERY_trend_reg)

ROBBERY_ts_trend = ROBBERY_ts - ROBBERY_ts_dc$seasonal
adf.test(ROBBERY_ts_trend, k =0 , alternative = "stationary")
kpss.test(ROBBERY_ts_trend)

ROBBERY_ts_diff = diff(ROBBERY_ts_trend, differences = 1)
adf.test(ROBBERY_ts_diff, k = 1, alternative = "stationary")
kpss.test(ROBBERY_ts_diff)

layout(1:2)
acf(ROBBERY_ts_diff, lag.max = 20)
pacf(ROBBERY_ts_diff, lag.max = 20)

ROBBERY_arima = auto.arima(ROBBERY_ts_trend)
ROBBERY_arima

ROBBERY_arima_fore = forecast(ROBBERY_arima)
ROBBERY_arima_fore
layout(1:1)
plot(ROBBERY_arima_fore)

accuracy(ROBBERY_arima_fore)
accuracy(ROBBERY_hw_fore)
#*********************************************************************

#time series FELONY_ASSAULT
Sum_FELONY_ASSAULT <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(FELONY_ASSAULT_TOTAL = sum(FELONY_ASSAULT))

FELONY_ASSAULT_ts = ts(Sum_FELONY_ASSAULT$FELONY_ASSAULT_TOTAL, frequency = 4, start = 2015)
plot(FELONY_ASSAULT_ts, main = 'Time Series of FELONY_ASSAULT variable')

boxplot(FELONY_ASSAULT_ts ~ cycle(FELONY_ASSAULT_ts), main = 'Seasonality of FELONY_ASSAULT variable')

FELONY_ASSAULT_ts_dc = decompose(FELONY_ASSAULT_ts)
plot(FELONY_ASSAULT_ts_dc)

FELONY_ASSAULT_hw = HoltWinters(FELONY_ASSAULT_ts)
FELONY_ASSAULT_hw
plot(FELONY_ASSAULT_hw)

#Forecast the model beyond the known range of data
FELONY_ASSAULT_hw_fore = forecast:::forecast.HoltWinters(FELONY_ASSAULT_hw)
FELONY_ASSAULT_hw_fore
#Look at forecasted values
plot(FELONY_ASSAULT_hw_fore)

#Assess constant variance
plot(FELONY_ASSAULT_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(FELONY_ASSAULT_hw_fore$residuals, 'Assessing Normal Distribution')

FELONY_ASSAULT_trendcomp = FELONY_ASSAULT_ts_dc$trend
FELONY_ASSAULT_trend_data = data.frame(trend = c(FELONY_ASSAULT_trendcomp), time = c(time(FELONY_ASSAULT_trendcomp)))
FELONY_ASSAULT_trend_reg = lm(FELONY_ASSAULT_trend_data$trend ~ FELONY_ASSAULT_trend_data$time)
summary(FELONY_ASSAULT_trend_reg)

FELONY_ASSAULT_ts_trend = FELONY_ASSAULT_ts - FELONY_ASSAULT_ts_dc$seasonal
adf.test(FELONY_ASSAULT_ts_trend, k = 1, alternative = "stationary")
kpss.test(FELONY_ASSAULT_ts_trend)

FELONY_ASSAULT_ts_diff = diff(FELONY_ASSAULT_ts_trend, differences = 1)
adf.test(FELONY_ASSAULT_ts_diff, k = 2, alternative = "stationary")
kpss.test(FELONY_ASSAULT_ts_diff)

layout(1:2)
acf(FELONY_ASSAULT_ts_diff, lag.max = 20)
pacf(FELONY_ASSAULT_ts_diff, lag.max = 20)

FELONY_ASSAULT_arima = auto.arima(FELONY_ASSAULT_ts_diff)
FELONY_ASSAULT_arima

FELONY_ASSAULT_arima_fore = forecast(FELONY_ASSAULT_arima)
FELONY_ASSAULT_arima_fore
layout(1:1)
plot(FELONY_ASSAULT_arima_fore)

accuracy(FELONY_ASSAULT_arima_fore)
accuracy(FELONY_ASSAULT_hw_fore)

#*********************************************************************

#time series BURGLARY
Sum_BURGLARY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(BURGLARY_TOTAL = sum(BURGLARY))

BURGLARY_ts = ts(Sum_BURGLARY$BURGLARY_TOTAL, frequency = 4, start = 2015)
plot(BURGLARY_ts, main = 'Time Series of BURGLARY variable')

boxplot(BURGLARY_ts ~ cycle(BURGLARY_ts), main = 'Seasonality of BURGLARY variable')

BURGLARY_ts_dc = decompose(BURGLARY_ts)
plot(BURGLARY_ts_dc)

BURGLARY_hw = HoltWinters(BURGLARY_ts)
BURGLARY_hw
plot(BURGLARY_hw)

#Forecast the model beyond the known range of data
BURGLARY_hw_fore = forecast:::forecast.HoltWinters(BURGLARY_hw)
BURGLARY_hw_fore
#Look at forecasted values
plot(BURGLARY_hw_fore)

#Assess constant variance
plot(BURGLARY_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(BURGLARY_hw_fore$residuals, 'Assessing Normal Distribution')

BURGLARY_trendcomp = BURGLARY_ts_dc$trend
BURGLARY_trend_data = data.frame(trend = c(BURGLARY_trendcomp), time = c(time(BURGLARY_trendcomp)))
BURGLARY_trend_reg = lm(BURGLARY_trend_data$trend ~ BURGLARY_trend_data$time)
summary(BURGLARY_trend_reg)

BURGLARY_ts_trend = BURGLARY_ts - BURGLARY_ts_dc$seasonal
adf.test(BURGLARY_ts_trend, k = 5, alternative = "stationary")
kpss.test(BURGLARY_ts_trend)

BURGLARY_ts_diff = diff(BURGLARY_ts_trend, differences = 1)
adf.test(BURGLARY_ts_diff, k = 5, alternative = "stationary")
kpss.test(BURGLARY_ts_diff)

layout(1:2)
acf(BURGLARY_ts_trend, lag.max = 20)
pacf(BURGLARY_ts_trend, lag.max = 20)

BURGLARY_arima = auto.arima(BURGLARY_ts_trend)
BURGLARY_arima

BURGLARY_arima_fore = forecast(BURGLARY_arima)
BURGLARY_arima_fore
layout(1:1)
plot(BURGLARY_arima_fore)

accuracy(BURGLARY_arima_fore)
accuracy(BURGLARY_hw_fore)

#*********************************************************************

#time series GRAND_LARCENY
Sum_GRAND_LARCENY <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(GRAND_LARCENY_TOTAL = sum(GRAND_LARCENY))

GRAND_LARCENY_ts = ts(Sum_GRAND_LARCENY$GRAND_LARCENY_TOTAL, frequency = 4, start = 2015)
plot(GRAND_LARCENY_ts, main = 'Time Series of GRAND_LARCENY variable')

boxplot(GRAND_LARCENY_ts ~ cycle(GRAND_LARCENY_ts), main = 'Seasonality of GRAND_LARCENY variable')

GRAND_LARCENY_ts_dc = decompose(GRAND_LARCENY_ts)
plot(GRAND_LARCENY_ts_dc)

GRAND_LARCENY_hw = HoltWinters(GRAND_LARCENY_ts)
GRAND_LARCENY_hw
plot(GRAND_LARCENY_hw)

#Forecast the model beyond the known range of data
GRAND_LARCENY_hw_fore = forecast:::forecast.HoltWinters(GRAND_LARCENY_hw)
GRAND_LARCENY_hw_fore
#Look at forecasted values
plot(GRAND_LARCENY_hw_fore)

#Assess constant variance
plot(GRAND_LARCENY_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(GRAND_LARCENY_hw_fore$residuals, 'Assessing Normal Distribution')

GRAND_LARCENY_trendcomp = GRAND_LARCENY_ts_dc$trend
GRAND_LARCENY_trend_data = data.frame(trend = c(GRAND_LARCENY_trendcomp), time = c(time(GRAND_LARCENY_trendcomp)))
GRAND_LARCENY_trend_reg = lm(GRAND_LARCENY_trend_data$trend ~ GRAND_LARCENY_trend_data$time)
summary(GRAND_LARCENY_trend_reg)

GRAND_LARCENY_ts_trend = GRAND_LARCENY_ts - GRAND_LARCENY_ts_dc$seasonal
adf.test(GRAND_LARCENY_ts_trend, k = 5, alternative = "stationary")
kpss.test(GRAND_LARCENY_ts_trend)

GRAND_LARCENY_ts_diff = diff(GRAND_LARCENY_ts_trend, differences = 4)
adf.test(GRAND_LARCENY_ts_diff, k = 0, alternative = "stationary")
kpss.test(GRAND_LARCENY_ts_diff)

layout(1:2)
acf(GRAND_LARCENY_ts_diff, lag.max = 20)
pacf(GRAND_LARCENY_ts_diff, lag.max = 20)

GRAND_LARCENY_arima = auto.arima(GRAND_LARCENY_ts_diff)
GRAND_LARCENY_arima

GRAND_LARCENY_arima_fore = forecast(GRAND_LARCENY_arima)
GRAND_LARCENY_arima_fore
layout(1:1)
plot(GRAND_LARCENY_arima_fore)

accuracy(GRAND_LARCENY_arima_fore)
accuracy(GRAND_LARCENY_hw_fore)

#*********************************************************************

#time series GRAND_LARCENY__MOTOR
Sum_GRAND_LARCENY__MOTOR <- NYC_Final_Data %>%
    group_by(Year, Quarter) %>%
    summarise(GRAND_LARCENY__MOTOR_TOTAL = sum(GRAND_LARCENY__MOTOR))

GRAND_LARCENY__MOTOR_ts = ts(Sum_GRAND_LARCENY__MOTOR$GRAND_LARCENY__MOTOR_TOTAL, frequency = 4, start = 2015)
plot(GRAND_LARCENY__MOTOR_ts, main = 'Time Series of GRAND_LARCENY__MOTOR variable')

boxplot(GRAND_LARCENY__MOTOR_ts ~ cycle(GRAND_LARCENY__MOTOR_ts), main = 'Seasonality of GRAND_LARCENY__MOTOR variable')

GRAND_LARCENY__MOTOR_ts_dc = decompose(GRAND_LARCENY__MOTOR_ts)
plot(GRAND_LARCENY__MOTOR_ts_dc)

GRAND_LARCENY__MOTOR_hw = HoltWinters(GRAND_LARCENY__MOTOR_ts)
GRAND_LARCENY__MOTOR_hw
plot(GRAND_LARCENY__MOTOR_hw)

#Forecast the model beyond the known range of data
GRAND_LARCENY__MOTOR_hw_fore = forecast:::forecast.HoltWinters(GRAND_LARCENY__MOTOR_hw)
GRAND_LARCENY__MOTOR_hw_fore
#Look at forecasted values
plot(GRAND_LARCENY__MOTOR_hw_fore)

#Assess constant variance
plot(GRAND_LARCENY__MOTOR_hw_fore$residuals)
lines(c(0, 3000), c(0, 0), col = 'red')

#Assess normality of residuals
plotForecastErrors(GRAND_LARCENY__MOTOR_hw_fore$residuals, 'Assessing Normal Distribution')

GRAND_LARCENY__MOTOR_trendcomp = GRAND_LARCENY__MOTOR_ts_dc$trend
GRAND_LARCENY__MOTOR_trend_data = data.frame(trend = c(GRAND_LARCENY__MOTOR_trendcomp), time = c(time(GRAND_LARCENY__MOTOR_trendcomp)))
GRAND_LARCENY__MOTOR_trend_reg = lm(GRAND_LARCENY__MOTOR_trend_data$trend ~ GRAND_LARCENY__MOTOR_trend_data$time)
summary(GRAND_LARCENY__MOTOR_trend_reg)

GRAND_LARCENY__MOTOR_ts_trend = GRAND_LARCENY__MOTOR_ts - GRAND_LARCENY__MOTOR_ts_dc$seasonal
adf.test(GRAND_LARCENY__MOTOR_ts_trend, k = 3, alternative = "stationary")
kpss.test(GRAND_LARCENY__MOTOR_ts_trend)

GRAND_LARCENY__MOTOR_ts_diff =
    diff(GRAND_LARCENY__MOTOR_ts_trend, differences = 2)
adf.test(GRAND_LARCENY__MOTOR_ts_diff, k = 3, alternative = "stationary")
kpss.test(GRAND_LARCENY__MOTOR_ts_diff)

layout(1:2)
acf(GRAND_LARCENY__MOTOR_ts_diff, lag.max = 20)
pacf(GRAND_LARCENY__MOTOR_ts_diff, lag.max = 20)

GRAND_LARCENY__MOTOR_arima = auto.arima(GRAND_LARCENY__MOTOR_ts_diff)
GRAND_LARCENY__MOTOR_arima

GRAND_LARCENY__MOTOR_arima_fore = forecast(GRAND_LARCENY__MOTOR_arima)
GRAND_LARCENY__MOTOR_arima_fore
layout(1:1)
plot(GRAND_LARCENY__MOTOR_arima_fore)

accuracy(GRAND_LARCENY__MOTOR_arima_fore)
accuracy(GRAND_LARCENY__MOTOR_hw_fore)


## LINEAR REGRESSION MODEL
reg_data = NYC_Final_Data[complete.cases(NYC_Final_Data),]
reg_data = na.omit(reg_data)

reg = lm(NYC_Final_Data$TOTAL ~ reg_data$MURDER + reg_data$RAPE + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR)

#checking assumptions
par(mfrow = c(2, 2))
plot(reg)
library(ggfortify)
autoplot(reg)
VIF(lm(reg_data$MURDER ~ reg_data$RAPE + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$RAPE ~ reg_data$MURDER + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$ROBBERY ~ reg_data$MURDER + reg_data$RAPE + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$FELONY_ASSAULT ~ reg_data$MURDER + reg_data$RAPE + reg_data$ROBBERY + reg_data$BURGLARY + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$BURGLARY ~ reg_data$MURDER + reg_data$RAPE + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$GRAND_LARCENY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$GRAND_LARCENY ~ reg_data$MURDER + reg_data$RAPE + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY_MOTOR))
VIF(lm(reg_data$GRAND_LARCENY_MOTOR ~ reg_data$MURDER + reg_data$RAPE + reg_data$ROBBERY + reg_data$FELONY_ASSAULT + reg_data$BURGLARY + reg_data$GRAND_LARCENY))

##data split 
df = reg_data
fractionTraining <- 0.80
fractionValidation <- 0.20

# Compute sample sizes.
sampleSizeTraining <- floor(fractionTraining * nrow(df))
sampleSizeValidation <- floor(fractionValidation * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining <- sort(sample(seq_len(nrow(df)), size = sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation <- sort(sample(indicesNotTraining, size = sampleSizeValidation))

# Finally, output the three dataframes for training, validation and test.
dfTraining <- df[indicesTraining,]
dfValidation <- df[indicesValidation,]

##accuracy of model
distPred <- predict(reg, dfValidation)
actuals_preds <- data.frame(cbind(actuals = reg_data$TOTAL, predicteds = distPred)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

##Residual sum of squares, Mean squared error and Root MSE
RSS = c(crossprod(reg$residuals))
MSE <- RSS / length(reg$residuals)
RMSE <- sqrt(MSE)
