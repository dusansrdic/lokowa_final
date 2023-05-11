
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
WBD <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="WBD")
names(WBD)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

WBD_descriptive_statistics <- data.frame(open=mean(WBD$Open),high=mean(WBD$High),low=mean(WBD$Low),last=mean(WBD$Last)) 
WBD_descriptive_statistics[2,] <- c(median(WBD$Open),median(WBD$High),median(WBD$Low),median(WBD$Last))
library(moments)
WBD_descriptive_statistics[3,] <- c(skewness(WBD$Open),skewness(WBD$High),skewness(WBD$Low),skewness(WBD$Last))
WBD_descriptive_statistics[4,] <- c(kurtosis(WBD$Open),kurtosis(WBD$High),kurtosis(WBD$Low),kurtosis(WBD$Last))

rownames(WBD_descriptive_statistics) <- c("MWBDN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
WBD$LogReturn_Open<-0
WBD$LogReturn_High<-0
WBD$LogReturn_Low<-0
WBD$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD)){
  WBD$LogReturn_Open[i]<-log(WBD$Open[i]/WBD$Open[i-1])
  WBD$LogReturn_High[i]<-log(WBD$High[i]/WBD$High[i-1])
  WBD$LogReturn_Low[i]<-log(WBD$Low[i]/WBD$Low[i-1])
  WBD$LogReturn_Last[i]<-log(WBD$Last[i]/WBD$Last[i-1])
}

#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
WBD$NetReturn_Open<-0
WBD$NetReturn_High<-0
WBD$NetReturn_Low<-0
WBD$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD)){
  WBD$NetReturn_Open[i]<-(WBD$Open[i]-WBD$Open[i-1])/WBD$Open[i-1]
  WBD$NetReturn_High[i]<-(WBD$High[i]-WBD$High[i-1])/WBD$High[i-1]
  WBD$NetReturn_Low[i]<-(WBD$Low[i]-WBD$Low[i-1])/WBD$Low[i-1]
  WBD$NetReturn_Last[i]<-(WBD$Last[i]-WBD$Last[i-1])/WBD$Last[i-1]
}
# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
WBD$week <- week(WBD$Date)
WBD$year <- year(WBD$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
WBD_weekly <- WBD %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(WBD_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

WBD_weekly$LogReturn_Open<-0
WBD_weekly$LogReturn_High<-0
WBD_weekly$LogReturn_Low<-0
WBD_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_weekly)){
  WBD_weekly$LogReturn_Open[i]<-log(WBD_weekly$weekly_open[i]/WBD_weekly$weekly_open[i-1])
  WBD_weekly$LogReturn_High[i]<-log(WBD_weekly$weekly_high[i]/WBD_weekly$weekly_high[i-1])
  WBD_weekly$LogReturn_Low[i]<-log(WBD_weekly$weekly_low[i]/WBD_weekly$weekly_low[i-1])
  WBD_weekly$LogReturn_Close[i]<-log(WBD_weekly$weekly_close[i]/WBD_weekly$weekly_close[i-1])
}


#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
WBD_weekly$NetReturn_Open<-0
WBD_weekly$NetReturn_High<-0
WBD_weekly$NetReturn_Low<-0
WBD_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_weekly)){
  WBD_weekly$NetReturn_Open[i]<-(WBD_weekly$weekly_open[i]-WBD_weekly$weekly_open[i-1])/WBD_weekly$weekly_open[i-1]
  WBD_weekly$NetReturn_High[i]<-(WBD_weekly$weekly_high[i]-WBD_weekly$weekly_high[i-1])/WBD_weekly$weekly_high[i-1]
  WBD_weekly$NetReturn_Low[i]<-(WBD_weekly$weekly_low[i]-WBD_weekly$weekly_low[i-1])/WBD_weekly$weekly_low[i-1]
  WBD_weekly$NetReturn_Last[i]<-(WBD_weekly$weekly_close[i]-WBD_weekly$weekly_close[i-1])/WBD_weekly$weekly_close[i-1]
}


# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
WBD$month <- month(WBD$Date)
WBD$year <- year(WBD$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
WBD_monthly <- WBD %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
WBD_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
WBD_monthly$LogReturn_Open<-0
WBD_monthly$LogReturn_High<-0
WBD_monthly$LogReturn_Low<-0
WBD_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_monthly)){
  WBD_monthly$LogReturn_Open[i]<-log(WBD_monthly$monthly_open[i]/WBD_monthly$monthly_open[i-1])
  WBD_monthly$LogReturn_High[i]<-log(WBD_monthly$monthly_high[i]/WBD_monthly$monthly_high[i-1])
  WBD_monthly$LogReturn_Low[i]<-log(WBD_monthly$monthly_low[i]/WBD_monthly$monthly_low[i-1])
  WBD_monthly$LogReturn_Close[i]<-log(WBD_monthly$monthly_close[i]/WBD_monthly$monthly_close[i-1])
}

## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
WBD_monthly$NetReturn_Open<-0
WBD_monthly$NetReturn_High<-0
WBD_monthly$NetReturn_Low<-0
WBD_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_monthly)){
  WBD_monthly$NetReturn_Open[i]<-(WBD_monthly$monthly_open[i]-WBD_monthly$monthly_open[i-1])/WBD_monthly$monthly_open[i-1]
  WBD_monthly$NetReturn_High[i]<-(WBD_monthly$monthly_high[i]-WBD_monthly$monthly_high[i-1])/WBD_monthly$monthly_high[i-1]
  WBD_monthly$NetReturn_Low[i]<-(WBD_monthly$monthly_low[i]-WBD_monthly$monthly_low[i-1])/WBD_monthly$monthly_low[i-1]
  WBD_monthly$NetReturn_Last[i]<-(WBD_monthly$monthly_close[i]-WBD_monthly$monthly_close[i-1])/WBD_monthly$monthly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
WBD_yearly <- WBD %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
WBD_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
WBD_yearly$LogReturn_Open<-0
WBD_yearly$LogReturn_High<-0
WBD_yearly$LogReturn_Low<-0
WBD_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_yearly)){
  WBD_yearly$LogReturn_Open[i]<-log(WBD_yearly$yearly_open[i]/WBD_yearly$yearly_open[i-1])
  WBD_yearly$LogReturn_High[i]<-log(WBD_yearly$yearly_high[i]/WBD_yearly$yearly_high[i-1])
  WBD_yearly$LogReturn_Low[i]<-log(WBD_yearly$yearly_low[i]/WBD_yearly$yearly_low[i-1])
  WBD_yearly$LogReturn_Close[i]<-log(WBD_yearly$yearly_close[i]/WBD_yearly$yearly_close[i-1])
}

#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
WBD_yearly$NetReturn_Open<-0
WBD_yearly$NetReturn_High<-0
WBD_yearly$NetReturn_Low<-0
WBD_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD_yearly)){
  WBD_yearly$NetReturn_Open[i]<-(WBD_yearly$yearly_open[i]-WBD_yearly$yearly_open[i-1])/WBD_yearly$yearly_open[i-1]
  WBD_yearly$NetReturn_High[i]<-(WBD_yearly$yearly_high[i]-WBD_yearly$yearly_high[i-1])/WBD_yearly$yearly_high[i-1]
  WBD_yearly$NetReturn_Low[i]<-(WBD_yearly$yearly_low[i]-WBD_yearly$yearly_low[i-1])/WBD_yearly$yearly_low[i-1]
  WBD_yearly$NetReturn_Last[i]<-(WBD_yearly$yearly_close[i]-WBD_yearly$yearly_close[i-1])/WBD_yearly$yearly_close[i-1]
}


#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
WBD_yearly$NetReturns_Open_UK <- "/"
WBD_yearly$NetReturns_Open_UK[1] <- sd(WBD_yearly$NetReturn_Open)
WBD_yearly$NetReturns_High_UK <- "/"
WBD_yearly$NetReturns_High_UK[1] <- sd(WBD_yearly$NetReturn_High)
WBD_yearly$NetReturns_Low_UK<- "/"
WBD_yearly$NetReturns_Low_UK[1] <- sd(WBD_yearly$NetReturn_Low)
WBD_yearly$NetReturns_Last_UK<- "/"
WBD_yearly$NetReturns_Last_UK[1] <- sd(WBD_yearly$NetReturn_Last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#WBD<-WBD[,10]
#install.packages("quantmod")
library(quantmod)

WBD<-WBD[,-10]
WBD_yearly$Volatility_Open <- "/"
WBD_yearly$Volatility_Open[1] <- sd(WBD_yearly$LogReturn_Open)
WBD_yearly$Volatility_High <- "/"
WBD_yearly$Volatility_High[1] <- sd(WBD_yearly$LogReturn_High)
WBD_yearly$Volatility_Low <- "/"
WBD_yearly$Volatility_Low[1] <- sd(WBD_yearly$LogReturn_Low)
WBD_yearly$Volatility_Last <- "/"
WBD_yearly$Volatility_Last[1] <- sd(WBD_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
WBD_yearly_volatility <- WBD %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
WBD_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(WBD)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(WBD[1])<-"Date"

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD <- plot_ly(data = WBD, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD <- fig_WBD %>% layout(title = "WBD Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# WBDplay the chart
fig_WBD

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD_lr_d <- plot_ly(data = WBD, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_d <- fig_WBD_lr_d %>% layout(title = "WBD Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# WBDplay the chart
fig_WBD_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD_lr_w <- plot_ly(data = WBD_weekly, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_w <- fig_WBD_lr_w %>% layout(title = "WBD Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# WBDplay the chart
fig_WBD_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
WBD_monthly$month<- as.character(WBD_monthly$month)

# Create a candlestick chart
fig_WBD_lr_m <- plot_ly(data = WBD_monthly, type = "candlestick",
                          x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_m <- fig_WBD_lr_m %>% layout(title = "WBD Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# WBDplay the chart
fig_WBD_lr_m

# YWBDRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD_yearly$year<- as.character(WBD_yearly$year)

# Create a candlestick chart
fig_WBD_lr_y <- plot_ly(data = WBD_yearly, type = "candlestick",
                          x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_y <- fig_WBD_lr_y %>% layout(title = "WBD Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# WBDplay the chart
fig_WBD_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
WBD$MA5 <- rollmean(WBD$Last, k = 5, fill = NA)
WBD$MA21 <- rollmean(WBD$Last, k = 21, fill = NA)
WBD$MA63 <- rollmean(WBD$Last, k = 63, fill = NA)
WBD$MA126 <- rollmean(WBD$Last, k = 126, fill = NA)
WBD$MA252 <- rollmean(WBD$Last, k = 252, fill = NA)

ggplot(WBD, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

#------------------
ggplot(WBD, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "solid") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "solid") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "solid") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "solid") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "solid") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))


#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------

# prinose (i log i net) iscrtati na line grafiku sa 5 podgrafika:
# prinos open cene
# prinos high
# prinos low
# prinos close
# prinos candlestick (OVAJ DEO NE MOZE DA SE URADI, NE MOGU DA NADJEM NACIN DA SPOJIM LINECHART SA CANDLESTICK CHARTOM)

#DNEVNI
# Grafikon (Log Return)
plot(WBD$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="WBD Open, Close, High i Low Log Return")
lines(WBD$LogReturn_High, type="l", col="blue")
lines(WBD$LogReturn_Low, type="l", col="green")
lines(WBD$LogReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

##### sa candlestickom ---------NE RADI

# Učitavanje potrebnih paketa
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("tidyquant")
library(tidyquant)

#****-------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshapeovanje podataka
WBD.m <- melt(WBD[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(WBD.m, aes(Date, value)) +
  geom_line(data = subset(WBD.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = WBD, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "WBD Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(WBD$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="WBD Open, Close, High i Low Net Return")
lines(WBD$NetReturn_High, type="l", col="blue")
lines(WBD$NetReturn_Low, type="l", col="green")
lines(WBD$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)



