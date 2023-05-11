
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
NASDAQ <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NASDAQ")
#meni je bio cudan format datuma pa sam sve pretvarala ---------------
names(NASDAQ) <- c("Date", "Open", "High", "Low", "Last")
NASDAQ <- NASDAQ[-1,]
library(openxlsx)
NASDAQ$Date <- convertToDateTime(NASDAQ$Date)
NASDAQ$Open <- as.numeric(NASDAQ$Open)
NASDAQ$High <- as.numeric(NASDAQ$High)
NASDAQ$Low <- as.numeric(NASDAQ$Low)
NASDAQ$Last <- as.numeric(NASDAQ$Last)
#----------------------------------------------------------------------

# DESKRIPTIVNA STATISTIKA


NASDAQ_descriptive_statistics <- data.frame(open=mean(NASDAQ$Open),high=mean(NASDAQ$High),low=mean(NASDAQ$Low),last=mean(NASDAQ$Last)) 
NASDAQ_descriptive_statistics[2,] <- c(median(NASDAQ$Open),median(NASDAQ$High),median(NASDAQ$Low),median(NASDAQ$Last))
library(moments)
NASDAQ_descriptive_statistics[3,] <- c(skewness(NASDAQ$Open),skewness(NASDAQ$High),skewness(NASDAQ$Low),skewness(NASDAQ$Last))
NASDAQ_descriptive_statistics[4,] <- c(kurtosis(NASDAQ$Open),kurtosis(NASDAQ$High),kurtosis(NASDAQ$Low),kurtosis(NASDAQ$Last))

rownames(NASDAQ_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NASDAQ$LogReturn_Open<-0
NASDAQ$LogReturn_High<-0
NASDAQ$LogReturn_Low<-0
NASDAQ$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ)){
  NASDAQ$LogReturn_Open[i]<-log(NASDAQ$Open[i]/NASDAQ$Open[i-1])
  NASDAQ$LogReturn_High[i]<-log(NASDAQ$High[i]/NASDAQ$High[i-1])
  NASDAQ$LogReturn_Low[i]<-log(NASDAQ$Low[i]/NASDAQ$Low[i-1])
  NASDAQ$LogReturn_Last[i]<-log(NASDAQ$Last[i]/NASDAQ$Last[i-1])
}

#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)
#prvo moraju da se naprave kolone u dataframe-u
NASDAQ$NetReturn_Open<-0
NASDAQ$NetReturn_High<-0
NASDAQ$NetReturn_Low<-0
NASDAQ$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ)){
  NASDAQ$NetReturn_Open[i]<-(NASDAQ$Open[i]-NASDAQ$Open[i-1])/NASDAQ$Open[i-1]
  NASDAQ$NetReturn_High[i]<-(NASDAQ$High[i]-NASDAQ$High[i-1])/NASDAQ$High[i-1]
  NASDAQ$NetReturn_Low[i]<-(NASDAQ$Low[i]-NASDAQ$Low[i-1])/NASDAQ$Low[i-1]
  NASDAQ$NetReturn_Last[i]<-(NASDAQ$Last[i]-NASDAQ$Last[i-1])/NASDAQ$Last[i-1]
}
# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
NASDAQ$week <- week(NASDAQ$Date)
NASDAQ$year <- year(NASDAQ$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
NASDAQ_weekly <- NASDAQ %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(NASDAQ_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

NASDAQ_weekly$LogReturn_Open<-0
NASDAQ_weekly$LogReturn_High<-0
NASDAQ_weekly$LogReturn_Low<-0
NASDAQ_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_weekly)){
  NASDAQ_weekly$LogReturn_Open[i]<-log(NASDAQ_weekly$weekly_open[i]/NASDAQ_weekly$weekly_open[i-1])
  NASDAQ_weekly$LogReturn_High[i]<-log(NASDAQ_weekly$weekly_high[i]/NASDAQ_weekly$weekly_high[i-1])
  NASDAQ_weekly$LogReturn_Low[i]<-log(NASDAQ_weekly$weekly_low[i]/NASDAQ_weekly$weekly_low[i-1])
  NASDAQ_weekly$LogReturn_Close[i]<-log(NASDAQ_weekly$weekly_close[i]/NASDAQ_weekly$weekly_close[i-1])
}

#nedeljni prinos

NASDAQ_weekly$NetReturn_Open<-0
NASDAQ_weekly$NetReturn_High<-0
NASDAQ_weekly$NetReturn_Low<-0
NASDAQ_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_weekly)){
  NASDAQ_weekly$NetReturn_Open[i]<-(NASDAQ_weekly$weekly_open[i]-NASDAQ_weekly$weekly_open[i-1])/NASDAQ_weekly$weekly_open[i-1]
  NASDAQ_weekly$NetReturn_High[i]<-(NASDAQ_weekly$weekly_high[i]-NASDAQ_weekly$weekly_high[i-1])/NASDAQ_weekly$weekly_high[i-1]
  NASDAQ_weekly$NetReturn_Low[i]<-(NASDAQ_weekly$weekly_low[i]-NASDAQ_weekly$weekly_low[i-1])/NASDAQ_weekly$weekly_low[i-1]
  NASDAQ_weekly$NetReturn_Last[i]<-(NASDAQ_weekly$weekly_close[i]-NASDAQ_weekly$weekly_close[i-1])/NASDAQ_weekly$weekly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
NASDAQ$month <- month(NASDAQ$Date)
NASDAQ$year <- year(NASDAQ$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NASDAQ_monthly <- NASDAQ %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
NASDAQ_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NASDAQ_monthly$LogReturn_Open<-0
NASDAQ_monthly$LogReturn_High<-0
NASDAQ_monthly$LogReturn_Low<-0
NASDAQ_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_monthly)){
  NASDAQ_monthly$LogReturn_Open[i]<-log(NASDAQ_monthly$monthly_open[i]/NASDAQ_monthly$monthly_open[i-1])
  NASDAQ_monthly$LogReturn_High[i]<-log(NASDAQ_monthly$monthly_high[i]/NASDAQ_monthly$monthly_high[i-1])
  NASDAQ_monthly$LogReturn_Low[i]<-log(NASDAQ_monthly$monthly_low[i]/NASDAQ_monthly$monthly_low[i-1])
  NASDAQ_monthly$LogReturn_Close[i]<-log(NASDAQ_monthly$monthly_close[i]/NASDAQ_monthly$monthly_close[i-1])
}

## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NASDAQ_monthly$NetReturn_Open<-0
NASDAQ_monthly$NetReturn_High<-0
NASDAQ_monthly$NetReturn_Low<-0
NASDAQ_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_monthly)){
  NASDAQ_monthly$NetReturn_Open[i]<-(NASDAQ_monthly$monthly_open[i]-NASDAQ_monthly$monthly_open[i-1])/NASDAQ_monthly$monthly_open[i-1]
  NASDAQ_monthly$NetReturn_High[i]<-(NASDAQ_monthly$monthly_high[i]-NASDAQ_monthly$monthly_high[i-1])/NASDAQ_monthly$monthly_high[i-1]
  NASDAQ_monthly$NetReturn_Low[i]<-(NASDAQ_monthly$monthly_low[i]-NASDAQ_monthly$monthly_low[i-1])/NASDAQ_monthly$monthly_low[i-1]
  NASDAQ_monthly$NetReturn_Last[i]<-(NASDAQ_monthly$monthly_close[i]-NASDAQ_monthly$monthly_close[i-1])/NASDAQ_monthly$monthly_close[i-1]
}

#GODISNJI CIST PRINOS
#prvo moraju da se naprave kolone u dataframe-u
NASDAQ_yearly$NetReturn_Open<-0
NASDAQ_yearly$NetReturn_High<-0
NASDAQ_yearly$NetReturn_Low<-0
NASDAQ_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_yearly)){
  NASDAQ_yearly$NetReturn_Open[i]<-(NASDAQ_yearly$yearly_open[i]-NASDAQ_yearly$yearly_open[i-1])/NASDAQ_yearly$yearly_open[i-1]
  NASDAQ_yearly$NetReturn_High[i]<-(NASDAQ_yearly$yearly_high[i]-NASDAQ_yearly$yearly_high[i-1])/NASDAQ_yearly$yearly_high[i-1]
  NASDAQ_yearly$NetReturn_Low[i]<-(NASDAQ_yearly$yearly_low[i]-NASDAQ_yearly$yearly_low[i-1])/NASDAQ_yearly$yearly_low[i-1]
  NASDAQ_yearly$NetReturn_Last[i]<-(NASDAQ_yearly$yearly_close[i]-NASDAQ_yearly$yearly_close[i-1])/NASDAQ_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
NASDAQ_yearly$NetReturns_Open_UK <- "/"
NASDAQ_yearly$NetReturns_Open_UK[1] <- sd(NASDAQ_yearly$NetReturn_Open)
NASDAQ_yearly$NetReturns_High_UK <- "/"
NASDAQ_yearly$NetReturns_High_UK[1] <- sd(NASDAQ_yearly$NetReturn_High)
NASDAQ_yearly$NetReturns_Low_UK<- "/"
NASDAQ_yearly$NetReturns_Low_UK[1] <- sd(NASDAQ_yearly$NetReturn_Low)
NASDAQ_yearly$NetReturns_Last_UK<- "/"
NASDAQ_yearly$NetReturns_Last_UK[1] <- sd(NASDAQ_yearly$NetReturn_Last)
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NASDAQ_yearly <- NASDAQ %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
NASDAQ_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NASDAQ_yearly$LogReturn_Open<-0
NASDAQ_yearly$LogReturn_High<-0
NASDAQ_yearly$LogReturn_Low<-0
NASDAQ_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ_yearly)){
  NASDAQ_yearly$LogReturn_Open[i]<-log(NASDAQ_yearly$yearly_open[i]/NASDAQ_yearly$yearly_open[i-1])
  NASDAQ_yearly$LogReturn_High[i]<-log(NASDAQ_yearly$yearly_high[i]/NASDAQ_yearly$yearly_high[i-1])
  NASDAQ_yearly$LogReturn_Low[i]<-log(NASDAQ_yearly$yearly_low[i]/NASDAQ_yearly$yearly_low[i-1])
  NASDAQ_yearly$LogReturn_Close[i]<-log(NASDAQ_yearly$yearly_close[i]/NASDAQ_yearly$yearly_close[i-1])
}

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#NASDAQ<-NASDAQ[,10]
#install.packages("quantmod")
library(quantmod)

NASDAQ<-NASDAQ[,-10]
NASDAQ_yearly$Volatility_Open <- "/"
NASDAQ_yearly$Volatility_Open[1] <- sd(NASDAQ_yearly$LogReturn_Open)
NASDAQ_yearly$Volatility_High <- "/"
NASDAQ_yearly$Volatility_High[1] <- sd(NASDAQ_yearly$LogReturn_High)
NASDAQ_yearly$Volatility_Low <- "/"
NASDAQ_yearly$Volatility_Low[1] <- sd(NASDAQ_yearly$LogReturn_Low)
NASDAQ_yearly$Volatility_Last <- "/"
NASDAQ_yearly$Volatility_Last[1] <- sd(NASDAQ_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NASDAQ_yearly_volatility <- NASDAQ %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
NASDAQ_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NASDAQ)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(NASDAQ[1])<-"Date"

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ <- plot_ly(data = NASDAQ, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ <- fig_NASDAQ %>% layout(title = "NASDAQ Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NASDAQplay the chart
fig_NASDAQ

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ_lr_d <- plot_ly(data = NASDAQ, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_d <- fig_NASDAQ_lr_d %>% layout(title = "NASDAQ Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NASDAQplay the chart
fig_NASDAQ_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ_lr_w <- plot_ly(data = NASDAQ_weekly, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_w <- fig_NASDAQ_lr_w %>% layout(title = "NASDAQ Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NASDAQplay the chart
fig_NASDAQ_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
NASDAQ_monthly$month<- as.character(NASDAQ_monthly$month)

# Create a candlestick chart
fig_NASDAQ_lr_m <- plot_ly(data = NASDAQ_monthly, type = "candlestick",
                          x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_m <- fig_NASDAQ_lr_m %>% layout(title = "NASDAQ Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NASDAQplay the chart
fig_NASDAQ_lr_m

# YNASDAQRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ_yearly$year<- as.character(NASDAQ_yearly$year)

# Create a candlestick chart
fig_NASDAQ_lr_y <- plot_ly(data = NASDAQ_yearly, type = "candlestick",
                          x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_y <- fig_NASDAQ_lr_y %>% layout(title = "NASDAQ Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NASDAQplay the chart
fig_NASDAQ_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
NASDAQ$MA5 <- rollmean(NASDAQ$Last, k = 5, fill = NA)
NASDAQ$MA21 <- rollmean(NASDAQ$Last, k = 21, fill = NA)
NASDAQ$MA63 <- rollmean(NASDAQ$Last, k = 63, fill = NA)
NASDAQ$MA126 <- rollmean(NASDAQ$Last, k = 126, fill = NA)
NASDAQ$MA252 <- rollmean(NASDAQ$Last, k = 252, fill = NA)

ggplot(NASDAQ, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")


#------------------
ggplot(NASDAQ, aes(x = Date, y = Last,group = 1)) +
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
plot(NASDAQ$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="NASDAQ Open, Close, High i Low Log Return")
lines(NASDAQ$LogReturn_High, type="l", col="blue")
lines(NASDAQ$LogReturn_Low, type="l", col="green")
lines(NASDAQ$LogReturn_Last, type="l", col="purple")

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
NASDAQ.m <- melt(NASDAQ[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(NASDAQ.m, aes(Date, value)) +
  geom_line(data = subset(NASDAQ.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = NASDAQ, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "NASDAQ Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(NASDAQ$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="NASDAQ Open, Close, High i Low Net Return")
lines(NASDAQ$NetReturn_High, type="l", col="blue")
lines(NASDAQ$NetReturn_Low, type="l", col="green")
lines(NASDAQ$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)




















