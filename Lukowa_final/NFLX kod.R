
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
NFLX <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NFLX")
names(NFLX)[1] <- "Date"


# DESKRIPTIVNA STATISTIKA

NFLX_descriptive_statistics <- data.frame(open=mean(NFLX$Open),high=mean(NFLX$High),low=mean(NFLX$Low),last=mean(NFLX$Last)) 
NFLX_descriptive_statistics[2,] <- c(median(NFLX$Open),median(NFLX$High),median(NFLX$Low),median(NFLX$Last))
library(moments)
NFLX_descriptive_statistics[3,] <- c(skewness(NFLX$Open),skewness(NFLX$High),skewness(NFLX$Low),skewness(NFLX$Last))
NFLX_descriptive_statistics[4,] <- c(kurtosis(NFLX$Open),kurtosis(NFLX$High),kurtosis(NFLX$Low),kurtosis(NFLX$Last))

rownames(NFLX_descriptive_statistics) <- c("MNFLXN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NFLX$LogReturn_Open<-0
NFLX$LogReturn_High<-0
NFLX$LogReturn_Low<-0
NFLX$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX)){
  NFLX$LogReturn_Open[i]<-log(NFLX$Open[i]/NFLX$Open[i-1])
  NFLX$LogReturn_High[i]<-log(NFLX$High[i]/NFLX$High[i-1])
  NFLX$LogReturn_Low[i]<-log(NFLX$Low[i]/NFLX$Low[i-1])
  NFLX$LogReturn_Last[i]<-log(NFLX$Last[i]/NFLX$Last[i-1])
}

#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NFLX$NetReturn_Open<-0
NFLX$NetReturn_High<-0
NFLX$NetReturn_Low<-0
NFLX$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX)){
  NFLX$NetReturn_Open[i]<-(NFLX$Open[i]-NFLX$Open[i-1])/NFLX$Open[i-1]
  NFLX$NetReturn_High[i]<-(NFLX$High[i]-NFLX$High[i-1])/NFLX$High[i-1]
  NFLX$NetReturn_Low[i]<-(NFLX$Low[i]-NFLX$Low[i-1])/NFLX$Low[i-1]
  NFLX$NetReturn_Last[i]<-(NFLX$Last[i]-NFLX$Last[i-1])/NFLX$Last[i-1]
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
NFLX$week <- week(NFLX$Date)
NFLX$year <- year(NFLX$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
NFLX_weekly <- NFLX %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(NFLX_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

NFLX_weekly$LogReturn_Open<-0
NFLX_weekly$LogReturn_High<-0
NFLX_weekly$LogReturn_Low<-0
NFLX_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_weekly)){
  NFLX_weekly$LogReturn_Open[i]<-log(NFLX_weekly$weekly_open[i]/NFLX_weekly$weekly_open[i-1])
  NFLX_weekly$LogReturn_High[i]<-log(NFLX_weekly$weekly_high[i]/NFLX_weekly$weekly_high[i-1])
  NFLX_weekly$LogReturn_Low[i]<-log(NFLX_weekly$weekly_low[i]/NFLX_weekly$weekly_low[i-1])
  NFLX_weekly$LogReturn_Close[i]<-log(NFLX_weekly$weekly_close[i]/NFLX_weekly$weekly_close[i-1])
}


#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NFLX_weekly$NetReturn_Open<-0
NFLX_weekly$NetReturn_High<-0
NFLX_weekly$NetReturn_Low<-0
NFLX_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_weekly)){
  NFLX_weekly$NetReturn_Open[i]<-(NFLX_weekly$weekly_open[i]-NFLX_weekly$weekly_open[i-1])/NFLX_weekly$weekly_open[i-1]
  NFLX_weekly$NetReturn_High[i]<-(NFLX_weekly$weekly_high[i]-NFLX_weekly$weekly_high[i-1])/NFLX_weekly$weekly_high[i-1]
  NFLX_weekly$NetReturn_Low[i]<-(NFLX_weekly$weekly_low[i]-NFLX_weekly$weekly_low[i-1])/NFLX_weekly$weekly_low[i-1]
  NFLX_weekly$NetReturn_Last[i]<-(NFLX_weekly$weekly_close[i]-NFLX_weekly$weekly_close[i-1])/NFLX_weekly$weekly_close[i-1]
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
NFLX$month <- month(NFLX$Date)
NFLX$year <- year(NFLX$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NFLX_monthly <- NFLX %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
NFLX_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NFLX_monthly$LogReturn_Open<-0
NFLX_monthly$LogReturn_High<-0
NFLX_monthly$LogReturn_Low<-0
NFLX_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_monthly)){
  NFLX_monthly$LogReturn_Open[i]<-log(NFLX_monthly$monthly_open[i]/NFLX_monthly$monthly_open[i-1])
  NFLX_monthly$LogReturn_High[i]<-log(NFLX_monthly$monthly_high[i]/NFLX_monthly$monthly_high[i-1])
  NFLX_monthly$LogReturn_Low[i]<-log(NFLX_monthly$monthly_low[i]/NFLX_monthly$monthly_low[i-1])
  NFLX_monthly$LogReturn_Close[i]<-log(NFLX_monthly$monthly_close[i]/NFLX_monthly$monthly_close[i-1])
}

## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NFLX_monthly$NetReturn_Open<-0
NFLX_monthly$NetReturn_High<-0
NFLX_monthly$NetReturn_Low<-0
NFLX_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_monthly)){
  NFLX_monthly$NetReturn_Open[i]<-(NFLX_monthly$monthly_open[i]-NFLX_monthly$monthly_open[i-1])/NFLX_monthly$monthly_open[i-1]
  NFLX_monthly$NetReturn_High[i]<-(NFLX_monthly$monthly_high[i]-NFLX_monthly$monthly_high[i-1])/NFLX_monthly$monthly_high[i-1]
  NFLX_monthly$NetReturn_Low[i]<-(NFLX_monthly$monthly_low[i]-NFLX_monthly$monthly_low[i-1])/NFLX_monthly$monthly_low[i-1]
  NFLX_monthly$NetReturn_Last[i]<-(NFLX_monthly$monthly_close[i]-NFLX_monthly$monthly_close[i-1])/NFLX_monthly$monthly_close[i-1]
}
#
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NFLX_yearly <- NFLX %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
NFLX_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NFLX_yearly$LogReturn_Open<-0
NFLX_yearly$LogReturn_High<-0
NFLX_yearly$LogReturn_Low<-0
NFLX_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_yearly)){
  NFLX_yearly$LogReturn_Open[i]<-log(NFLX_yearly$yearly_open[i]/NFLX_yearly$yearly_open[i-1])
  NFLX_yearly$LogReturn_High[i]<-log(NFLX_yearly$yearly_high[i]/NFLX_yearly$yearly_high[i-1])
  NFLX_yearly$LogReturn_Low[i]<-log(NFLX_yearly$yearly_low[i]/NFLX_yearly$yearly_low[i-1])
  NFLX_yearly$LogReturn_Close[i]<-log(NFLX_yearly$yearly_close[i]/NFLX_yearly$yearly_close[i-1])
}

#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NFLX_yearly$NetReturn_Open<-0
NFLX_yearly$NetReturn_High<-0
NFLX_yearly$NetReturn_Low<-0
NFLX_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX_yearly)){
  NFLX_yearly$NetReturn_Open[i]<-(NFLX_yearly$yearly_open[i]-NFLX_yearly$yearly_open[i-1])/NFLX_yearly$yearly_open[i-1]
  NFLX_yearly$NetReturn_High[i]<-(NFLX_yearly$yearly_high[i]-NFLX_yearly$yearly_high[i-1])/NFLX_yearly$yearly_high[i-1]
  NFLX_yearly$NetReturn_Low[i]<-(NFLX_yearly$yearly_low[i]-NFLX_yearly$yearly_low[i-1])/NFLX_yearly$yearly_low[i-1]
  NFLX_yearly$NetReturn_Last[i]<-(NFLX_yearly$yearly_close[i]-NFLX_yearly$yearly_close[i-1])/NFLX_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
NFLX_yearly$NetReturns_Open_UK <- "/"
NFLX_yearly$NetReturns_Open_UK[1] <- sd(NFLX_yearly$NetReturn_Open)
NFLX_yearly$NetReturns_High_UK <- "/"
NFLX_yearly$NetReturns_High_UK[1] <- sd(NFLX_yearly$NetReturn_High)
NFLX_yearly$NetReturns_Low_UK<- "/"
NFLX_yearly$NetReturns_Low_UK[1] <- sd(NFLX_yearly$NetReturn_Low)
NFLX_yearly$NetReturns_Last_UK<- "/"
NFLX_yearly$NetReturns_Last_UK[1] <- sd(NFLX_yearly$NetReturn_Last)
#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#NFLX<-NFLX[,10]
#install.packages("quantmod")
library(quantmod)

NFLX<-NFLX[,-10]
NFLX_yearly$Volatility_Open <- "/"
NFLX_yearly$Volatility_Open[1] <- sd(NFLX_yearly$LogReturn_Open)
NFLX_yearly$Volatility_High <- "/"
NFLX_yearly$Volatility_High[1] <- sd(NFLX_yearly$LogReturn_High)
NFLX_yearly$Volatility_Low <- "/"
NFLX_yearly$Volatility_Low[1] <- sd(NFLX_yearly$LogReturn_Low)
NFLX_yearly$Volatility_Last <- "/"
NFLX_yearly$Volatility_Last[1] <- sd(NFLX_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NFLX_yearly_volatility <- NFLX %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
NFLX_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NFLX)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(NFLX[1])<-"Date"

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX <- plot_ly(data = NFLX, type = "candlestick",
                      x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX <- fig_NFLX %>% layout(title = "NFLX Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NFLXplay the chart
fig_NFLX

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX_lr_d <- plot_ly(data = NFLX, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_d <- fig_NFLX_lr_d %>% layout(title = "NFLX Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NFLXplay the chart
fig_NFLX_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX_lr_w <- plot_ly(data = NFLX_weekly, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_w <- fig_NFLX_lr_w %>% layout(title = "NFLX Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NFLXplay the chart
fig_NFLX_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
NFLX_monthly$month<- as.character(NFLX_monthly$month)

# Create a candlestick chart
fig_NFLX_lr_m <- plot_ly(data = NFLX_monthly, type = "candlestick",
                           x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_m <- fig_NFLX_lr_m %>% layout(title = "NFLX Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NFLXplay the chart
fig_NFLX_lr_m

# YNFLXRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX_yearly$year<- as.character(NFLX_yearly$year)

# Create a candlestick chart
fig_NFLX_lr_y <- plot_ly(data = NFLX_yearly, type = "candlestick",
                           x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_y <- fig_NFLX_lr_y %>% layout(title = "NFLX Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NFLXplay the chart
fig_NFLX_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
NFLX$MA5 <- rollmean(NFLX$Last, k = 5, fill = NA)
NFLX$MA21 <- rollmean(NFLX$Last, k = 21, fill = NA)
NFLX$MA63 <- rollmean(NFLX$Last, k = 63, fill = NA)
NFLX$MA126 <- rollmean(NFLX$Last, k = 126, fill = NA)
NFLX$MA252 <- rollmean(NFLX$Last, k = 252, fill = NA)

ggplot(NFLX, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")



#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------
ggplot(NFLX, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "solid") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "solid") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "solid") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "solid") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "solid") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

# prinose (i log i net) iscrtati na line grafiku sa 5 podgrafika:
# prinos open cene
# prinos high
# prinos low
# prinos close
# prinos candlestick (OVAJ DEO NE MOZE DA SE URADI, NE MOGU DA NADJEM NACIN DA SPOJIM LINECHART SA CANDLESTICK CHARTOM)

#DNEVNI
# Grafikon (Log Return)
plot(NFLX$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="NFLX Open, Close, High i Low Log Return")
lines(NFLX$LogReturn_High, type="l", col="blue")
lines(NFLX$LogReturn_Low, type="l", col="green")
lines(NFLX$LogReturn_Last, type="l", col="purple")

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
NFLX.m <- melt(NFLX[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(NFLX.m, aes(Date, value)) +
  geom_line(data = subset(NFLX.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = NFLX, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "NFLX Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(NFLX$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="NFLX Open, Close, High i Low Net Return")
lines(NFLX$NetReturn_High, type="l", col="blue")
lines(NFLX$NetReturn_Low, type="l", col="green")
lines(NFLX$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)






















