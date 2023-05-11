
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
NWS <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NWS")
names(NWS)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

NWS_descriptive_statistics <- data.frame(open=mean(NWS$Open),high=mean(NWS$High),low=mean(NWS$Low),last=mean(NWS$Last)) 
NWS_descriptive_statistics[2,] <- c(median(NWS$Open),median(NWS$High),median(NWS$Low),median(NWS$Last))
library(moments)
NWS_descriptive_statistics[3,] <- c(skewness(NWS$Open),skewness(NWS$High),skewness(NWS$Low),skewness(NWS$Last))
NWS_descriptive_statistics[4,] <- c(kurtosis(NWS$Open),kurtosis(NWS$High),kurtosis(NWS$Low),kurtosis(NWS$Last))

rownames(NWS_descriptive_statistics) <- c("MNWSN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NWS$LogReturn_Open<-0
NWS$LogReturn_High<-0
NWS$LogReturn_Low<-0
NWS$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS)){
  NWS$LogReturn_Open[i]<-log(NWS$Open[i]/NWS$Open[i-1])
  NWS$LogReturn_High[i]<-log(NWS$High[i]/NWS$High[i-1])
  NWS$LogReturn_Low[i]<-log(NWS$Low[i]/NWS$Low[i-1])
  NWS$LogReturn_Last[i]<-log(NWS$Last[i]/NWS$Last[i-1])
}
#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NWS$NetReturn_Open<-0
NWS$NetReturn_High<-0
NWS$NetReturn_Low<-0
NWS$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS)){
  NWS$NetReturn_Open[i]<-(NWS$Open[i]-NWS$Open[i-1])/NWS$Open[i-1]
  NWS$NetReturn_High[i]<-(NWS$High[i]-NWS$High[i-1])/NWS$High[i-1]
  NWS$NetReturn_Low[i]<-(NWS$Low[i]-NWS$Low[i-1])/NWS$Low[i-1]
  NWS$NetReturn_Last[i]<-(NWS$Last[i]-NWS$Last[i-1])/NWS$Last[i-1]
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
NWS$week <- week(NWS$Date)
NWS$year <- year(NWS$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
NWS_weekly <- NWS %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(NWS_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

NWS_weekly$LogReturn_Open<-0
NWS_weekly$LogReturn_High<-0
NWS_weekly$LogReturn_Low<-0
NWS_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_weekly)){
  NWS_weekly$LogReturn_Open[i]<-log(NWS_weekly$weekly_open[i]/NWS_weekly$weekly_open[i-1])
  NWS_weekly$LogReturn_High[i]<-log(NWS_weekly$weekly_high[i]/NWS_weekly$weekly_high[i-1])
  NWS_weekly$LogReturn_Low[i]<-log(NWS_weekly$weekly_low[i]/NWS_weekly$weekly_low[i-1])
  NWS_weekly$LogReturn_Close[i]<-log(NWS_weekly$weekly_close[i]/NWS_weekly$weekly_close[i-1])
}

#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NWS_weekly$NetReturn_Open<-0
NWS_weekly$NetReturn_High<-0
NWS_weekly$NetReturn_Low<-0
NWS_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_weekly)){
  NWS_weekly$NetReturn_Open[i]<-(NWS_weekly$weekly_open[i]-NWS_weekly$weekly_open[i-1])/NWS_weekly$weekly_open[i-1]
  NWS_weekly$NetReturn_High[i]<-(NWS_weekly$weekly_high[i]-NWS_weekly$weekly_high[i-1])/NWS_weekly$weekly_high[i-1]
  NWS_weekly$NetReturn_Low[i]<-(NWS_weekly$weekly_low[i]-NWS_weekly$weekly_low[i-1])/NWS_weekly$weekly_low[i-1]
  NWS_weekly$NetReturn_Last[i]<-(NWS_weekly$weekly_close[i]-NWS_weekly$weekly_close[i-1])/NWS_weekly$weekly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
NWS$month <- month(NWS$Date)
NWS$year <- year(NWS$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NWS_monthly <- NWS %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
NWS_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NWS_monthly$LogReturn_Open<-0
NWS_monthly$LogReturn_High<-0
NWS_monthly$LogReturn_Low<-0
NWS_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_monthly)){
  NWS_monthly$LogReturn_Open[i]<-log(NWS_monthly$monthly_open[i]/NWS_monthly$monthly_open[i-1])
  NWS_monthly$LogReturn_High[i]<-log(NWS_monthly$monthly_high[i]/NWS_monthly$monthly_high[i-1])
  NWS_monthly$LogReturn_Low[i]<-log(NWS_monthly$monthly_low[i]/NWS_monthly$monthly_low[i-1])
  NWS_monthly$LogReturn_Close[i]<-log(NWS_monthly$monthly_close[i]/NWS_monthly$monthly_close[i-1])
}


## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NWS_monthly$NetReturn_Open<-0
NWS_monthly$NetReturn_High<-0
NWS_monthly$NetReturn_Low<-0
NWS_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_monthly)){
  NWS_monthly$NetReturn_Open[i]<-(NWS_monthly$monthly_open[i]-NWS_monthly$monthly_open[i-1])/NWS_monthly$monthly_open[i-1]
  NWS_monthly$NetReturn_High[i]<-(NWS_monthly$monthly_high[i]-NWS_monthly$monthly_high[i-1])/NWS_monthly$monthly_high[i-1]
  NWS_monthly$NetReturn_Low[i]<-(NWS_monthly$monthly_low[i]-NWS_monthly$monthly_low[i-1])/NWS_monthly$monthly_low[i-1]
  NWS_monthly$NetReturn_Last[i]<-(NWS_monthly$monthly_close[i]-NWS_monthly$monthly_close[i-1])/NWS_monthly$monthly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NWS_yearly <- NWS %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
NWS_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NWS_yearly$LogReturn_Open<-0
NWS_yearly$LogReturn_High<-0
NWS_yearly$LogReturn_Low<-0
NWS_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_yearly)){
  NWS_yearly$LogReturn_Open[i]<-log(NWS_yearly$yearly_open[i]/NWS_yearly$yearly_open[i-1])
  NWS_yearly$LogReturn_High[i]<-log(NWS_yearly$yearly_high[i]/NWS_yearly$yearly_high[i-1])
  NWS_yearly$LogReturn_Low[i]<-log(NWS_yearly$yearly_low[i]/NWS_yearly$yearly_low[i-1])
  NWS_yearly$LogReturn_Close[i]<-log(NWS_yearly$yearly_close[i]/NWS_yearly$yearly_close[i-1])
}
#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NWS_yearly$NetReturn_Open<-0
NWS_yearly$NetReturn_High<-0
NWS_yearly$NetReturn_Low<-0
NWS_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS_yearly)){
  NWS_yearly$NetReturn_Open[i]<-(NWS_yearly$yearly_open[i]-NWS_yearly$yearly_open[i-1])/NWS_yearly$yearly_open[i-1]
  NWS_yearly$NetReturn_High[i]<-(NWS_yearly$yearly_high[i]-NWS_yearly$yearly_high[i-1])/NWS_yearly$yearly_high[i-1]
  NWS_yearly$NetReturn_Low[i]<-(NWS_yearly$yearly_low[i]-NWS_yearly$yearly_low[i-1])/NWS_yearly$yearly_low[i-1]
  NWS_yearly$NetReturn_Last[i]<-(NWS_yearly$yearly_close[i]-NWS_yearly$yearly_close[i-1])/NWS_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
NWS_yearly$NetReturns_Open_UK <- "/"
NWS_yearly$NetReturns_Open_UK[1] <- sd(NWS_yearly$NetReturn_Open)
NWS_yearly$NetReturns_High_UK <- "/"
NWS_yearly$NetReturns_High_UK[1] <- sd(NWS_yearly$NetReturn_High)
NWS_yearly$NetReturns_Low_UK<- "/"
NWS_yearly$NetReturns_Low_UK[1] <- sd(NWS_yearly$NetReturn_Low)
NWS_yearly$NetReturns_Last_UK<- "/"
NWS_yearly$NetReturns_Last_UK[1] <- sd(NWS_yearly$NetReturn_Last)

# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#NWS<-NWS[,10]
#install.packages("quantmod")
library(quantmod)

NWS<-NWS[,-10]
NWS_yearly$Volatility_Open <- "/"
NWS_yearly$Volatility_Open[1] <- sd(NWS_yearly$LogReturn_Open)
NWS_yearly$Volatility_High <- "/"
NWS_yearly$Volatility_High[1] <- sd(NWS_yearly$LogReturn_High)
NWS_yearly$Volatility_Low <- "/"
NWS_yearly$Volatility_Low[1] <- sd(NWS_yearly$LogReturn_Low)
NWS_yearly$Volatility_Last <- "/"
NWS_yearly$Volatility_Last[1] <- sd(NWS_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NWS_yearly_volatility <- NWS %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
NWS_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NWS)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(NWS[1])<-"Date"

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS <- plot_ly(data = NWS, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS <- fig_NWS %>% layout(title = "NWS Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NWSplay the chart
fig_NWS

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS_lr_d <- plot_ly(data = NWS, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_d <- fig_NWS_lr_d %>% layout(title = "NWS Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NWSplay the chart
fig_NWS_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS_lr_w <- plot_ly(data = NWS_weekly, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_w <- fig_NWS_lr_w %>% layout(title = "NWS Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NWSplay the chart
fig_NWS_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
NWS_monthly$month<- as.character(NWS_monthly$month)

# Create a candlestick chart
fig_NWS_lr_m <- plot_ly(data = NWS_monthly, type = "candlestick",
                          x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_m <- fig_NWS_lr_m %>% layout(title = "NWS Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NWSplay the chart
fig_NWS_lr_m

# YNWSRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS_yearly$year<- as.character(NWS_yearly$year)

# Create a candlestick chart
fig_NWS_lr_y <- plot_ly(data = NWS_yearly, type = "candlestick",
                          x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_y <- fig_NWS_lr_y %>% layout(title = "NWS Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NWSplay the chart
fig_NWS_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
NWS$MA5 <- rollmean(NWS$Last, k = 5, fill = NA)
NWS$MA21 <- rollmean(NWS$Last, k = 21, fill = NA)
NWS$MA63 <- rollmean(NWS$Last, k = 63, fill = NA)
NWS$MA126 <- rollmean(NWS$Last, k = 126, fill = NA)
NWS$MA252 <- rollmean(NWS$Last, k = 252, fill = NA)

ggplot(NWS, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")


#------------------
ggplot(NWS, aes(x = Date, y = Last,group = 1)) +
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
plot(NWS$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="NWS Open, Close, High i Low Log Return")
lines(NWS$LogReturn_High, type="l", col="blue")
lines(NWS$LogReturn_Low, type="l", col="green")
lines(NWS$LogReturn_Last, type="l", col="purple")

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
NWS.m <- melt(NWS[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(NWS.m, aes(Date, value)) +
  geom_line(data = subset(NWS.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = NWS, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "NWS Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(NWS$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="NWS Open, Close, High i Low Net Return")
lines(NWS$NetReturn_High, type="l", col="blue")
lines(NWS$NetReturn_Low, type="l", col="green")
lines(NWS$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)










