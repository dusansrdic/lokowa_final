
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
SP500 <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="SP500")
#meni je bio cudan format datuma pa sam sve pretvarala ---------------
names(SP500) <- c("Date", "Open", "High", "Low", "Last")
SP500 <- SP500[-1,]
library(openxlsx)
SP500$Date <- convertToDateTime(SP500$Date)
SP500$Open <- as.numeric(SP500$Open)
SP500$High <- as.numeric(SP500$High)
SP500$Low <- as.numeric(SP500$Low)
SP500$Last <- as.numeric(SP500$Last)
#----------------------------------------------------------------------

# DESKRIPTIVNA STATISTIKA


SP500_descriptive_statistics <- data.frame(open=mean(SP500$Open),high=mean(SP500$High),low=mean(SP500$Low),last=mean(SP500$Last)) 
SP500_descriptive_statistics[2,] <- c(median(SP500$Open),median(SP500$High),median(SP500$Low),median(SP500$Last))
library(moments)
SP500_descriptive_statistics[3,] <- c(skewness(SP500$Open),skewness(SP500$High),skewness(SP500$Low),skewness(SP500$Last))
SP500_descriptive_statistics[4,] <- c(kurtosis(SP500$Open),kurtosis(SP500$High),kurtosis(SP500$Low),kurtosis(SP500$Last))

rownames(SP500_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
SP500$LogReturn_Open<-0
SP500$LogReturn_High<-0
SP500$LogReturn_Low<-0
SP500$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500)){
  SP500$LogReturn_Open[i]<-log(SP500$Open[i]/SP500$Open[i-1])
  SP500$LogReturn_High[i]<-log(SP500$High[i]/SP500$High[i-1])
  SP500$LogReturn_Low[i]<-log(SP500$Low[i]/SP500$Low[i-1])
  SP500$LogReturn_Last[i]<-log(SP500$Last[i]/SP500$Last[i-1])
}
#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
SP500$NetReturn_Open<-0
SP500$NetReturn_High<-0
SP500$NetReturn_Low<-0
SP500$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500)){
  SP500$NetReturn_Open[i]<-(SP500$Open[i]-SP500$Open[i-1])/SP500$Open[i-1]
  SP500$NetReturn_High[i]<-(SP500$High[i]-SP500$High[i-1])/SP500$High[i-1]
  SP500$NetReturn_Low[i]<-(SP500$Low[i]-SP500$Low[i-1])/SP500$Low[i-1]
  SP500$NetReturn_Last[i]<-(SP500$Last[i]-SP500$Last[i-1])/SP500$Last[i-1]
}
# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
SP500$week <- week(SP500$Date)
SP500$year <- year(SP500$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
SP500_weekly <- SP500 %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(SP500_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

SP500_weekly$LogReturn_Open<-0
SP500_weekly$LogReturn_High<-0
SP500_weekly$LogReturn_Low<-0
SP500_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_weekly)){
  SP500_weekly$LogReturn_Open[i]<-log(SP500_weekly$weekly_open[i]/SP500_weekly$weekly_open[i-1])
  SP500_weekly$LogReturn_High[i]<-log(SP500_weekly$weekly_high[i]/SP500_weekly$weekly_high[i-1])
  SP500_weekly$LogReturn_Low[i]<-log(SP500_weekly$weekly_low[i]/SP500_weekly$weekly_low[i-1])
  SP500_weekly$LogReturn_Close[i]<-log(SP500_weekly$weekly_close[i]/SP500_weekly$weekly_close[i-1])
}

#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
SP500_weekly$NetReturn_Open<-0
SP500_weekly$NetReturn_High<-0
SP500_weekly$NetReturn_Low<-0
SP500_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_weekly)){
  SP500_weekly$NetReturn_Open[i]<-(SP500_weekly$weekly_open[i]-SP500_weekly$weekly_open[i-1])/SP500_weekly$weekly_open[i-1]
  SP500_weekly$NetReturn_High[i]<-(SP500_weekly$weekly_high[i]-SP500_weekly$weekly_high[i-1])/SP500_weekly$weekly_high[i-1]
  SP500_weekly$NetReturn_Low[i]<-(SP500_weekly$weekly_low[i]-SP500_weekly$weekly_low[i-1])/SP500_weekly$weekly_low[i-1]
  SP500_weekly$NetReturn_Last[i]<-(SP500_weekly$weekly_close[i]-SP500_weekly$weekly_close[i-1])/SP500_weekly$weekly_close[i-1]
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
SP500$month <- month(SP500$Date)
SP500$year <- year(SP500$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
SP500_monthly <- SP500 %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
SP500_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
SP500_monthly$LogReturn_Open<-0
SP500_monthly$LogReturn_High<-0
SP500_monthly$LogReturn_Low<-0
SP500_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_monthly)){
  SP500_monthly$LogReturn_Open[i]<-log(SP500_monthly$monthly_open[i]/SP500_monthly$monthly_open[i-1])
  SP500_monthly$LogReturn_High[i]<-log(SP500_monthly$monthly_high[i]/SP500_monthly$monthly_high[i-1])
  SP500_monthly$LogReturn_Low[i]<-log(SP500_monthly$monthly_low[i]/SP500_monthly$monthly_low[i-1])
  SP500_monthly$LogReturn_Close[i]<-log(SP500_monthly$monthly_close[i]/SP500_monthly$monthly_close[i-1])
}
#mesecni cist prinos
#prvo moraju da se naprave kolone u dataframe-u
SP500_monthly$NetReturn_Open<-0
SP500_monthly$NetReturn_High<-0
SP500_monthly$NetReturn_Low<-0
SP500_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_monthly)){
  SP500_monthly$NetReturn_Open[i]<-(SP500_monthly$monthly_open[i]-SP500_monthly$monthly_open[i-1])/SP500_monthly$monthly_open[i-1]
  SP500_monthly$NetReturn_High[i]<-(SP500_monthly$monthly_high[i]-SP500_monthly$monthly_high[i-1])/SP500_monthly$monthly_high[i-1]
  SP500_monthly$NetReturn_Low[i]<-(SP500_monthly$monthly_low[i]-SP500_monthly$monthly_low[i-1])/SP500_monthly$monthly_low[i-1]
  SP500_monthly$NetReturn_Last[i]<-(SP500_monthly$monthly_close[i]-SP500_monthly$monthly_close[i-1])/SP500_monthly$monthly_close[i-1]
}
#

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
SP500_yearly <- SP500 %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
SP500_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
SP500_yearly$LogReturn_Open<-0
SP500_yearly$LogReturn_High<-0
SP500_yearly$LogReturn_Low<-0
SP500_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_yearly)){
  SP500_yearly$LogReturn_Open[i]<-log(SP500_yearly$yearly_open[i]/SP500_yearly$yearly_open[i-1])
  SP500_yearly$LogReturn_High[i]<-log(SP500_yearly$yearly_high[i]/SP500_yearly$yearly_high[i-1])
  SP500_yearly$LogReturn_Low[i]<-log(SP500_yearly$yearly_low[i]/SP500_yearly$yearly_low[i-1])
  SP500_yearly$LogReturn_Close[i]<-log(SP500_yearly$yearly_close[i]/SP500_yearly$yearly_close[i-1])
}

#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
SP500_yearly$NetReturn_Open<-0
SP500_yearly$NetReturn_High<-0
SP500_yearly$NetReturn_Low<-0
SP500_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500_yearly)){
  SP500_yearly$NetReturn_Open[i]<-(SP500_yearly$yearly_open[i]-SP500_yearly$yearly_open[i-1])/SP500_yearly$yearly_open[i-1]
  SP500_yearly$NetReturn_High[i]<-(SP500_yearly$yearly_high[i]-SP500_yearly$yearly_high[i-1])/SP500_yearly$yearly_high[i-1]
  SP500_yearly$NetReturn_Low[i]<-(SP500_yearly$yearly_low[i]-SP500_yearly$yearly_low[i-1])/SP500_yearly$yearly_low[i-1]
  SP500_yearly$NetReturn_Last[i]<-(SP500_yearly$yearly_close[i]-SP500_yearly$yearly_close[i-1])/SP500_yearly$yearly_close[i-1]
}


#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
SP500_yearly$NetReturns_Open_UK <- "/"
SP500_yearly$NetReturns_Open_UK[1] <- sd(SP500_yearly$NetReturn_Open)
SP500_yearly$NetReturns_High_UK <- "/"
SP500_yearly$NetReturns_High_UK[1] <- sd(SP500_yearly$NetReturn_High)
SP500_yearly$NetReturns_Low_UK<- "/"
SP500_yearly$NetReturns_Low_UK[1] <- sd(SP500_yearly$NetReturn_Low)
SP500_yearly$NetReturns_Last_UK<- "/"
SP500_yearly$NetReturns_Last_UK[1] <- sd(SP500_yearly$NetReturn_Last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#SP500<-SP500[,10]
#install.packages("quantmod")
library(quantmod)

SP500<-SP500[,-10]
SP500_yearly$Volatility_Open <- "/"
SP500_yearly$Volatility_Open[1] <- sd(SP500_yearly$LogReturn_Open)
SP500_yearly$Volatility_High <- "/"
SP500_yearly$Volatility_High[1] <- sd(SP500_yearly$LogReturn_High)
SP500_yearly$Volatility_Low <- "/"
SP500_yearly$Volatility_Low[1] <- sd(SP500_yearly$LogReturn_Low)
SP500_yearly$Volatility_Last <- "/"
SP500_yearly$Volatility_Last[1] <- sd(SP500_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
SP500_yearly_volatility <- SP500 %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
SP500_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(SP500)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(SP500[1])<-"Date"

# Convert date to character for better labeling
SP500$Date<- as.character(SP500$Date)

# Create a candlestick chart
fig_SP500 <- plot_ly(data = SP500, type = "candlestick",
                    x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_SP500 <- fig_SP500 %>% layout(title = "SP500 Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# SP500play the chart
fig_SP500

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
SP500$Date<- as.character(SP500$Date)

# Create a candlestick chart
fig_SP500_lr_d <- plot_ly(data = SP500, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_SP500_lr_d <- fig_SP500_lr_d %>% layout(title = "SP500 Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# SP500play the chart
fig_SP500_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
SP500$Date<- as.character(SP500$Date)

# Create a candlestick chart
fig_SP500_lr_w <- plot_ly(data = SP500_weekly, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_SP500_lr_w <- fig_SP500_lr_w %>% layout(title = "SP500 Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# SP500play the chart
fig_SP500_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
SP500_monthly$month<- as.character(SP500_monthly$month)

# Create a candlestick chart
fig_SP500_lr_m <- plot_ly(data = SP500_monthly, type = "candlestick",
                         x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_SP500_lr_m <- fig_SP500_lr_m %>% layout(title = "SP500 Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# SP500play the chart
fig_SP500_lr_m

# YSP500RLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
SP500_yearly$year<- as.character(SP500_yearly$year)

# Create a candlestick chart
fig_SP500_lr_y <- plot_ly(data = SP500_yearly, type = "candlestick",
                         x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_SP500_lr_y <- fig_SP500_lr_y %>% layout(title = "SP500 Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# SP500play the chart
fig_SP500_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
SP500$MA5 <- rollmean(SP500$Last, k = 5, fill = NA)
SP500$MA21 <- rollmean(SP500$Last, k = 21, fill = NA)
SP500$MA63 <- rollmean(SP500$Last, k = 63, fill = NA)
SP500$MA126 <- rollmean(SP500$Last, k = 126, fill = NA)
SP500$MA252 <- rollmean(SP500$Last, k = 252, fill = NA)

ggplot(SP500, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

#------------------
ggplot(SP500, aes(x = Date, y = Last,group = 1)) +
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
plot(SP500$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="SP500 Open, Close, High i Low Log Return")
lines(SP500$LogReturn_High, type="l", col="blue")
lines(SP500$LogReturn_Low, type="l", col="green")
lines(SP500$LogReturn_Last, type="l", col="purple")

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
SP500.m <- melt(SP500[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(SP500.m, aes(Date, value)) +
  geom_line(data = subset(SP500.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = SP500, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "SP500 Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(SP500$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="SP500 Open, Close, High i Low Net Return")
lines(SP500$NetReturn_High, type="l", col="blue")
lines(SP500$NetReturn_Low, type="l", col="green")
lines(SP500$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)












