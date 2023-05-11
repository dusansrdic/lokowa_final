
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
NTDOY <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NTDOY")
names(NTDOY)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

NTDOY_descriptive_statistics <- data.frame(open=mean(NTDOY$Open),high=mean(NTDOY$High),low=mean(NTDOY$Low),last=mean(NTDOY$Last)) 
NTDOY_descriptive_statistics[2,] <- c(median(NTDOY$Open),median(NTDOY$High),median(NTDOY$Low),median(NTDOY$Last))
library(moments)
NTDOY_descriptive_statistics[3,] <- c(skewness(NTDOY$Open),skewness(NTDOY$High),skewness(NTDOY$Low),skewness(NTDOY$Last))
NTDOY_descriptive_statistics[4,] <- c(kurtosis(NTDOY$Open),kurtosis(NTDOY$High),kurtosis(NTDOY$Low),kurtosis(NTDOY$Last))

rownames(NTDOY_descriptive_statistics) <- c("MNTDOYN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NTDOY$LogReturn_Open<-0
NTDOY$LogReturn_High<-0
NTDOY$LogReturn_Low<-0
NTDOY$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY)){
  NTDOY$LogReturn_Open[i]<-log(NTDOY$Open[i]/NTDOY$Open[i-1])
  NTDOY$LogReturn_High[i]<-log(NTDOY$High[i]/NTDOY$High[i-1])
  NTDOY$LogReturn_Low[i]<-log(NTDOY$Low[i]/NTDOY$Low[i-1])
  NTDOY$LogReturn_Last[i]<-log(NTDOY$Last[i]/NTDOY$Last[i-1])
}
#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY$NetReturn_Open<-0
NTDOY$NetReturn_High<-0
NTDOY$NetReturn_Low<-0
NTDOY$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY)){
  NTDOY$NetReturn_Open[i]<-(NTDOY$Open[i]-NTDOY$Open[i-1])/NTDOY$Open[i-1]
  NTDOY$NetReturn_High[i]<-(NTDOY$High[i]-NTDOY$High[i-1])/NTDOY$High[i-1]
  NTDOY$NetReturn_Low[i]<-(NTDOY$Low[i]-NTDOY$Low[i-1])/NTDOY$Low[i-1]
  NTDOY$NetReturn_Last[i]<-(NTDOY$Last[i]-NTDOY$Last[i-1])/NTDOY$Last[i-1]
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
NTDOY$week <- week(NTDOY$Date)
NTDOY$year <- year(NTDOY$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
NTDOY_weekly <- NTDOY %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(NTDOY_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

NTDOY_weekly$LogReturn_Open<-0
NTDOY_weekly$LogReturn_High<-0
NTDOY_weekly$LogReturn_Low<-0
NTDOY_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_weekly)){
  NTDOY_weekly$LogReturn_Open[i]<-log(NTDOY_weekly$weekly_open[i]/NTDOY_weekly$weekly_open[i-1])
  NTDOY_weekly$LogReturn_High[i]<-log(NTDOY_weekly$weekly_high[i]/NTDOY_weekly$weekly_high[i-1])
  NTDOY_weekly$LogReturn_Low[i]<-log(NTDOY_weekly$weekly_low[i]/NTDOY_weekly$weekly_low[i-1])
  NTDOY_weekly$LogReturn_Close[i]<-log(NTDOY_weekly$weekly_close[i]/NTDOY_weekly$weekly_close[i-1])
}
#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY_weekly$NetReturn_Open<-0
NTDOY_weekly$NetReturn_High<-0
NTDOY_weekly$NetReturn_Low<-0
NTDOY_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_weekly)){
  NTDOY_weekly$NetReturn_Open[i]<-(NTDOY_weekly$weekly_open[i]-NTDOY_weekly$weekly_open[i-1])/NTDOY_weekly$weekly_open[i-1]
  NTDOY_weekly$NetReturn_High[i]<-(NTDOY_weekly$weekly_high[i]-NTDOY_weekly$weekly_high[i-1])/NTDOY_weekly$weekly_high[i-1]
  NTDOY_weekly$NetReturn_Low[i]<-(NTDOY_weekly$weekly_low[i]-NTDOY_weekly$weekly_low[i-1])/NTDOY_weekly$weekly_low[i-1]
  NTDOY_weekly$NetReturn_Last[i]<-(NTDOY_weekly$weekly_close[i]-NTDOY_weekly$weekly_close[i-1])/NTDOY_weekly$weekly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
NTDOY$month <- month(NTDOY$Date)
NTDOY$year <- year(NTDOY$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NTDOY_monthly <- NTDOY %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
NTDOY_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY_monthly$LogReturn_Open<-0
NTDOY_monthly$LogReturn_High<-0
NTDOY_monthly$LogReturn_Low<-0
NTDOY_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_monthly)){
  NTDOY_monthly$LogReturn_Open[i]<-log(NTDOY_monthly$monthly_open[i]/NTDOY_monthly$monthly_open[i-1])
  NTDOY_monthly$LogReturn_High[i]<-log(NTDOY_monthly$monthly_high[i]/NTDOY_monthly$monthly_high[i-1])
  NTDOY_monthly$LogReturn_Low[i]<-log(NTDOY_monthly$monthly_low[i]/NTDOY_monthly$monthly_low[i-1])
  NTDOY_monthly$LogReturn_Close[i]<-log(NTDOY_monthly$monthly_close[i]/NTDOY_monthly$monthly_close[i-1])
}


## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY_monthly$NetReturn_Open<-0
NTDOY_monthly$NetReturn_High<-0
NTDOY_monthly$NetReturn_Low<-0
NTDOY_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_monthly)){
  NTDOY_monthly$NetReturn_Open[i]<-(NTDOY_monthly$monthly_open[i]-NTDOY_monthly$monthly_open[i-1])/NTDOY_monthly$monthly_open[i-1]
  NTDOY_monthly$NetReturn_High[i]<-(NTDOY_monthly$monthly_high[i]-NTDOY_monthly$monthly_high[i-1])/NTDOY_monthly$monthly_high[i-1]
  NTDOY_monthly$NetReturn_Low[i]<-(NTDOY_monthly$monthly_low[i]-NTDOY_monthly$monthly_low[i-1])/NTDOY_monthly$monthly_low[i-1]
  NTDOY_monthly$NetReturn_Last[i]<-(NTDOY_monthly$monthly_close[i]-NTDOY_monthly$monthly_close[i-1])/NTDOY_monthly$monthly_close[i-1]
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NTDOY_yearly <- NTDOY %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
NTDOY_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY_yearly$LogReturn_Open<-0
NTDOY_yearly$LogReturn_High<-0
NTDOY_yearly$LogReturn_Low<-0
NTDOY_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_yearly)){
  NTDOY_yearly$LogReturn_Open[i]<-log(NTDOY_yearly$yearly_open[i]/NTDOY_yearly$yearly_open[i-1])
  NTDOY_yearly$LogReturn_High[i]<-log(NTDOY_yearly$yearly_high[i]/NTDOY_yearly$yearly_high[i-1])
  NTDOY_yearly$LogReturn_Low[i]<-log(NTDOY_yearly$yearly_low[i]/NTDOY_yearly$yearly_low[i-1])
  NTDOY_yearly$LogReturn_Close[i]<-log(NTDOY_yearly$yearly_close[i]/NTDOY_yearly$yearly_close[i-1])
}
#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
NTDOY_yearly$NetReturn_Open<-0
NTDOY_yearly$NetReturn_High<-0
NTDOY_yearly$NetReturn_Low<-0
NTDOY_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY_yearly)){
  NTDOY_yearly$NetReturn_Open[i]<-(NTDOY_yearly$yearly_open[i]-NTDOY_yearly$yearly_open[i-1])/NTDOY_yearly$yearly_open[i-1]
  NTDOY_yearly$NetReturn_High[i]<-(NTDOY_yearly$yearly_high[i]-NTDOY_yearly$yearly_high[i-1])/NTDOY_yearly$yearly_high[i-1]
  NTDOY_yearly$NetReturn_Low[i]<-(NTDOY_yearly$yearly_low[i]-NTDOY_yearly$yearly_low[i-1])/NTDOY_yearly$yearly_low[i-1]
  NTDOY_yearly$NetReturn_Last[i]<-(NTDOY_yearly$yearly_close[i]-NTDOY_yearly$yearly_close[i-1])/NTDOY_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
NTDOY_yearly$NetReturns_Open_UK <- "/"
NTDOY_yearly$NetReturns_Open_UK[1] <- sd(NTDOY_yearly$NetReturn_Open)
NTDOY_yearly$NetReturns_High_UK <- "/"
NTDOY_yearly$NetReturns_High_UK[1] <- sd(NTDOY_yearly$NetReturn_High)
NTDOY_yearly$NetReturns_Low_UK<- "/"
NTDOY_yearly$NetReturns_Low_UK[1] <- sd(NTDOY_yearly$NetReturn_Low)
NTDOY_yearly$NetReturns_Last_UK<- "/"
NTDOY_yearly$NetReturns_Last_UK[1] <- sd(NTDOY_yearly$NetReturn_Last)
#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#NTDOY<-NTDOY[,10]
#install.packages("quantmod")
library(quantmod)

NTDOY<-NTDOY[,-10]
NTDOY_yearly$Volatility_Open <- "/"
NTDOY_yearly$Volatility_Open[1] <- sd(NTDOY_yearly$LogReturn_Open)
NTDOY_yearly$Volatility_High <- "/"
NTDOY_yearly$Volatility_High[1] <- sd(NTDOY_yearly$LogReturn_High)
NTDOY_yearly$Volatility_Low <- "/"
NTDOY_yearly$Volatility_Low[1] <- sd(NTDOY_yearly$LogReturn_Low)
NTDOY_yearly$Volatility_Last <- "/"
NTDOY_yearly$Volatility_Last[1] <- sd(NTDOY_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
NTDOY_yearly_volatility <- NTDOY %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
NTDOY_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NTDOY)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(NTDOY[1])<-"Date"

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY <- plot_ly(data = NTDOY, type = "candlestick",
                      x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY <- fig_NTDOY %>% layout(title = "NTDOY Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY_lr_d <- plot_ly(data = NTDOY, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_d <- fig_NTDOY_lr_d %>% layout(title = "NTDOY Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY_lr_w <- plot_ly(data = NTDOY_weekly, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_w <- fig_NTDOY_lr_w %>% layout(title = "NTDOY Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
NTDOY_monthly$month<- as.character(NTDOY_monthly$month)

# Create a candlestick chart
fig_NTDOY_lr_m <- plot_ly(data = NTDOY_monthly, type = "candlestick",
                           x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_m <- fig_NTDOY_lr_m %>% layout(title = "NTDOY Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_m

# YNTDOYRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY_yearly$year<- as.character(NTDOY_yearly$year)

# Create a candlestick chart
fig_NTDOY_lr_y <- plot_ly(data = NTDOY_yearly, type = "candlestick",
                           x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_y <- fig_NTDOY_lr_y %>% layout(title = "NTDOY Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
NTDOY$MA5 <- rollmean(NTDOY$Last, k = 5, fill = NA)
NTDOY$MA21 <- rollmean(NTDOY$Last, k = 21, fill = NA)
NTDOY$MA63 <- rollmean(NTDOY$Last, k = 63, fill = NA)
NTDOY$MA126 <- rollmean(NTDOY$Last, k = 126, fill = NA)
NTDOY$MA252 <- rollmean(NTDOY$Last, k = 252, fill = NA)

ggplot(NTDOY, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")


#------------------
ggplot(NTDOY, aes(x = Date, y = Last,group = 1)) +
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
plot(NTDOY$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="NTDOY Open, Close, High i Low Log Return")
lines(NTDOY$LogReturn_High, type="l", col="blue")
lines(NTDOY$LogReturn_Low, type="l", col="green")
lines(NTDOY$LogReturn_Last, type="l", col="purple")

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
NTDOY.m <- melt(NTDOY[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(NTDOY.m, aes(Date, value)) +
  geom_line(data = subset(NTDOY.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = NTDOY, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "NTDOY Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(NTDOY$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="NTDOY Open, Close, High i Low Net Return")
lines(NTDOY$NetReturn_High, type="l", col="blue")
lines(NTDOY$NetReturn_Low, type="l", col="green")
lines(NTDOY$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)





















