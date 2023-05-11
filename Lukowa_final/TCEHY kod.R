
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
TCEHY <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="TCEHY")
names(TCEHY)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

TCEHY_descriptive_statistics <- data.frame(open=mean(TCEHY$Open),high=mean(TCEHY$High),low=mean(TCEHY$Low),last=mean(TCEHY$Last)) 
TCEHY_descriptive_statistics[2,] <- c(median(TCEHY$Open),median(TCEHY$High),median(TCEHY$Low),median(TCEHY$Last))
library(moments)
TCEHY_descriptive_statistics[3,] <- c(skewness(TCEHY$Open),skewness(TCEHY$High),skewness(TCEHY$Low),skewness(TCEHY$Last))
TCEHY_descriptive_statistics[4,] <- c(kurtosis(TCEHY$Open),kurtosis(TCEHY$High),kurtosis(TCEHY$Low),kurtosis(TCEHY$Last))

rownames(TCEHY_descriptive_statistics) <- c("MTCEHYN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
TCEHY$LogReturn_Open<-0
TCEHY$LogReturn_High<-0
TCEHY$LogReturn_Low<-0
TCEHY$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY)){
  TCEHY$LogReturn_Open[i]<-log(TCEHY$Open[i]/TCEHY$Open[i-1])
  TCEHY$LogReturn_High[i]<-log(TCEHY$High[i]/TCEHY$High[i-1])
  TCEHY$LogReturn_Low[i]<-log(TCEHY$Low[i]/TCEHY$Low[i-1])
  TCEHY$LogReturn_Last[i]<-log(TCEHY$Last[i]/TCEHY$Last[i-1])
}


#CIST PRINOS
# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY$NetReturn_Open<-0
TCEHY$NetReturn_High<-0
TCEHY$NetReturn_Low<-0
TCEHY$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY)){
  TCEHY$NetReturn_Open[i]<-(TCEHY$Open[i]-TCEHY$Open[i-1])/TCEHY$Open[i-1]
  TCEHY$NetReturn_High[i]<-(TCEHY$High[i]-TCEHY$High[i-1])/TCEHY$High[i-1]
  TCEHY$NetReturn_Low[i]<-(TCEHY$Low[i]-TCEHY$Low[i-1])/TCEHY$Low[i-1]
  TCEHY$NetReturn_Last[i]<-(TCEHY$Last[i]-TCEHY$Last[i-1])/TCEHY$Last[i-1]
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
TCEHY$week <- week(TCEHY$Date)
TCEHY$year <- year(TCEHY$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
TCEHY_weekly <- TCEHY %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(TCEHY_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

TCEHY_weekly$LogReturn_Open<-0
TCEHY_weekly$LogReturn_High<-0
TCEHY_weekly$LogReturn_Low<-0
TCEHY_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_weekly)){
  TCEHY_weekly$LogReturn_Open[i]<-log(TCEHY_weekly$weekly_open[i]/TCEHY_weekly$weekly_open[i-1])
  TCEHY_weekly$LogReturn_High[i]<-log(TCEHY_weekly$weekly_high[i]/TCEHY_weekly$weekly_high[i-1])
  TCEHY_weekly$LogReturn_Low[i]<-log(TCEHY_weekly$weekly_low[i]/TCEHY_weekly$weekly_low[i-1])
  TCEHY_weekly$LogReturn_Close[i]<-log(TCEHY_weekly$weekly_close[i]/TCEHY_weekly$weekly_close[i-1])
}
#NEDELJNI CIST PRINOS
# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY_weekly$NetReturn_Open<-0
TCEHY_weekly$NetReturn_High<-0
TCEHY_weekly$NetReturn_Low<-0
TCEHY_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_weekly)){
  TCEHY_weekly$NetReturn_Open[i]<-(TCEHY_weekly$weekly_open[i]-TCEHY_weekly$weekly_open[i-1])/TCEHY_weekly$weekly_open[i-1]
  TCEHY_weekly$NetReturn_High[i]<-(TCEHY_weekly$weekly_high[i]-TCEHY_weekly$weekly_high[i-1])/TCEHY_weekly$weekly_high[i-1]
  TCEHY_weekly$NetReturn_Low[i]<-(TCEHY_weekly$weekly_low[i]-TCEHY_weekly$weekly_low[i-1])/TCEHY_weekly$weekly_low[i-1]
  TCEHY_weekly$NetReturn_Last[i]<-(TCEHY_weekly$weekly_close[i]-TCEHY_weekly$weekly_close[i-1])/TCEHY_weekly$weekly_close[i-1]
}


# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
TCEHY$month <- month(TCEHY$Date)
TCEHY$year <- year(TCEHY$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TCEHY_monthly <- TCEHY %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
TCEHY_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY_monthly$LogReturn_Open<-0
TCEHY_monthly$LogReturn_High<-0
TCEHY_monthly$LogReturn_Low<-0
TCEHY_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_monthly)){
  TCEHY_monthly$LogReturn_Open[i]<-log(TCEHY_monthly$monthly_open[i]/TCEHY_monthly$monthly_open[i-1])
  TCEHY_monthly$LogReturn_High[i]<-log(TCEHY_monthly$monthly_high[i]/TCEHY_monthly$monthly_high[i-1])
  TCEHY_monthly$LogReturn_Low[i]<-log(TCEHY_monthly$monthly_low[i]/TCEHY_monthly$monthly_low[i-1])
  TCEHY_monthly$LogReturn_Close[i]<-log(TCEHY_monthly$monthly_close[i]/TCEHY_monthly$monthly_close[i-1])
}


## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY_monthly$NetReturn_Open<-0
TCEHY_monthly$NetReturn_High<-0
TCEHY_monthly$NetReturn_Low<-0
TCEHY_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_monthly)){
  TCEHY_monthly$NetReturn_Open[i]<-(TCEHY_monthly$monthly_open[i]-TCEHY_monthly$monthly_open[i-1])/TCEHY_monthly$monthly_open[i-1]
  TCEHY_monthly$NetReturn_High[i]<-(TCEHY_monthly$monthly_high[i]-TCEHY_monthly$monthly_high[i-1])/TCEHY_monthly$monthly_high[i-1]
  TCEHY_monthly$NetReturn_Low[i]<-(TCEHY_monthly$monthly_low[i]-TCEHY_monthly$monthly_low[i-1])/TCEHY_monthly$monthly_low[i-1]
  TCEHY_monthly$NetReturn_Last[i]<-(TCEHY_monthly$monthly_close[i]-TCEHY_monthly$monthly_close[i-1])/TCEHY_monthly$monthly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TCEHY_yearly <- TCEHY %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
TCEHY_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY_yearly$LogReturn_Open<-0
TCEHY_yearly$LogReturn_High<-0
TCEHY_yearly$LogReturn_Low<-0
TCEHY_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_yearly)){
  TCEHY_yearly$LogReturn_Open[i]<-log(TCEHY_yearly$yearly_open[i]/TCEHY_yearly$yearly_open[i-1])
  TCEHY_yearly$LogReturn_High[i]<-log(TCEHY_yearly$yearly_high[i]/TCEHY_yearly$yearly_high[i-1])
  TCEHY_yearly$LogReturn_Low[i]<-log(TCEHY_yearly$yearly_low[i]/TCEHY_yearly$yearly_low[i-1])
  TCEHY_yearly$LogReturn_Close[i]<-log(TCEHY_yearly$yearly_close[i]/TCEHY_yearly$yearly_close[i-1])
}

#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TCEHY_yearly$NetReturn_Open<-0
TCEHY_yearly$NetReturn_High<-0
TCEHY_yearly$NetReturn_Low<-0
TCEHY_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY_yearly)){
  TCEHY_yearly$NetReturn_Open[i]<-(TCEHY_yearly$yearly_open[i]-TCEHY_yearly$yearly_open[i-1])/TCEHY_yearly$yearly_open[i-1]
  TCEHY_yearly$NetReturn_High[i]<-(TCEHY_yearly$yearly_high[i]-TCEHY_yearly$yearly_high[i-1])/TCEHY_yearly$yearly_high[i-1]
  TCEHY_yearly$NetReturn_Low[i]<-(TCEHY_yearly$yearly_low[i]-TCEHY_yearly$yearly_low[i-1])/TCEHY_yearly$yearly_low[i-1]
  TCEHY_yearly$NetReturn_Last[i]<-(TCEHY_yearly$yearly_close[i]-TCEHY_yearly$yearly_close[i-1])/TCEHY_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg
TCEHY_yearly$NetReturns_Open_UK <- "/"
TCEHY_yearly$NetReturns_Open_UK[1] <- sd(TCEHY_yearly$NetReturn_Open)
TCEHY_yearly$NetReturns_High_UK <- "/"
TCEHY_yearly$NetReturns_High_UK[1] <- sd(TCEHY_yearly$NetReturn_High)
TCEHY_yearly$NetReturns_Low_UK<- "/"
TCEHY_yearly$NetReturns_Low_UK[1] <- sd(TCEHY_yearly$NetReturn_Low)
TCEHY_yearly$NetReturns_Last_UK<- "/"
TCEHY_yearly$NetReturns_Last_UK[1] <- sd(TCEHY_yearly$NetReturn_Last)
 
library(TTR)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#TCEHY<-TCEHY[,10]
#install.packages("quantmod")
library(quantmod)

TCEHY<-TCEHY[,-10]
TCEHY_yearly$Volatility_Open <- "/"
TCEHY_yearly$Volatility_Open[1] <- sd(TCEHY_yearly$LogReturn_Open)
TCEHY_yearly$Volatility_High <- "/"
TCEHY_yearly$Volatility_High[1] <- sd(TCEHY_yearly$LogReturn_High)
TCEHY_yearly$Volatility_Low <- "/"
TCEHY_yearly$Volatility_Low[1] <- sd(TCEHY_yearly$LogReturn_Low)
TCEHY_yearly$Volatility_Last <- "/"
TCEHY_yearly$Volatility_Last[1] <- sd(TCEHY_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TCEHY_yearly_volatility <- TCEHY %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
TCEHY_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(TCEHY)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(TCEHY[1])<-"Date"

# Convert date to character for better labeling
TCEHY$Date<- as.character(TCEHY$Date)

# Create a candlestick chart
fig_TCEHY <- plot_ly(data = TCEHY, type = "candlestick",
                    x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TCEHY <- fig_TCEHY %>% layout(title = "TCEHY Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TCEHYplay the chart
fig_TCEHY

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TCEHY$Date<- as.character(TCEHY$Date)

# Create a candlestick chart
fig_TCEHY_lr_d <- plot_ly(data = TCEHY, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TCEHY_lr_d <- fig_TCEHY_lr_d %>% layout(title = "TCEHY Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TCEHYplay the chart
fig_TCEHY_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TCEHY$Date<- as.character(TCEHY$Date)

# Create a candlestick chart
fig_TCEHY_lr_w <- plot_ly(data = TCEHY_weekly, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TCEHY_lr_w <- fig_TCEHY_lr_w %>% layout(title = "TCEHY Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TCEHYplay the chart
fig_TCEHY_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
TCEHY_monthly$month<- as.character(TCEHY_monthly$month)

# Create a candlestick chart
fig_TCEHY_lr_m <- plot_ly(data = TCEHY_monthly, type = "candlestick",
                         x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TCEHY_lr_m <- fig_TCEHY_lr_m %>% layout(title = "TCEHY Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TCEHYplay the chart
fig_TCEHY_lr_m

# YTCEHYRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TCEHY_yearly$year<- as.character(TCEHY_yearly$year)

# Create a candlestick chart
fig_TCEHY_lr_y <- plot_ly(data = TCEHY_yearly, type = "candlestick",
                         x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TCEHY_lr_y <- fig_TCEHY_lr_y %>% layout(title = "TCEHY Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TCEHYplay the chart
fig_TCEHY_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
TCEHY$MA5 <- rollmean(TCEHY$Last, k = 5, fill = NA)
TCEHY$MA21 <- rollmean(TCEHY$Last, k = 21, fill = NA)
TCEHY$MA63 <- rollmean(TCEHY$Last, k = 63, fill = NA)
TCEHY$MA126 <- rollmean(TCEHY$Last, k = 126, fill = NA)
TCEHY$MA252 <- rollmean(TCEHY$Last, k = 252, fill = NA)

ggplot(TCEHY, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")


#------------------
ggplot(TCEHY, aes(x = Date, y = Last,group = 1)) +
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
plot(TCEHY$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="TCEHY Open, Close, High i Low Log Return")
lines(TCEHY$LogReturn_High, type="l", col="blue")
lines(TCEHY$LogReturn_Low, type="l", col="green")
lines(TCEHY$LogReturn_Last, type="l", col="purple")

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
TCEHY.m <- melt(TCEHY[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(TCEHY.m, aes(Date, value)) +
  geom_line(data = subset(TCEHY.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = TCEHY, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "TCEHY Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(TCEHY$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="TCEHY Open, Close, High i Low Net Return")
lines(TCEHY$NetReturn_High, type="l", col="blue")
lines(TCEHY$NetReturn_Low, type="l", col="green")
lines(TCEHY$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)














