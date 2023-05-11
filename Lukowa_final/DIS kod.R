
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
DIS <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="DIS")
names(DIS)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

DIS_descriptive_statistics <- data.frame(open=mean(DIS$Open),high=mean(DIS$High),low=mean(DIS$Low),last=mean(DIS$Last)) 
DIS_descriptive_statistics[2,] <- c(median(DIS$Open),median(DIS$High),median(DIS$Low),median(DIS$Last))
library(moments)
DIS_descriptive_statistics[3,] <- c(skewness(DIS$Open),skewness(DIS$High),skewness(DIS$Low),skewness(DIS$Last))
DIS_descriptive_statistics[4,] <- c(kurtosis(DIS$Open),kurtosis(DIS$High),kurtosis(DIS$Low),kurtosis(DIS$Last))

rownames(DIS_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
DIS$LogReturn_Open<-0
DIS$LogReturn_High<-0
DIS$LogReturn_Low<-0
DIS$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS)){
  DIS$LogReturn_Open[i]<-log(DIS$Open[i]/DIS$Open[i-1])
  DIS$LogReturn_High[i]<-log(DIS$High[i]/DIS$High[i-1])
  DIS$LogReturn_Low[i]<-log(DIS$Low[i]/DIS$Low[i-1])
  DIS$LogReturn_Last[i]<-log(DIS$Last[i]/DIS$Last[i-1])
}
#prvo moraju da se naprave kolone u dataframe-u
DIS$NetReturn_Open<-0
DIS$NetReturn_High<-0
DIS$NetReturn_Low<-0
DIS$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS)){
  DIS$NetReturn_Open[i]<-(DIS$Open[i]-DIS$Open[i-1])/DIS$Open[i-1]
  DIS$NetReturn_High[i]<-(DIS$High[i]-DIS$High[i-1])/DIS$High[i-1]
  DIS$NetReturn_Low[i]<-(DIS$Low[i]-DIS$Low[i-1])/DIS$Low[i-1]
  DIS$NetReturn_Last[i]<-(DIS$Last[i]-DIS$Last[i-1])/DIS$Last[i-1]
}
# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
DIS$week <- week(DIS$Date)
DIS$year <- year(DIS$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
DIS_weekly <- DIS %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(DIS_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

DIS_weekly$LogReturn_Open<-0
DIS_weekly$LogReturn_High<-0
DIS_weekly$LogReturn_Low<-0
DIS_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_weekly)){
  DIS_weekly$LogReturn_Open[i]<-log(DIS_weekly$weekly_open[i]/DIS_weekly$weekly_open[i-1])
  DIS_weekly$LogReturn_High[i]<-log(DIS_weekly$weekly_high[i]/DIS_weekly$weekly_high[i-1])
  DIS_weekly$LogReturn_Low[i]<-log(DIS_weekly$weekly_low[i]/DIS_weekly$weekly_low[i-1])
  DIS_weekly$LogReturn_Close[i]<-log(DIS_weekly$weekly_close[i]/DIS_weekly$weekly_close[i-1])
}

# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
DIS_weekly$NetReturn_Open<-0
DIS_weekly$NetReturn_High<-0
DIS_weekly$NetReturn_Low<-0
DIS_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_weekly)){
  DIS_weekly$NetReturn_Open[i]<-(DIS_weekly$weekly_open[i]-DIS_weekly$weekly_open[i-1])/DIS_weekly$weekly_open[i-1]
  DIS_weekly$NetReturn_High[i]<-(DIS_weekly$weekly_high[i]-DIS_weekly$weekly_high[i-1])/DIS_weekly$weekly_high[i-1]
  DIS_weekly$NetReturn_Low[i]<-(DIS_weekly$weekly_low[i]-DIS_weekly$weekly_low[i-1])/DIS_weekly$weekly_low[i-1]
  DIS_weekly$NetReturn_Last[i]<-(DIS_weekly$weekly_close[i]-DIS_weekly$weekly_close[i-1])/DIS_weekly$weekly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
DIS$month <- month(DIS$Date)
DIS$year <- year(DIS$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
DIS_monthly <- DIS %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
DIS_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
DIS_monthly$LogReturn_Open<-0
DIS_monthly$LogReturn_High<-0
DIS_monthly$LogReturn_Low<-0
DIS_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_monthly)){
  DIS_monthly$LogReturn_Open[i]<-log(DIS_monthly$monthly_open[i]/DIS_monthly$monthly_open[i-1])
  DIS_monthly$LogReturn_High[i]<-log(DIS_monthly$monthly_high[i]/DIS_monthly$monthly_high[i-1])
  DIS_monthly$LogReturn_Low[i]<-log(DIS_monthly$monthly_low[i]/DIS_monthly$monthly_low[i-1])
  DIS_monthly$LogReturn_Close[i]<-log(DIS_monthly$monthly_close[i]/DIS_monthly$monthly_close[i-1])
}

# MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
DIS_monthly$NetReturn_Open<-0
DIS_monthly$NetReturn_High<-0
DIS_monthly$NetReturn_Low<-0
DIS_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_monthly)){
  DIS_monthly$NetReturn_Open[i]<-(DIS_monthly$monthly_open[i]-DIS_monthly$monthly_open[i-1])/DIS_monthly$monthly_open[i-1]
  DIS_monthly$NetReturn_High[i]<-(DIS_monthly$monthly_high[i]-DIS_monthly$monthly_high[i-1])/DIS_monthly$monthly_high[i-1]
  DIS_monthly$NetReturn_Low[i]<-(DIS_monthly$monthly_low[i]-DIS_monthly$monthly_low[i-1])/DIS_monthly$monthly_low[i-1]
  DIS_monthly$NetReturn_Last[i]<-(DIS_monthly$monthly_close[i]-DIS_monthly$monthly_close[i-1])/DIS_monthly$monthly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
DIS_yearly <- DIS %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
DIS_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
DIS_yearly$LogReturn_Open<-0
DIS_yearly$LogReturn_High<-0
DIS_yearly$LogReturn_Low<-0
DIS_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_yearly)){
  DIS_yearly$LogReturn_Open[i]<-log(DIS_yearly$yearly_open[i]/DIS_yearly$yearly_open[i-1])
  DIS_yearly$LogReturn_High[i]<-log(DIS_yearly$yearly_high[i]/DIS_yearly$yearly_high[i-1])
  DIS_yearly$LogReturn_Low[i]<-log(DIS_yearly$yearly_low[i]/DIS_yearly$yearly_low[i-1])
  DIS_yearly$LogReturn_Close[i]<-log(DIS_yearly$yearly_close[i]/DIS_yearly$yearly_close[i-1])
}
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
DIS_yearly$NetReturn_Open<-0
DIS_yearly$NetReturn_High<-0
DIS_yearly$NetReturn_Low<-0
DIS_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS_yearly)){
  DIS_yearly$NetReturn_Open[i]<-(DIS_yearly$yearly_open[i]-DIS_yearly$yearly_open[i-1])/DIS_yearly$yearly_open[i-1]
  DIS_yearly$NetReturn_High[i]<-(DIS_yearly$yearly_high[i]-DIS_yearly$yearly_high[i-1])/DIS_yearly$yearly_high[i-1]
  DIS_yearly$NetReturn_Low[i]<-(DIS_yearly$yearly_low[i]-DIS_yearly$yearly_low[i-1])/DIS_yearly$yearly_low[i-1]
  DIS_yearly$NetReturn_Last[i]<-(DIS_yearly$yearly_close[i]-DIS_yearly$yearly_close[i-1])/DIS_yearly$yearly_close[i-1]
}



#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
DIS_yearly$NetReturns_Open_UK <- "/"
DIS_yearly$NetReturns_Open_UK[1] <- sd(DIS_yearly$NetReturn_Open)
DIS_yearly$NetReturns_High_UK <- "/"
DIS_yearly$NetReturns_High_UK[1] <- sd(DIS_yearly$NetReturn_High)
DIS_yearly$NetReturns_Low_UK<- "/"
DIS_yearly$NetReturns_Low_UK[1] <- sd(DIS_yearly$NetReturn_Low)
DIS_yearly$NetReturns_Last_UK<- "/"
DIS_yearly$NetReturns_Last_UK[1] <- sd(DIS_yearly$NetReturn_Last)
#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#DIS<-DIS[,10]
#install.packages("quantmod")
library(quantmod)

DIS<-DIS[,-10]
DIS_yearly$Volatility_Open <- "/"
DIS_yearly$Volatility_Open[1] <- sd(DIS_yearly$LogReturn_Open)
DIS_yearly$Volatility_High <- "/"
DIS_yearly$Volatility_High[1] <- sd(DIS_yearly$LogReturn_High)
DIS_yearly$Volatility_Low <- "/"
DIS_yearly$Volatility_Low[1] <- sd(DIS_yearly$LogReturn_Low)
DIS_yearly$Volatility_Last <- "/"
DIS_yearly$Volatility_Last[1] <- sd(DIS_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
DIS_yearly_volatility <- DIS %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
DIS_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(DIS)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(DIS[1])<-"Date"

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS <- plot_ly(data = DIS, type = "candlestick",
                    x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS <- fig_DIS %>% layout(title = "DIS Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# DISplay the chart
fig_DIS

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS_lr_d <- plot_ly(data = DIS, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_d <- fig_DIS_lr_d %>% layout(title = "DIS Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# DISplay the chart
fig_DIS_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS_lr_w <- plot_ly(data = DIS_weekly, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_w <- fig_DIS_lr_w %>% layout(title = "DIS Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# DISplay the chart
fig_DIS_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
DIS_monthly$Month<- as.character(DIS_monthly$Month)

# Create a candlestick chart
fig_DIS_lr_m <- plot_ly(data = DIS_monthly, type = "candlestick",
                         x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_m <- fig_DIS_lr_m %>% layout(title = "DIS Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# DISplay the chart
fig_DIS_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS_yearly$year<- as.character(DIS_yearly$year)

# Create a candlestick chart
fig_DIS_lr_y <- plot_ly(data = DIS_yearly, type = "candlestick",
                         x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_y <- fig_DIS_lr_y %>% layout(title = "DIS Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# DISplay the chart
fig_DIS_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
DIS$MA5 <- rollmean(DIS$Last, k = 5, fill = NA)
DIS$MA21 <- rollmean(DIS$Last, k = 21, fill = NA)
DIS$MA63 <- rollmean(DIS$Last, k = 63, fill = NA)
DIS$MA126 <- rollmean(DIS$Last, k = 126, fill = NA)
DIS$MA252 <- rollmean(DIS$Last, k = 252, fill = NA)

ggplot(DIS, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

dev.off()
ggplot(DIS, aes(x = Date, y = Last,group = 1)) +
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
#prinose (i log i net) iscrtati na line grafiku sa 5 podgrafika:
  # prinos open cene
  # prinos high
  # prinos low
  # prinos close
  # prinos candlestick (OVAJ DEO NE MOZE DA SE URADI, NE MOGU DA NADJEM NACIN DA SPOJIM LINECHART SA CANDLESTICK CHARTOM)
  #--------------------------------------------------------------------------------------------------------------
#DNEVNI
# Grafikon (Log Return)
plot(DIS$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="DIS Open, Close, High i Low Log Return")
lines(DIS$LogReturn_High, type="l", col="blue")
lines(DIS$LogReturn_Low, type="l", col="green")
lines(DIS$LogReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

##### sa candlestickom ---------NE RADI

# Učitavanje potrebnih paketa
library(ggplot2)
install.packages("reshape2")
library(reshape2)


# Reshapeovanje podataka
DIS.m <- melt(DIS[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(DIS.m, aes(Date, value)) +
  geom_line(data = subset(DIS.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = DIS, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "DIS Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))


# Grafikon (Net Return)
plot(DIS$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="DIS Open, Close, High i Low Net Return")
lines(DIS$NetReturn_High, type="l", col="blue")
lines(DIS$NetReturn_Low, type="l", col="green")
lines(DIS$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)












