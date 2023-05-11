
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
EA <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="EA")
names(EA)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

EA_descriptive_statistics <- data.frame(open=mean(EA$Open),high=mean(EA$High),low=mean(EA$Low),last=mean(EA$Last)) 
EA_descriptive_statistics[2,] <- c(median(EA$Open),median(EA$High),median(EA$Low),median(EA$Last))
library(moments)
EA_descriptive_statistics[3,] <- c(skewness(EA$Open),skewness(EA$High),skewness(EA$Low),skewness(EA$Last))
EA_descriptive_statistics[4,] <- c(kurtosis(EA$Open),kurtosis(EA$High),kurtosis(EA$Low),kurtosis(EA$Last))

rownames(EA_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
EA$LogReturn_Open<-0
EA$LogReturn_High<-0
EA$LogReturn_Low<-0
EA$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA)){
  EA$LogReturn_Open[i]<-log(EA$Open[i]/EA$Open[i-1])
  EA$LogReturn_High[i]<-log(EA$High[i]/EA$High[i-1])
  EA$LogReturn_Low[i]<-log(EA$Low[i]/EA$Low[i-1])
  EA$LogReturn_Last[i]<-log(EA$Last[i]/EA$Last[i-1])
}
#cist prinos

EA$NetReturn_Open<-0
EA$NetReturn_High<-0
EA$NetReturn_Low<-0
EA$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA)){
  EA$NetReturn_Open[i]<-(EA$Open[i]-EA$Open[i-1])/EA$Open[i-1]
  EA$NetReturn_High[i]<-(EA$High[i]-EA$High[i-1])/EA$High[i-1]
  EA$NetReturn_Low[i]<-(EA$Low[i]-EA$Low[i-1])/EA$Low[i-1]
  EA$NetReturn_Last[i]<-(EA$Last[i]-EA$Last[i-1])/EA$Last[i-1]
}
# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
EA$week <- week(EA$Date)
EA$year <- year(EA$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
EA_weekly <- EA %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(EA_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

EA_weekly$LogReturn_Open<-0
EA_weekly$LogReturn_High<-0
EA_weekly$LogReturn_Low<-0
EA_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_weekly)){
  EA_weekly$LogReturn_Open[i]<-log(EA_weekly$weekly_open[i]/EA_weekly$weekly_open[i-1])
  EA_weekly$LogReturn_High[i]<-log(EA_weekly$weekly_high[i]/EA_weekly$weekly_high[i-1])
  EA_weekly$LogReturn_Low[i]<-log(EA_weekly$weekly_low[i]/EA_weekly$weekly_low[i-1])
  EA_weekly$LogReturn_Close[i]<-log(EA_weekly$weekly_close[i]/EA_weekly$weekly_close[i-1])
}

#prvo moraju da se naprave kolone u dataframe-u
EA_weekly$NetReturn_Open<-0
EA_weekly$NetReturn_High<-0
EA_weekly$NetReturn_Low<-0
EA_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_weekly)){
  EA_weekly$NetReturn_Open[i]<-(EA_weekly$weekly_open[i]-EA_weekly$weekly_open[i-1])/EA_weekly$weekly_open[i-1]
  EA_weekly$NetReturn_High[i]<-(EA_weekly$weekly_high[i]-EA_weekly$weekly_high[i-1])/EA_weekly$weekly_high[i-1]
  EA_weekly$NetReturn_Low[i]<-(EA_weekly$weekly_low[i]-EA_weekly$weekly_low[i-1])/EA_weekly$weekly_low[i-1]
  EA_weekly$NetReturn_Last[i]<-(EA_weekly$weekly_close[i]-EA_weekly$weekly_close[i-1])/EA_weekly$weekly_close[i-1]
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
EA$month <- month(EA$Date)
EA$year <- year(EA$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
EA_monthly <- EA %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
EA_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
EA_monthly$LogReturn_Open<-0
EA_monthly$LogReturn_High<-0
EA_monthly$LogReturn_Low<-0
EA_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_monthly)){
  EA_monthly$LogReturn_Open[i]<-log(EA_monthly$monthly_open[i]/EA_monthly$monthly_open[i-1])
  EA_monthly$LogReturn_High[i]<-log(EA_monthly$monthly_high[i]/EA_monthly$monthly_high[i-1])
  EA_monthly$LogReturn_Low[i]<-log(EA_monthly$monthly_low[i]/EA_monthly$monthly_low[i-1])
  EA_monthly$LogReturn_Close[i]<-log(EA_monthly$monthly_close[i]/EA_monthly$monthly_close[i-1])
}

#prvo moraju da se naprave kolone u dataframe-u
EA_monthly$NetReturn_Open<-0
EA_monthly$NetReturn_High<-0
EA_monthly$NetReturn_Low<-0
EA_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_monthly)){
  EA_monthly$NetReturn_Open[i]<-(EA_monthly$monthly_open[i]-EA_monthly$monthly_open[i-1])/EA_monthly$monthly_open[i-1]
  EA_monthly$NetReturn_High[i]<-(EA_monthly$monthly_high[i]-EA_monthly$monthly_high[i-1])/EA_monthly$monthly_high[i-1]
  EA_monthly$NetReturn_Low[i]<-(EA_monthly$monthly_low[i]-EA_monthly$monthly_low[i-1])/EA_monthly$monthly_low[i-1]
  EA_monthly$NetReturn_Last[i]<-(EA_monthly$monthly_close[i]-EA_monthly$monthly_close[i-1])/EA_monthly$monthly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
EA_yearly <- EA %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
EA_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
EA_yearly$LogReturn_Open<-0
EA_yearly$LogReturn_High<-0
EA_yearly$LogReturn_Low<-0
EA_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_yearly)){
  EA_yearly$LogReturn_Open[i]<-log(EA_yearly$yearly_open[i]/EA_yearly$yearly_open[i-1])
  EA_yearly$LogReturn_High[i]<-log(EA_yearly$yearly_high[i]/EA_yearly$yearly_high[i-1])
  EA_yearly$LogReturn_Low[i]<-log(EA_yearly$yearly_low[i]/EA_yearly$yearly_low[i-1])
  EA_yearly$LogReturn_Close[i]<-log(EA_yearly$yearly_close[i]/EA_yearly$yearly_close[i-1])
}

#GODISNJI CIST PRINOS
# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
EA_yearly$NetReturn_Open<-0
EA_yearly$NetReturn_High<-0
EA_yearly$NetReturn_Low<-0
EA_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA_yearly)){
  EA_yearly$NetReturn_Open[i]<-(EA_yearly$yearly_open[i]-EA_yearly$yearly_open[i-1])/EA_yearly$yearly_open[i-1]
  EA_yearly$NetReturn_High[i]<-(EA_yearly$yearly_high[i]-EA_yearly$yearly_high[i-1])/EA_yearly$yearly_high[i-1]
  EA_yearly$NetReturn_Low[i]<-(EA_yearly$yearly_low[i]-EA_yearly$yearly_low[i-1])/EA_yearly$yearly_low[i-1]
  EA_yearly$NetReturn_Last[i]<-(EA_yearly$yearly_close[i]-EA_yearly$yearly_close[i-1])/EA_yearly$yearly_close[i-1]
}

#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
EA_yearly$NetReturns_Open_UK <- "/"
EA_yearly$NetReturns_Open_UK[1] <- sd(EA_yearly$NetReturn_Open)
EA_yearly$NetReturns_High_UK <- "/"
EA_yearly$NetReturns_High_UK[1] <- sd(EA_yearly$NetReturn_High)
EA_yearly$NetReturns_Low_UK<- "/"
EA_yearly$NetReturns_Low_UK[1] <- sd(EA_yearly$NetReturn_Low)
EA_yearly$NetReturns_Last_UK<- "/"
EA_yearly$NetReturns_Last_UK[1] <- sd(EA_yearly$NetReturn_Last)
#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#EA<-EA[,10]
#install.packages("quantmod")
library(quantmod)

EA<-EA[,-10]
EA_yearly$Volatility_Open <- "/"
EA_yearly$Volatility_Open[1] <- sd(EA_yearly$LogReturn_Open)
EA_yearly$Volatility_High <- "/"
EA_yearly$Volatility_High[1] <- sd(EA_yearly$LogReturn_High)
EA_yearly$Volatility_Low <- "/"
EA_yearly$Volatility_Low[1] <- sd(EA_yearly$LogReturn_Low)
EA_yearly$Volatility_Last <- "/"
EA_yearly$Volatility_Last[1] <- sd(EA_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
EA_yearly_volatility <- EA %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
EA_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(EA)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(EA[1])<-"Date"

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA <- plot_ly(data = EA, type = "candlestick",
                    x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA <- fig_EA %>% layout(title = "EA Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# EAplay the chart
fig_EA

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA_lr_d <- plot_ly(data = EA, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_d <- fig_EA_lr_d %>% layout(title = "EA Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# EAplay the chart
fig_EA_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA_lr_w <- plot_ly(data = EA_weekly, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_w <- fig_EA_lr_w %>% layout(title = "EA Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# EAplay the chart
fig_EA_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
EA_monthly$month<- as.character(EA_monthly$month)

# Create a candlestick chart
fig_EA_lr_m <- plot_ly(data = EA_monthly, type = "candlestick",
                         x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_m <- fig_EA_lr_m %>% layout(title = "EA Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# EAplay the chart
fig_EA_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA_yearly$year<- as.character(EA_yearly$year)

# Create a candlestick chart
fig_EA_lr_y <- plot_ly(data = EA_yearly, type = "candlestick",
                         x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_y <- fig_EA_lr_y %>% layout(title = "EA Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# EAplay the chart
fig_EA_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
EA$MA5 <- rollmean(EA$Last, k = 5, fill = NA)
EA$MA21 <- rollmean(EA$Last, k = 21, fill = NA)
EA$MA63 <- rollmean(EA$Last, k = 63, fill = NA)
EA$MA126 <- rollmean(EA$Last, k = 126, fill = NA)
EA$MA252 <- rollmean(EA$Last, k = 252, fill = NA)

ggplot(EA, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")


ggplot(EA, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "solid") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "solid") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "solid") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "solid") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "solid") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#------------------
dev.off()


# prinose (i log i net) iscrtati na line grafiku sa 5 podgrafika:
# prinos open cene
# prinos high
# prinos low
# prinos close
# prinos candlestick (OVAJ DEO NE MOZE DA SE URADI, NE MOGU DA NADJEM NACIN DA SPOJIM LINECHART SA CANDLESTICK CHARTOM)

#DNEVNI
# Grafikon (Log Return)
plot(EA$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="EA Open, Close, High i Low Log Return")
lines(EA$LogReturn_High, type="l", col="blue")
lines(EA$LogReturn_Low, type="l", col="green")
lines(EA$LogReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

##### sa candlestickom ---------NE RADI

# Učitavanje potrebnih paketa
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#install.packages("tidyquant")
library(tidyquant)

#****-------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshapeovanje podataka
EA.m <- melt(EA[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(EA.m, aes(Date, value)) +
  geom_line(data = subset(EA.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = EA, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "EA Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))

#***-------------------------------------------------------------------------------------------------------------------------------------------------
# Grafikon (Net Return)
plot(EA$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="EA Open, Close, High i Low Net Return")
lines(EA$NetReturn_High, type="l", col="blue")
lines(EA$NetReturn_Low, type="l", col="green")
lines(EA$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

