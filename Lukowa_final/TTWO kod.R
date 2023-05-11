
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
TTWO <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="TTWO")
names(TTWO)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

TTWO_descriptive_statistics <- data.frame(open=mean(TTWO$Open),high=mean(TTWO$High),low=mean(TTWO$Low),last=mean(TTWO$Last)) 
TTWO_descriptive_statistics[2,] <- c(median(TTWO$Open),median(TTWO$High),median(TTWO$Low),median(TTWO$Last))
library(moments)
TTWO_descriptive_statistics[3,] <- c(skewness(TTWO$Open),skewness(TTWO$High),skewness(TTWO$Low),skewness(TTWO$Last))
TTWO_descriptive_statistics[4,] <- c(kurtosis(TTWO$Open),kurtosis(TTWO$High),kurtosis(TTWO$Low),kurtosis(TTWO$Last))

rownames(TTWO_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN 

#prvo moraju da se naprave kolone u dataframe-u
TTWO$LogReturn_Open<-0
TTWO$LogReturn_High<-0
TTWO$LogReturn_Low<-0
TTWO$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO)){
  TTWO$LogReturn_Open[i]<-log(TTWO$Open[i]/TTWO$Open[i-1])
  TTWO$LogReturn_High[i]<-log(TTWO$High[i]/TTWO$High[i-1])
  TTWO$LogReturn_Low[i]<-log(TTWO$Low[i]/TTWO$Low[i-1])
  TTWO$LogReturn_Last[i]<-log(TTWO$Last[i]/TTWO$Last[i-1])
}

# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO$NetReturn_Open<-0
TTWO$NetReturn_High<-0
TTWO$NetReturn_Low<-0
TTWO$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO)){
  TTWO$NetReturn_Open[i]<-(TTWO$Open[i]-TTWO$Open[i-1])/TTWO$Open[i-1]
  TTWO$NetReturn_High[i]<-(TTWO$High[i]-TTWO$High[i-1])/TTWO$High[i-1]
  TTWO$NetReturn_Low[i]<-(TTWO$Low[i]-TTWO$Low[i-1])/TTWO$Low[i-1]
  TTWO$NetReturn_Last[i]<-(TTWO$Last[i]-TTWO$Last[i-1])/TTWO$Last[i-1]
}


# ///////////////////////////


# GRUPISANJE PODATAKA PO NEDELJAMA

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
TTWO$week <- week(TTWO$Date)
TTWO$year <- year(TTWO$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
TTWO_weekly <- TTWO %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(TTWO_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

TTWO_weekly$LogReturn_Open<-0
TTWO_weekly$LogReturn_High<-0
TTWO_weekly$LogReturn_Low<-0
TTWO_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_weekly)){
  TTWO_weekly$LogReturn_Open[i]<-log(TTWO_weekly$weekly_open[i]/TTWO_weekly$weekly_open[i-1])
  TTWO_weekly$LogReturn_High[i]<-log(TTWO_weekly$weekly_high[i]/TTWO_weekly$weekly_high[i-1])
  TTWO_weekly$LogReturn_Low[i]<-log(TTWO_weekly$weekly_low[i]/TTWO_weekly$weekly_low[i-1])
  TTWO_weekly$LogReturn_Close[i]<-log(TTWO_weekly$weekly_close[i]/TTWO_weekly$weekly_close[i-1])
}

# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_weekly$NetReturn_Open<-0
TTWO_weekly$NetReturn_High<-0
TTWO_weekly$NetReturn_Low<-0
TTWO_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_weekly)){
  TTWO_weekly$NetReturn_Open[i]<-(TTWO_weekly$weekly_open[i]-TTWO_weekly$weekly_open[i-1])/TTWO_weekly$weekly_open[i-1]
  TTWO_weekly$NetReturn_High[i]<-(TTWO_weekly$weekly_high[i]-TTWO_weekly$weekly_high[i-1])/TTWO_weekly$weekly_high[i-1]
  TTWO_weekly$NetReturn_Low[i]<-(TTWO_weekly$weekly_low[i]-TTWO_weekly$weekly_low[i-1])/TTWO_weekly$weekly_low[i-1]
  TTWO_weekly$NetReturn_Last[i]<-(TTWO_weekly$weekly_close[i]-TTWO_weekly$weekly_close[i-1])/TTWO_weekly$weekly_close[i-1]
}

# ///////////////////////////

# GRUPISANJE PODATAKA PO MESECIMA

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
TTWO$month <- month(TTWO$Date)
TTWO$year <- year(TTWO$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TTWO_monthly <- TTWO %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
TTWO_monthly

# /////////////////////////


# MESECNI LOG RETURN

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_monthly$LogReturn_Open<-0
TTWO_monthly$LogReturn_High<-0
TTWO_monthly$LogReturn_Low<-0
TTWO_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_monthly)){
  TTWO_monthly$LogReturn_Open[i]<-log(TTWO_monthly$monthly_open[i]/TTWO_monthly$monthly_open[i-1])
  TTWO_monthly$LogReturn_High[i]<-log(TTWO_monthly$monthly_high[i]/TTWO_monthly$monthly_high[i-1])
  TTWO_monthly$LogReturn_Low[i]<-log(TTWO_monthly$monthly_low[i]/TTWO_monthly$monthly_low[i-1])
  TTWO_monthly$LogReturn_Close[i]<-log(TTWO_monthly$monthly_close[i]/TTWO_monthly$monthly_close[i-1])
}

## MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_monthly$NetReturn_Open<-0
TTWO_monthly$NetReturn_High<-0
TTWO_monthly$NetReturn_Low<-0
TTWO_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_monthly)){
  TTWO_monthly$NetReturn_Open[i]<-(TTWO_monthly$monthly_open[i]-TTWO_monthly$monthly_open[i-1])/TTWO_monthly$monthly_open[i-1]
  TTWO_monthly$NetReturn_High[i]<-(TTWO_monthly$monthly_high[i]-TTWO_monthly$monthly_high[i-1])/TTWO_monthly$monthly_high[i-1]
  TTWO_monthly$NetReturn_Low[i]<-(TTWO_monthly$monthly_low[i]-TTWO_monthly$monthly_low[i-1])/TTWO_monthly$monthly_low[i-1]
  TTWO_monthly$NetReturn_Last[i]<-(TTWO_monthly$monthly_close[i]-TTWO_monthly$monthly_close[i-1])/TTWO_monthly$monthly_close[i-1]
}
#


# ///////////////////////////

# GRUPISANJE PODATAKA PO GODINAMA

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TTWO_yearly <- TTWO %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
TTWO_yearly

# /////////////////////////

# GODISNJI LOG RETURN

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_yearly$LogReturn_Open<-0
TTWO_yearly$LogReturn_High<-0
TTWO_yearly$LogReturn_Low<-0
TTWO_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_yearly)){
  TTWO_yearly$LogReturn_Open[i]<-log(TTWO_yearly$yearly_open[i]/TTWO_yearly$yearly_open[i-1])
  TTWO_yearly$LogReturn_High[i]<-log(TTWO_yearly$yearly_high[i]/TTWO_yearly$yearly_high[i-1])
  TTWO_yearly$LogReturn_Low[i]<-log(TTWO_yearly$yearly_low[i]/TTWO_yearly$yearly_low[i-1])
  TTWO_yearly$LogReturn_Close[i]<-log(TTWO_yearly$yearly_close[i]/TTWO_yearly$yearly_close[i-1])
}

# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_yearly$NetReturn_Open<-0
TTWO_yearly$NetReturn_High<-0
TTWO_yearly$NetReturn_Low<-0
TTWO_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_yearly)){
  TTWO_yearly$NetReturn_Open[i]<-(TTWO_yearly$yearly_open[i]-TTWO_yearly$yearly_open[i-1])/TTWO_yearly$yearly_open[i-1]
  TTWO_yearly$NetReturn_High[i]<-(TTWO_yearly$yearly_high[i]-TTWO_yearly$yearly_high[i-1])/TTWO_yearly$yearly_high[i-1]
  TTWO_yearly$NetReturn_Low[i]<-(TTWO_yearly$yearly_low[i]-TTWO_yearly$yearly_low[i-1])/TTWO_yearly$yearly_low[i-1]
  TTWO_yearly$NetReturn_Last[i]<-(TTWO_yearly$yearly_close[i]-TTWO_yearly$yearly_close[i-1])/TTWO_yearly$yearly_close[i-1]
}

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------

#---
# Volatilnost za raw prices
#---

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i izracunaj podatke
TTWO_yearly_volatility <- TTWO %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
TTWO_yearly_volatility

# ukupan raw price volatility za sve godine

TTWO_yearly_volatility$open_volatility_uk <- "/"
TTWO_yearly_volatility$open_volatility_uk[1] <- sd(TTWO_yearly_volatility$open_volatility)
TTWO_yearly_volatility$high_volatility_uk <- "/"
TTWO_yearly_volatility$high_volatility_uk[1] <- sd(TTWO_yearly_volatility$high_volatility)
TTWO_yearly_volatility$low_volatility_uk<- "/"
TTWO_yearly_volatility$low_volatility_uk[1] <- sd(TTWO_yearly_volatility$low_volatility)
TTWO_yearly_volatility$close_volatility_uk<- "/"
TTWO_yearly_volatility$close_volatility_uk[1] <- sd(TTWO_yearly_volatility$close_volatility)

#---
# Volatilnost za log returns
#---

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i izracunaj podatke
TTWO_yearly_volatility <- TTWO %>%
  group_by(year) %>%
  summarise(log_return_open_volatility = sd(LogReturn_Open),
            log_return_high_volatility = sd(LogReturn_High),
            log_return_low_volatility = sd(LogReturn_Low),
            log_return_close_volatility = sd(LogReturn_Last))

# Prikazi rezultate
TTWO_yearly_volatility

# ukupan log return volatility za sve godine 

TTWO_yearly_volatility$log_return_open_volatility_uk <- "/"
TTWO_yearly_volatility$log_return_open_volatility_uk[1] <- sd(TTWO_yearly_volatility$log_return_open_volatility)
TTWO_yearly_volatility$log_return_high_volatility_uk <- "/"
TTWO_yearly_volatility$log_return_high_volatility_uk[1] <- sd(TTWO_yearly_volatility$log_return_high_volatility)
TTWO_yearly_volatility$log_return_low_volatility_uk<- "/"
TTWO_yearly_volatility$log_return_low_volatility_uk[1] <- sd(TTWO_yearly_volatility$log_return_low_volatility)
TTWO_yearly_volatility$log_return_close_volatility_uk<- "/"
TTWO_yearly_volatility$log_return_close_volatility_uk[1] <- sd(TTWO_yearly_volatility$log_return_close_volatility)


#---
# Volatilnost za net return
#---

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i izracunaj podatke
TTWO_yearly_volatility <- TTWO %>%
  group_by(year) %>%
  summarise(net_return_open_volatility = sd(NetReturn_Open),
            net_return_high_volatility = sd(NetReturn_High),
            net_return_low_volatility = sd(NetReturn_Low),
            net_return_close_volatility = sd(NetReturn_Last))

# Prikazi rezultate
TTWO_yearly_volatility

# ukupan net return volatility za sve godine 

TTWO_yearly_volatility$net_return_open_volatility_uk <- "/"
TTWO_yearly_volatility$net_return_open_volatility_uk[1] <- sd(TTWO_yearly_volatility$net_return_open_volatility)
TTWO_yearly_volatility$net_return_high_volatility_uk <- "/"
TTWO_yearly_volatility$net_return_high_volatility_uk[1] <- sd(TTWO_yearly_volatility$net_return_high_volatility)
TTWO_yearly_volatility$net_return_low_volatility_uk<- "/"
TTWO_yearly_volatility$net_return_low_volatility_uk[1] <- sd(TTWO_yearly_volatility$net_return_low_volatility)
TTWO_yearly_volatility$net_return_close_volatility_uk<- "/"
TTWO_yearly_volatility$net_return_close_volatility_uk[1] <- sd(TTWO_yearly_volatility$net_return_close_volatility)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(TTWO)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries

#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(TTWO[1])<-"Date"

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO <- plot_ly(data = TTWO, type = "candlestick",
                    x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO <- fig_TTWO %>% layout(title = "TTWO Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_d <- plot_ly(data = TTWO, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_d <- fig_TTWO_lr_d %>% layout(title = "TTWO Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_w <- plot_ly(data = TTWO_weekly, type = "candlestick",
                         x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_w <- fig_TTWO_lr_w %>% layout(title = "TTWO Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
TTWO_monthly$Month<- as.character(TTWO_monthly$Month)

# Create a candlestick chart
fig_TTWO_lr_m <- plot_ly(data = TTWO_monthly, type = "candlestick",
                         x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_m <- fig_TTWO_lr_m %>% layout(title = "TTWO Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO_yearly$year<- as.character(TTWO_yearly$year)

# Create a candlestick chart
fig_TTWO_lr_y <- plot_ly(data = TTWO_yearly, type = "candlestick",
                         x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                         increasing = list(fillcolor = "green", line = list(color = "green")),
                         decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_y <- fig_TTWO_lr_y %>% layout(title = "TTWO Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21, 63, 126 and 252
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
TTWO$MA5 <- rollmean(TTWO$Last, k = 5, fill = NA)
TTWO$MA21 <- rollmean(TTWO$Last, k = 21, fill = NA)
TTWO$MA63 <- rollmean(TTWO$Last, k = 63, fill = NA)
TTWO$MA126 <- rollmean(TTWO$Last, k = 126, fill = NA)
TTWO$MA252 <- rollmean(TTWO$Last, k = 252, fill = NA)

ggplot(TTWO, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

#------------------
ggplot(TTWO, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "solid") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "solid") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "solid") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "solid") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "solid") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# prinose (i log i net) iscrtati na line grafiku sa 4 podgrafika:
# prinos open cene
# prinos high
# prinos low
# prinos close
#--------------------------------------------------------------------------------------------------------------
#DNEVNI

# Grafikon (Log Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO$LogReturn_Open, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO$LogReturn_Last, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO$LogReturn_High, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO$LogReturn_Low, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO$LogReturn_Open, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Open")
plot(TTWO$LogReturn_Last, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Close")
plot(TTWO$LogReturn_High, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - High")
plot(TTWO$LogReturn_Low, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Low")



# Grafikon (Net Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO$NetReturn_Open, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO$NetReturn_Last, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO$NetReturn_High, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO$NetReturn_Low, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO$NetReturn_Open, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Open")
plot(TTWO$NetReturn_Last, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Close")
plot(TTWO$NetReturn_High, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - High")
plot(TTWO$NetReturn_Low, type = "l", xlab = "Dan", ylab = "Cena", main = "TTWO - Low")


# NEDELJNI

# Grafikon (Log Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_weekly$LogReturn_Open, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Open", col = "blue")
plot(TTWO_weekly$LogReturn_Last, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Close", col = "red")
plot(TTWO_weekly$LogReturn_High, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - High", col = "green")
plot(TTWO_weekly$LogReturn_Low, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_weekly$LogReturn_Open, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Open")
plot(TTWO_weekly$LogReturn_Last, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Close")
plot(TTWO_weekly$LogReturn_High, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - High")
plot(TTWO_weekly$LogReturn_Low, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Low")

# Grafikon (Net Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_weekly$NetReturn_Open, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Open", col = "blue")
plot(TTWO_weekly$NetReturn_Last, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Close", col = "red")
plot(TTWO_weekly$NetReturn_High, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - High", col = "green")
plot(TTWO_weekly$NetReturn_Low, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_weekly$NetReturn_Open, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Open")
plot(TTWO_weekly$NetReturn_Last, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Close")
plot(TTWO_weekly$NetReturn_High, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - High")
plot(TTWO_weekly$NetReturn_Low, type = "l", xlab = "Nedelja", ylab = "Cena", main = "TTWO (w) - Low")


# MESECNI

# Grafikon (Log Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_monthly$LogReturn_Open, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO_monthly$LogReturn_Last, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO_monthly$LogReturn_High, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO_monthly$LogReturn_Low, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_monthly$LogReturn_Open, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Open")
plot(TTWO_monthly$LogReturn_Last, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Close")
plot(TTWO_monthly$LogReturn_High, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - High")
plot(TTWO_monthly$LogReturn_Low, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Low")

# Grafikon (Net Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_monthly$NetReturn_Open, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO_monthly$NetReturn_Last, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO_monthly$NetReturn_High, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO_monthly$NetReturn_Low, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_monthly$NetReturn_Open, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Open")
plot(TTWO_monthly$NetReturn_Last, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Close")
plot(TTWO_monthly$NetReturn_High, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - High")
plot(TTWO_monthly$NetReturn_Low, type = "l", xlab = "Mesec", ylab = "Cena", main = "TTWO - Low")


# GODISNJI

# Grafikon (Log Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_yearly$LogReturn_Open, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO_yearly$LogReturn_Last, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO_yearly$LogReturn_High, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO_yearly$LogReturn_Low, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_yearly$LogReturn_Open, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Open")
plot(TTWO_yearly$LogReturn_Last, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Close")
plot(TTWO_yearly$LogReturn_High, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - High")
plot(TTWO_yearly$LogReturn_Low, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Low")

# Grafikon (Net Return)

# sa bojama

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_yearly$NetReturn_Open, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Open", col = "blue")
plot(TTWO_yearly$NetReturn_Last, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Close", col = "red")
plot(TTWO_yearly$NetReturn_High, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - High", col = "green")
plot(TTWO_yearly$NetReturn_Low, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Low", col = "orange")

# crna boja

layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

# Kreiranje svih grafikona i dodavanje ih u odgovarajuće panele
plot(TTWO_yearly$NetReturn_Open, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Open")
plot(TTWO_yearly$NetReturn_Last, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Close")
plot(TTWO_yearly$NetReturn_High, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - High")
plot(TTWO_yearly$NetReturn_Low, type = "l", xlab = "Godina", ylab = "Cena", main = "TTWO - Low")










