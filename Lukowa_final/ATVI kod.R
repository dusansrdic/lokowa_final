
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
ATVI <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="ATVI")
#meni je bio cudan format datuma pa sam sve pretvarala ---------------
names(ATVI) <- c("Date", "Open", "High", "Low", "Last")
ATVI <- ATVI[-1,]
library(openxlsx)
ATVI$Date <- convertToDateTime(ATVI$Date)
ATVI$Open <- as.numeric(ATVI$Open)
ATVI$High <- as.numeric(ATVI$High)
ATVI$Low <- as.numeric(ATVI$Low)
ATVI$Last <- as.numeric(ATVI$Last)
#----------------------------------------------------------------------

# DESKRIPTIVNA STATISTIKA


ATVI_descriptive_statistics <- data.frame(open=mean(ATVI$Open),high=mean(ATVI$High),low=mean(ATVI$Low),last=mean(ATVI$Last)) 
ATVI_descriptive_statistics[2,] <- c(median(ATVI$Open),median(ATVI$High),median(ATVI$Low),median(ATVI$Last))
library(moments)
ATVI_descriptive_statistics[3,] <- c(skewness(ATVI$Open),skewness(ATVI$High),skewness(ATVI$Low),skewness(ATVI$Last))
ATVI_descriptive_statistics[4,] <- c(kurtosis(ATVI$Open),kurtosis(ATVI$High),kurtosis(ATVI$Low),kurtosis(ATVI$Last))

rownames(ATVI_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
ATVI$LogReturn_Open<-0
ATVI$LogReturn_High<-0
ATVI$LogReturn_Low<-0
ATVI$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI)){
  ATVI$LogReturn_Open[i]<-log(ATVI$Open[i]/ATVI$Open[i-1])
  ATVI$LogReturn_High[i]<-log(ATVI$High[i]/ATVI$High[i-1])
  ATVI$LogReturn_Low[i]<-log(ATVI$Low[i]/ATVI$Low[i-1])
  ATVI$LogReturn_Last[i]<-log(ATVI$Last[i]/ATVI$Last[i-1])
}

# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
ATVI$NetReturn_Open<-0
ATVI$NetReturn_High<-0
ATVI$NetReturn_Low<-0
ATVI$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI)){
  ATVI$NetReturn_Open[i]<-(ATVI$Open[i]-ATVI$Open[i-1])/ATVI$Open[i-1]
  ATVI$NetReturn_High[i]<-(ATVI$High[i]-ATVI$High[i-1])/ATVI$High[i-1]
  ATVI$NetReturn_Low[i]<-(ATVI$Low[i]-ATVI$Low[i-1])/ATVI$Low[i-1]
  ATVI$NetReturn_Last[i]<-(ATVI$Last[i]-ATVI$Last[i-1])/ATVI$Last[i-1]
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
ATVI$week <- week(ATVI$Date)
ATVI$year <- year(ATVI$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
ATVI_weekly <- ATVI %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(ATVI_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

ATVI_weekly$LogReturn_Open<-0
ATVI_weekly$LogReturn_High<-0
ATVI_weekly$LogReturn_Low<-0
ATVI_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_weekly)){
  ATVI_weekly$LogReturn_Open[i]<-log(ATVI_weekly$weekly_open[i]/ATVI_weekly$weekly_open[i-1])
  ATVI_weekly$LogReturn_High[i]<-log(ATVI_weekly$weekly_high[i]/ATVI_weekly$weekly_high[i-1])
  ATVI_weekly$LogReturn_Low[i]<-log(ATVI_weekly$weekly_low[i]/ATVI_weekly$weekly_low[i-1])
  ATVI_weekly$LogReturn_Close[i]<-log(ATVI_weekly$weekly_close[i]/ATVI_weekly$weekly_close[i-1])
}

# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
ATVI_weekly$NetReturn_Open<-0
ATVI_weekly$NetReturn_High<-0
ATVI_weekly$NetReturn_Low<-0
ATVI_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_weekly)){
  ATVI_weekly$NetReturn_Open[i]<-(ATVI_weekly$weekly_open[i]-ATVI_weekly$weekly_open[i-1])/ATVI_weekly$weekly_open[i-1]
  ATVI_weekly$NetReturn_High[i]<-(ATVI_weekly$weekly_high[i]-ATVI_weekly$weekly_high[i-1])/ATVI_weekly$weekly_high[i-1]
  ATVI_weekly$NetReturn_Low[i]<-(ATVI_weekly$weekly_low[i]-ATVI_weekly$weekly_low[i-1])/ATVI_weekly$weekly_low[i-1]
  ATVI_weekly$NetReturn_Last[i]<-(ATVI_weekly$weekly_close[i]-ATVI_weekly$weekly_close[i-1])/ATVI_weekly$weekly_close[i-1]
}
# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
ATVI$month <- month(ATVI$Date)
ATVI$year <- year(ATVI$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
ATVI_monthly <- ATVI %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
ATVI_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
ATVI_monthly$LogReturn_Open<-0
ATVI_monthly$LogReturn_High<-0
ATVI_monthly$LogReturn_Low<-0
ATVI_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_monthly)){
  ATVI_monthly$LogReturn_Open[i]<-log(ATVI_monthly$monthly_open[i]/ATVI_monthly$monthly_open[i-1])
  ATVI_monthly$LogReturn_High[i]<-log(ATVI_monthly$monthly_high[i]/ATVI_monthly$monthly_high[i-1])
  ATVI_monthly$LogReturn_Low[i]<-log(ATVI_monthly$monthly_low[i]/ATVI_monthly$monthly_low[i-1])
  ATVI_monthly$LogReturn_Close[i]<-log(ATVI_monthly$monthly_close[i]/ATVI_monthly$monthly_close[i-1])
}

# MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
ATVI_monthly$NetReturn_Open<-0
ATVI_monthly$NetReturn_High<-0
ATVI_monthly$NetReturn_Low<-0
ATVI_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_monthly)){
  ATVI_monthly$NetReturn_Open[i]<-(ATVI_monthly$monthly_open[i]-ATVI_monthly$monthly_open[i-1])/ATVI_monthly$monthly_open[i-1]
  ATVI_monthly$NetReturn_High[i]<-(ATVI_monthly$monthly_high[i]-ATVI_monthly$monthly_high[i-1])/ATVI_monthly$monthly_high[i-1]
  ATVI_monthly$NetReturn_Low[i]<-(ATVI_monthly$monthly_low[i]-ATVI_monthly$monthly_low[i-1])/ATVI_monthly$monthly_low[i-1]
  ATVI_monthly$NetReturn_Last[i]<-(ATVI_monthly$monthly_close[i]-ATVI_monthly$monthly_close[i-1])/ATVI_monthly$monthly_close[i-1]
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
ATVI_yearly <- ATVI %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
ATVI_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
ATVI_yearly$LogReturn_Open<-0
ATVI_yearly$LogReturn_High<-0
ATVI_yearly$LogReturn_Low<-0
ATVI_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_yearly)){
  ATVI_yearly$LogReturn_Open[i]<-log(ATVI_yearly$yearly_open[i]/ATVI_yearly$yearly_open[i-1])
  ATVI_yearly$LogReturn_High[i]<-log(ATVI_yearly$yearly_high[i]/ATVI_yearly$yearly_high[i-1])
  ATVI_yearly$LogReturn_Low[i]<-log(ATVI_yearly$yearly_low[i]/ATVI_yearly$yearly_low[i-1])
  ATVI_yearly$LogReturn_Close[i]<-log(ATVI_yearly$yearly_close[i]/ATVI_yearly$yearly_close[i-1])
}

#prvo moraju da se naprave kolone u dataframe-u
ATVI_yearly$NetReturn_Open<-0
ATVI_yearly$NetReturn_High<-0
ATVI_yearly$NetReturn_Low<-0
ATVI_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI_yearly)){
  ATVI_yearly$NetReturn_Open[i]<-(ATVI_yearly$yearly_open[i]-ATVI_yearly$yearly_open[i-1])/ATVI_yearly$yearly_open[i-1]
  ATVI_yearly$NetReturn_High[i]<-(ATVI_yearly$yearly_high[i]-ATVI_yearly$yearly_high[i-1])/ATVI_yearly$yearly_high[i-1]
  ATVI_yearly$NetReturn_Low[i]<-(ATVI_yearly$yearly_low[i]-ATVI_yearly$yearly_low[i-1])/ATVI_yearly$yearly_low[i-1]
  ATVI_yearly$NetReturn_Last[i]<-(ATVI_yearly$yearly_close[i]-ATVI_yearly$yearly_close[i-1])/ATVI_yearly$yearly_close[i-1]
}


#net returns ukupno od godisnjeg, VOLATILNOST PRINOSA
ATVI_yearly$NetReturns_Open_UK <- "/"
ATVI_yearly$NetReturns_Open_UK[1] <- sd(ATVI_yearly$NetReturn_Open)
ATVI_yearly$NetReturns_High_UK <- "/"
ATVI_yearly$NetReturns_High_UK[1] <- sd(ATVI_yearly$NetReturn_High)
ATVI_yearly$NetReturns_Low_UK<- "/"
ATVI_yearly$NetReturns_Low_UK[1] <- sd(ATVI_yearly$NetReturn_Low)
ATVI_yearly$NetReturns_Last_UK<- "/"
ATVI_yearly$NetReturns_Last_UK[1] <- sd(ATVI_yearly$NetReturn_Last)
#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#ATVI<-ATVI[,10]
#install.packages("quantmod")
library(quantmod)

ATVI<-ATVI[,-10]
ATVI_yearly$Volatility_Open <- "/"
ATVI_yearly$Volatility_Open[1] <- sd(ATVI_yearly$LogReturn_Open)
ATVI_yearly$Volatility_High <- "/"
ATVI_yearly$Volatility_High[1] <- sd(ATVI_yearly$LogReturn_High)
ATVI_yearly$Volatility_Low <- "/"
ATVI_yearly$Volatility_Low[1] <- sd(ATVI_yearly$LogReturn_Low)
ATVI_yearly$Volatility_Last <- "/"
ATVI_yearly$Volatility_Last[1] <- sd(ATVI_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
ATVI_yearly_volatility <- ATVI %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
ATVI_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(ATVI)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(ATVI[1])<-"Date"

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI <- plot_ly(data = ATVI, type = "candlestick",
                      x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI <- fig_ATVI %>% layout(title = "ATVI Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# ATVIplay the chart
fig_ATVI

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI_lr_d <- plot_ly(data = ATVI, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_d <- fig_ATVI_lr_d %>% layout(title = "ATVI Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# ATVIplay the chart
fig_ATVI_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI_lr_w <- plot_ly(data = ATVI_weekly, type = "candlestick",
                           x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_w <- fig_ATVI_lr_w %>% layout(title = "ATVI Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# ATVIplay the chart
fig_ATVI_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
ATVI_monthly$month<- as.character(ATVI_monthly$month)

# Create a candlestick chart
fig_ATVI_lr_m <- plot_ly(data = ATVI_monthly, type = "candlestick",
                           x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_m <- fig_ATVI_lr_m %>% layout(title = "ATVI Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# ATVIplay the chart
fig_ATVI_lr_m

# YATVIRLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI_yearly$year<- as.character(ATVI_yearly$year)

# Create a candlestick chart
fig_ATVI_lr_y <- plot_ly(data = ATVI_yearly, type = "candlestick",
                           x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                           increasing = list(fillcolor = "green", line = list(color = "green")),
                           decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_y <- fig_ATVI_lr_y %>% layout(title = "ATVI Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# ATVIplay the chart
fig_ATVI_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
ATVI$MA5 <- rollmean(ATVI$Last, k = 5, fill = NA)
ATVI$MA21 <- rollmean(ATVI$Last, k = 21, fill = NA)
ATVI$MA63 <- rollmean(ATVI$Last, k = 63, fill = NA)
ATVI$MA126 <- rollmean(ATVI$Last, k = 126, fill = NA)
ATVI$MA252 <- rollmean(ATVI$Last, k = 252, fill = NA)

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) +
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
plot(ATVI$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="ATVI Open, Close, High i Low Log Return")
lines(ATVI$LogReturn_High, type="l", col="blue")
lines(ATVI$LogReturn_Low, type="l", col="green")
lines(ATVI$LogReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

##### sa candlestickom ---------NE RADI

# Učitavanje potrebnih paketa
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

#---------------------------------------------------------------------------------------------------------------------------------------------------
# Reshapeovanje podataka
ATVI.m <- melt(ATVI[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(ATVI.m, aes(Date, value)) +
  geom_line(data = subset(ATVI.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = ATVI, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "ATVI Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))
#-----------------------------------------------------------------------------------------------------------------------------------------------

# Grafikon (Net Return)
plot(ATVI$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="ATVI Open, Close, High i Low Net Return")
lines(ATVI$NetReturn_High, type="l", col="blue")
lines(ATVI$NetReturn_Low, type="l", col="green")
lines(ATVI$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)









