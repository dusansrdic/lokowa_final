# NEDELJNI LOG RETURN 

############## STARO RESENJE (nedeljni) ################

for (i in nrow(TTWO)) {
  TTWO$LogReturn_Open_w<-"/"
  TTWO$LogReturn_High_w<-"/"
  TTWO$LogReturn_Low_w<-"/"
  TTWO$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(TTWO)){
  if(weekdays.POSIXt(TTWO$`Date (GMT)`[i])=="Friday"){
    for(j in 1:5){
      if(weekdays.POSIXt(TTWO$`Date (GMT)`[i-j])=="Friday"){
        TTWO$LogReturn_Open_w[i]<-ln(TTWO$Open[i]/TTWO$Open[j])
        TTWO$LogReturn_High_w[i]<-ln(TTWO$High[i]/TTWO$High[j])
        TTWO$LogReturn_Low_w[i]<-ln(TTWO$Low[i]/TTWO$Low[j])
        TTWO$LogReturn_Last_w[i]<-ln(TTWO$Last[i]/TTWO$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

############## STARO RESENJE (mesecni) ################

monthly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_TTWO_Open,monthly_returns_TTWO_High,by="Group.1")
low_last<-merge(monthly_returns_TTWO_Low,monthly_returns_TTWO_Last,by="Group.1")
monthly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_TTWO)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_TTWO_Open)
remove(monthly_returns_TTWO_High)
remove(monthly_returns_TTWO_Low)
remove(monthly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

############## STARO RESENJE (godisnji) ################

yearly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_TTWO_Open,yearly_returns_TTWO_High,by="Group.1")
low_last<-merge(yearly_returns_TTWO_Low,yearly_returns_TTWO_Last,by="Group.1")
yearly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_TTWO)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_TTWO_Open)
remove(yearly_returns_TTWO_High)
remove(yearly_returns_TTWO_Low)
remove(yearly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------

############## STARO RESENJE ################

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_TTWO$Open)
volatility_high <- sd(yearly_returns_TTWO$High)
volatility_low <- sd(yearly_returns_TTWO$Low)
volatility_last <- sd(yearly_returns_TTWO$Last)

yearly_returns_TTWO$Volatility_Open[1] <- "/"
yearly_returns_TTWO$Volatility_Open[1] <- sd(yearly_returns_TTWO$Open)
yearly_returns_TTWO$Volatility_High[1] <- "/"
yearly_returns_TTWO$Volatility_High[1] <- sd(yearly_returns_TTWO$High)
yearly_returns_TTWO$Volatility_Low[1] <- "/"
yearly_returns_TTWO$Volatility_Low[1] <- sd(yearly_returns_TTWO$Low)
yearly_returns_TTWO$Volatility_Last[1] <- "/"
yearly_returns_TTWO$Volatility_Last[1] <- sd(yearly_returns_TTWO$Last)