#plot sa svom volatilnoscu po barovima
total_volatility_df <- data.frame(c(ATVI_yearly_volatility$close_volatility), c(DIS_yearly_volatility$close_volatility),
                  c(EA_yearly_volatility$close_volatility),c(NASDAQ_yearly_volatility$close_volatility),c(NFLX_yearly_volatility$close_volatility),
                  c(NTDOY_yearly_volatility$close_volatility),c(NWS_yearly_volatility$close_volatility),c(PARA_yearly_volatility$close_volatility),
                  c(SP500_yearly_volatility$close_volatility),c(TCEHY_yearly_volatility$close_volatility), c(TTWO_yearly_volatility$close_volatility),
                  c(WBD_yearly_volatility$close_volatility),year , group = c(2013,2014, 2015,2016, 2017,2018,2019,2020, 2021, 2022))
total_volatility_df$group <-as.factor(total_volatility_df$group)
colnames(total_volatility_df)<- c('ATVI', "DIS", "EA", 'NASDAQ', 'NFLX', 'NTDOY','NWS', 'PARA', 'SP500', 'TCEHY','TTWO', 'WBD','year','group')

#test - prvi bar plot
barplot(atvi$ATVI, xlab = "year", ylab="atvi volatility")

#OVO JE DOBRO, OVO SU TRAZILI
par(mfrow = c(4,3))
atvi_barplot <- barplot(total_volatility_df$ATVI, xlab = "ATVI year", ylab="volatility", col = "blue")
dis_barplot <- barplot(total_volatility_df$DIS, xlab = "DIS year", ylab="volatility",  col = "yellow")
ea_barplot <- barplot(total_volatility_df$EA, xlab = " EA year", ylab="volatility",  col = "red")
nasdaq_barplot <- barplot(total_volatility_df$NASDAQ, xlab = "NASDAQ year", ylab="volatility",   col = "green")
nflx_barplot <- barplot(total_volatility_df$NFLX, xlab = "NFLX year", ylab="volatility",   col = "magenta")
ntdoy_barplot <- barplot(total_volatility_df$NTDOY, xlab = "NTDOY year", ylab="volatility",  col = "white")
nws_barplot <- barplot(total_volatility_df$NWS, xlab = "NWS year", ylab="volatility", col = "orange")
para_barplot <- barplot(total_volatility_df$PARA, xlab = "PARA year", ylab="volatility",  col = "brown")
sp500_barplot <- barplot(total_volatility_df$SP500, xlab = " SP500 year", ylab="volatility", col = "lavender")
tcehy_barplot <- barplot(total_volatility_df$TCEHY, xlab = "TCEHY year", ylab="volatility",  col = "purple")
ttwo_barplot <- barplot(total_volatility_df$TTWO, xlab = "TTWO year", ylab="volatility", col = "navy")
wbd_barplot <- barplot(total_volatility_df$WBD, xlab = "WBD year", ylab="volatility", col = "grey")


