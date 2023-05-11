#volatilnost tabela svih
total_df <- cbind(c(ATVI_yearly_volatility$close_volatility), c(DIS_yearly_volatility$close_volatility),
                  c(EA_yearly_volatility$close_volatility),c(NASDAQ_yearly_volatility$close_volatility),c(NFLX_yearly_volatility$close_volatility),
                  c(NTDOY_yearly_volatility$close_volatility),c(NWS_yearly_volatility$close_volatility),c(PARA_yearly_volatility$close_volatility),
                  c(SP500_yearly_volatility$close_volatility),c(TCEHY_yearly_volatility$close_volatility), c(TTWO_yearly_volatility$close_volatility),
                  c(WBD_yearly_volatility$close_volatility))
colnames(total_df)<- c('V.ATVI', "V.DIS", "V.EA", 'V.NASDAQ', 'V.NFLX', 'V.NTDOY','V.NWS', 'V.PARA', 'V.SP500', 'V.TCEHY','V.TTWO', 'V.WBD')


year <- c(2013,2014, 2015,2016, 2017,2018,2019,2020, 2021, 2022)
total_df <- cbind(total_df,year=c(2013,2014, 2015,2016, 2017,2018,2019,2020, 2021, 2022))
#total_df$year <- factor(total_df$year)
#str(total_df)
#total_df <- t(total_df)
#total_df <- total_df[-13,]
#ggplot(total_df) +
 # geom_bar(aes(x = 2013)) 

#returns tabela svih
total_rtns_df <- cbind(ATVI_yearly$NetReturn_Last,DIS_yearly$NetReturn_Last,EA_yearly$NetReturn_Last,
                       NASDAQ_yearly$NetReturn_Last, NFLX_yearly$NetReturn_Last, NTDOY_yearly$NetReturn_Last, NWS_yearly$NetReturn_Last,
                       PARA_yearly$NetReturn_Last, SP500_yearly$NetReturn_Last, TCEHY_yearly$NetReturn_Last, TTWO_yearly$NetReturn_Last,
                       WBD_yearly$NetReturn_Last)

year <- c(2013,2014, 2015,2016, 2017,2018,2019,2020, 2021, 2022)
total_rtns_df <- cbind(total_rtns_df,year=c(2013,2014, 2015,2016, 2017,2018,2019,2020, 2021, 2022))

colnames(total_rtns_df)<- c("R.ATVI", "R.DIS", "R.EA", 'R.NASDAQ', 'R.NFLX', 'R.NTDOY','R.NWS', 'R.PARA', 'R.SP500', 'R.TCEHY','R.TTWO', 'R.WBD','R.year')

#row.names(total_rtns_df) <- c("2013","2014", "2015","2016", "2017", "2018","2019","2020", "2021", "2022")

#drugi grafik volatilnost sa barovima

#---------------------------------------------------------- primer
# Load the ggplot2 package
library(ggplot2)



# Combine the data frames, spajamo sve u jedan data frame
TOTAL_TOTAL_PLOT <- cbind(total_df, total_rtns_df)
TOTAL_TOTAL_PLOT<- data.frame(TOTAL_TOTAL_PLOT)


#ovo je svaka komapnija da ima svoj plot sa tackicama
par(mfrow = c(2,1))
ggplot(TOTAL_TOTAL_PLOT , aes(x=V.DIS, y= R.DIS))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.EA, y= R.EA))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.NASDAQ, y= R.NASDAQ))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.NFLX, y= R.NFLX))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.NTDOY, y= R.NTDOY))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.NWS, y= R.NWS))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.PARA, y= R.PARA))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.SP500, y= R.SP500))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.TCEHY, y= R.TCEHY))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.TTWO, y= R.TTWO))+ geom_point()
ggplot(TOTAL_TOTAL_PLOT, aes(x=V.WBD, y= R.WBD))+ geom_point()



##########################################################################################################################################3

#OVO JE DOBRO, da na jednom plotu budu sve kompanije razlicitim bojama
#df gde idu prvo komp po godinama
TOTAL_TOTAL_RV <- data.frame(R= c(TOTAL_TOTAL_PLOT$R.ATVI, TOTAL_TOTAL_PLOT$R.DIS, TOTAL_TOTAL_PLOT$R.EA, TOTAL_TOTAL_PLOT$R.NASDAQ,TOTAL_TOTAL_PLOT$R.NFLX,
                                  TOTAL_TOTAL_PLOT$R.NTDOY,
                                  TOTAL_TOTAL_PLOT$R.NWS, TOTAL_TOTAL_PLOT$R.PARA,TOTAL_TOTAL_PLOT$R.SP500, TOTAL_TOTAL_PLOT$R.TCEHY,TOTAL_TOTAL_PLOT$R.TTWO, 
                                  TOTAL_TOTAL_PLOT$R.WBD),
                             V = c(TOTAL_TOTAL_PLOT$V.ATVI, TOTAL_TOTAL_PLOT$V.DIS, TOTAL_TOTAL_PLOT$V.EA, TOTAL_TOTAL_PLOT$V.NASDAQ,TOTAL_TOTAL_PLOT$V.NFLX,
                                   TOTAL_TOTAL_PLOT$V.NTDOY,
                                   TOTAL_TOTAL_PLOT$V.NWS, TOTAL_TOTAL_PLOT$V.PARA,TOTAL_TOTAL_PLOT$V.SP500, TOTAL_TOTAL_PLOT$V.TCEHY,TOTAL_TOTAL_PLOT$V.TTWO, 
                                   TOTAL_TOTAL_PLOT$V.WBD),
                             year= rep(as.character(2013:2022)),
                             company = c(rep("ATVI", times=10),rep("DIS", times=10), rep("EA", times=10), rep("NASDAQ", times=10),rep("NFLX", times=10),
                                         rep("NTDOY", times=10), rep("NWS", times=10), rep("PARA", times=10), rep("SP500", times=10), rep("TCEHY", times=10),
                                         rep("TTWO", times=10),rep("WBD", times=10)),
                             color = c(rep("blue", times=10),rep("brown", times=10),rep("green", times=10),rep("grey", times=10),rep("lavender", times=10),rep("magenta", times=10),
                                         rep("navy", times=10),rep("orange", times=10),rep("purple", times=10),rep("red", times=10),
                                         rep("white", times=10),rep("yellow", times=10)))
                             

TOTAL_TOTAL_RV$year_company <- paste(TOTAL_TOTAL_RV$company, TOTAL_TOTAL_RV$year, sep = "\n")

#da pise ime kad se predje misem OVO JE DOBRO
library(plotly)
rv <- ggplot(TOTAL_TOTAL_RV, aes(V, R,text=year_company)) +
  geom_point(aes(color = color), size = 2) +
  scale_color_manual(values = c("blue","brown", "green","grey","lavender", "magenta", "navy","orange","purple","red", "white", "yellow"))
  
ggplotly(rv)

###########################3
#ovo sve dole je isprobavanje  koda i ne mmora se pokretati
#da pise ime na plotu OVO NIJE DOBRO
ggplot(TOTAL_TOTAL_RV, aes(V, R)) +
  geom_point(aes(color = color), size = 2) +
  geom_text(aes(label = year_company), nudge_x = 0.1, nudge_y = 0.1)+
  scale_color_manual(values = TOTAL_TOTAL_RV$color)
colors = rep(c("blue","yellow", "red","green","magenta", "orange","brown","purple", "navy", "grey"), times=12)
color = rep(c("blue", times=10),rep("yellow", times=10),rep("red", times=10),rep("green", times=10),rep("magenta", times=10),rep("white", times=10),
            rep("orange", times=10),rep("brown", times=10),rep("lavender", times=10),rep("purple", times=10),rep("navy", times=10),rep("grey", times=10))
#PRIMERIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
#---------------------------------------------------------- primer
# Load the ggplot2 package
library(ggplot2)

# Create two data frames
df1 <- data.frame(x = 1:10, y = rnorm(10))
df2 <- data.frame(x = 1:10, y = rnorm(10))

# Combine the data frames
combined_df <- rbind(df1, df2)

# Create a new column in the combined data frame to distinguish between the two original data frames
combined_df$group <- rep(c("Anja", "Stojanovic"), each = nrow(df1))

# Plot the combined data frame using ggplot
ggplot(combined_df, aes(x = x, y = y, color = group)) + 
  geom_point() + 
  labs(title = "Two Data Frames in One Plot", x = "X", y = "Y")
#-------------------------------------------------------------------primer


library(ggplot2)

# Create a data frame
data <- data.frame(x = c(1, 2, 3, 4, 5),
                   y = c(2, 4, 3, 6, 5),
                   group = c("A", "A", "B", "B", "B"))

# Create a line plot
ggplot(data, aes(x = x, y = y, group = group)) +
  geom_line()


# Create a vector of values
values <- c(10, 20, 30, 40, 50)

# Plot a bar chart
barplot(values)
#---------------------------------------------------------------primer
# Load required packages
library(reshape2)
library(ggplot2)

# Create example data
dfAAA <- data.frame(category = rep(c("A", "B"), each = 6),
                 subcategory = rep(c("X", "Y", "Z","S"), times = 3),
                 value = rnorm(12))

# Reshape the data
df_meltAAA <- melt(dfAAA, id.vars = c("category", "subcategory"))

# Create the plot
ggplot(df_meltAAA, aes(x = category, y = subcategory, fill = value)) +
  geom_point()+
  scale_fill_manual(values = c("red", "green", "blue",'magenta')) +
  coord_flip() +
  theme_minimal()

#-----------------------------------primer
library(ggplot2)
points <- data.frame(
  x = c(1, 2, 3),
  y = c(4, 5, 6),
  names = c("Point A", "Point B", "Point C"),
  colors = c("red", "blue", "green")
)
ggplot(points, aes(x, y)) +
  geom_point(aes(color = colors), size = 2) +
  geom_text(aes(label = names), nudge_x = 0.2, nudge_y = 0.2) +
  scale_color_manual(values = points$colors)

