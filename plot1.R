# plot1
library(dplyr)
library(lubridate)

if(!file.exists("household_power_consumption.zip")) {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  dest <- "household_power_consumption.zip"
  download.file(url = url, 
                dest= dest)
}


df <- read.csv2(
  unz("household_power_consumption.zip", 
      "household_power_consumption.txt"))

temp <- df
df <- tbl_df(df)
df <- df[60000:70000,] # Make the dataframe smaller 

df <- mutate(df, Date = paste(as.character(Date), as.character(Time)))
df$Date = dmy_hms(select(df, Date)[[1]])

# Records from first two days of February 2007 are kept
df <- df %>% 
  filter(year(Date)=="2007") %>% 
  filter(month(Date)== "2") %>% 
  filter(day(Date)=="1" | day(Date)=="2" ) #%>%
#print


my_hist1 <- function(df){
  df2 <- transmute(df, Global_active_power = as.numeric(as.character(Global_active_power)))
  
  with(df2,hist(Global_active_power, col="red",  breaks = 20, axes = FALSE,
                xlab="Global Active Power (kilowatts)",
                xlim=c(0,8), main="Global Active Power"))
  x <- c(0, 2, 4, 6)
  axis(1, at=x,labels=x, col.axis="black", las=1)
  y <- seq(0, 1200,by = 200)
  axis(2, at=y,labels=y, col.axis="black", las=1) 
}

png(filename = "plot1.png")
my_hist1(df)
dev.off()





