# Plot 4
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

df <- tbl_df(df)
df <- df[60000:70000,] # Make the dataframe smaller 

df <- mutate(df, Date = paste(as.character(Date), as.character(Time)))
#df <- mutate(df, Date = paste(Date, Time))

df$Date = dmy_hms(select(df, Date)[[1]])

# Records from first two days of February 2007 are kept
df <- df %>% 
  filter(year(Date)=="2007") %>% 
  filter(month(Date)== "2") %>% 
  filter(day(Date)=="1" | day(Date)=="2" ) #%>%

my_plot4 <- function(df) {
  #par(mfrow = c(2, 2))
  df2 <- select(df, Date, Global_active_power)
  df2["Global_active_power"] <- as.numeric(as.character(df$Global_active_power))
  df2["Voltage"] <- as.numeric(as.character(df$Voltage))
  df2["Sub_metering_1"] <- as.numeric(as.character(df$Sub_metering_1))
  df2["Sub_metering_2"] <- as.numeric(as.character(df$Sub_metering_2))
  df2["Sub_metering_3"] <- as.numeric(as.character(df$Sub_metering_3))
  df2["Global_reactive_power"] <- as.numeric(as.character(df$Global_reactive_power))
  
  #windows()
  par(mfrow = c(2, 2))
  # 1
  with(df2,plot(Date, Global_active_power,  col = "black", type="l", xlab="",
                ylab="Global Active Power"))
  # 2
  with(df2,plot(Date, Voltage,  col = "black", type="l", xlab="datetime",
                ylab="Voltage"))
  
  # 3
  with(df2,plot(Date, Sub_metering_1,  col = "black", type="n", xlab="",
                ylab="Energy sub metering"))
  
  with(df2,lines(x = Date, y = Sub_metering_1, type="l", col="black", lwd=1))
  with(df2,lines(x = Date, y = Sub_metering_2, type="l", col="red", lwd=1))
  with(df2,lines(x = Date, y = Sub_metering_3, type="l", col="blue", lwd=1))
  #   legend("topright", bty = "n", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
  #          col=c("black","red","blue"), pch=21, lty=1, 
  #          pt.cex=0.5, cex=0.65, trace=TRUE, text.font=2, horiz=TRUE )
  legend(x="topright", lwd=1,lty=1, col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),cex=0.55)
  
  # 4
  with(df2,plot(Date, Global_reactive_power,  col = "black", type="l", xlab="datetime",
                ylab="Global_reactive_power"))
  
  
  
}

png(filename = "plot4.png")
my_plot4(df)
dev.off()

