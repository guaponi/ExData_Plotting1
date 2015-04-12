# Plot 2
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

my_lineplot1 <- function(df){
  par(mfrow = c(1,1))
  df2 <- select(df, Date, Global_active_power)
  df2["Global_active_power"] <- as.numeric(as.character(df$Global_active_power))
  with(df2,plot(Date, Global_active_power,  col = "black", type="l", xlab="",
                ylab="Global Active Power (kilowatts)"))  
}
png(filename = "plot2.png")
my_lineplot1(df)
dev.off()


