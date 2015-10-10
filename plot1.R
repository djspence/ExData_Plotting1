
filterByDateRange <- function(dframe,date1,date2)
{
    datefilter <- (ds$Date >= as.Date(date1) & ds$Date <= as.Date(date2))
    dframe <- dframe[datefilter,]
    dframe
}

ds <- read.csv("household_power_consumption.txt",colClasses = "character", 
               sep = ";", na.strings = c("?"))
ds$Date <- as.Date(ds$Date,format="%d/%m/%Y")
ds <- filterByDateRange(ds,"2007-02-01", "2007-02-02")

ds$Global_active_power <- as.numeric(ds$Global_active_power)

png("plot1.png", width = 480, height = 480)
hist(ds$Global_active_power,col="red", xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()


