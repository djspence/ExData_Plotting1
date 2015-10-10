
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
ds$Time <- as.POSIXct(paste(as.character(ds$Date),ds$Time))

r <- as.POSIXct(round(range(ds$Time), "days"))

png("plot2.png", width = 480, height = 480)
plot(ds$Global_active_power~ds$Time, type="l", xaxt="n", xlab="", 
     ylab = "Global Active Power (kilowatts)")
axis.POSIXct(1, at = seq(r[1],r[2],by="day"))
dev.off()


