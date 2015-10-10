
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

ds$Sub_metering_1 <- as.numeric(ds$Sub_metering_1)
ds$Sub_metering_2 <- as.numeric(ds$Sub_metering_2)
ds$Sub_metering_3 <- as.numeric(ds$Sub_metering_3)
ds$Time <- as.POSIXct(paste(as.character(ds$Date),ds$Time))

r <- as.POSIXct(round(range(ds$Time), "days"))

png("plot3.png", width = 480, height = 480)
with(ds,plot(Time,Sub_metering_1, type="n", xaxt="n", xlab="", 
     ylab = "Energy sub metering"))
with(ds, lines(Time,Sub_metering_1))
with(ds, lines(Time,Sub_metering_2,col="red"))
with(ds, lines(Time,Sub_metering_3,col="blue"))
axis.POSIXct(1, at = seq(r[1],r[2],by="day"))
legend("topright", col=c("black","blue","red"), legend=names(ds[7:9]), lty = c(1,1,1))
dev.off()


