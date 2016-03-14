install.packages("RCurl")
library(RCurl)
url <- "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip"
destFile <- "household_power_consumption.zip"
download.file(url, destFile)
dataFileFrame <- unzip(destFile,  list = TRUE) #household_power_consumption.txt
dataFileName <- dataFileFrame[1, "Name"]
unzip(destFile, dataFileName)

install.packages("data.table")
library(data.table)
# dataFileName <- household_power_consumption.txt
DT <- fread(dataFileName, ";", na.strings = "?", header = TRUE)
DT$Date <- as.Date(DT$ Date, "%d/%m/%Y")
DT$Global_active_power <- as.numeric(DT$Global_active_power)

dateRange <- as.Date(c("2007-02-01", "2007-02-02"))
DT <- DT[DT$ Date >= dateRange[1] & Date <= dateRange[2]]
width <- 480
height <- 480

png("plot1.png", width, height)
hist(DT$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col="red")
dev.off()

dateTime <- strptime(paste(DT$Date, DT$Time, sep=" "), "%Y-%m-%d %H:%M:%S")
png("plot2.png", width, height)
plot(dateTime, DT$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power(kilowatts)")
dev.off()

png("plot3.png", width, height)
with(DT, {
    plot(dateTime, DT$Sub_metering_1, type="l", xlab = "", ylab="Energy sub metering")
    lines(dateTime, DT$Sub_metering_2,col='Red')
    lines(dateTime, DT$Sub_metering_3,col='Blue')
})
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 2)
dev.off()

png("plot4.png", width, height)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
with(DT,{
    plot(dateTime, DT$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
    plot(dateTime, DT$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
    plot(dateTime, DT$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
    lines(dateTime, DT$Sub_metering_2,col = 'Red')
    lines(dateTime, DT$Sub_metering_3,col = 'Blue')
    legend("topright", col=c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 2)
    plot(dateTime, DT$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Energy sub metering")
})
dev.off()