library(data.table)

# Download the dataset
#url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
#path <- getwd()
#download.file(url, file.path(path, "household_power_consumption.zip"))
#unzip(zipfile = "household_power_consumption.zip")

rm(list=ls()) 

# Read dataset
powerDT <- fread(input = "household_power_consumption.txt", na.strings="?")

# Convert Global active power to numeric
powerDT$Global_active_power <- as.numeric(powerDT$Global_active_power)

# Making a POSIXct date capable of being filtered and graphed by time of day
powerDT[, dateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]

# Filter Dates for 2007-02-01 and 2007-02-02
powerDT <- powerDT[(dateTime >= "2007-02-01") & (dateTime < "2007-02-03")]

png("plot3.png", width=480, height=480)

# Plot 3
plot(powerDT$dateTime, powerDT$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
lines(powerDT$dateTime, powerDT$Sub_metering_2, col="red")
lines(powerDT$dateTime, powerDT$Sub_metering_3, col="blue")
legend("topright",
       col=c("black","red","blue"),
       c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),
       lty=c(1,1))

dev.off()

