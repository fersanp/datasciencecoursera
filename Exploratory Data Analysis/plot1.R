library(data.table)

# Download the dataset
#url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
#path <- getwd()
#download.file(url, file.path(path, "household_power_consumption.zip"))
#unzip(zipfile = "household_power_consumption.zip")

rm(list=ls()) 

# Read dataset
powerDT <- fread(input = "household_power_consumption.txt", na.strings="?")

# Select dates 2007-02-01 and 2007-02-02.
powerDT$Date <- as.Date(powerDT$Date, format = "%d/%m/%Y")
powerDT <- powerDT[(Date >= "2007-02-01") & (Date <= "2007-02-02")]

# Create histogram global active power
png("plot1.png", width=480, height=480)

## Plot 1
hist(powerDT[, Global_active_power], main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")

dev.off()
