readFile <- function(fileName = 'household_power_consumption.txt'){
  
  grepCmd <- paste("grep '^[1,2]/2/2007' ", fileName)
  
  #read subset via pipe
  dataSubset <- read.csv(pipe(grepCmd), sep = ";", na.strings = "?")
  
  #grep returns 1/2/2007, 11/2/2007, 21/2/2007,... - reduce
  dataSubset <- subset(dataSubset, dataSubset[,1] == '1/2/2007' | dataSubset[,1] == '2/2/2007')
  
  #names
  names(dataSubset) <- c("Date","Time","Global_active_power","Global_reactive_power",
                         "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
  #convert timestamp
  dataSubset$DateTime <- strptime(paste(dataSubset[, c(1)], dataSubset[, c(2)]), "%d/%m/%Y %H:%M:%S", tz = "")
  
  #result
  dataSubset
}

#data
fileName <-"./household_power_consumption.txt"
plotData <-readFile()

#plot params
plotTitle <- "Global Active Power"
xLabel <- "Global Active Power (kilowatts)"
yLabel <- "Frequency"

#reset plot
plot.new()
frame()
par(mfrow = c(1,1), mar = c(4, 4, 4, 4))

#plot to window
hist(plotData$Global_active_power, main=plotTitle, col="red", xlab = xLabel, ylab = yLabel)


#save to png
dev.copy(png, file = "./plot1.png", width = 480, height = 480 ) 
dev.off() 