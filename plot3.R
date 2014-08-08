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
xLabel <- ""
yLabel <- "Energy sub meetering"

#reset plot
plot.new()
frame()
par(mfrow = c(1,1), mar = c(4, 4, 4, 4))

#plot to window
yrange<-range(c(plotData$Sub_metering_1,plotData$Sub_metering_2,srcData$Sub_metering_3))
plot(plotData$DateTime,plotData$Sub_metering_1,type="l",ylim=yrange,xlab = xLabel, ylab = yLabel, col=1)
lines(plotData$DateTime,plotData$Sub_metering_2,type="l", col=2)
lines(plotData$DateTime,plotData$Sub_metering_3,type="l", col=3)
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd=2, col=c(1, 2, 3))


#save to png
dev.copy(png, file = "./plot3.png", width = 480, height = 480 ) 
dev.off() 