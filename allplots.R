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

#plot1 - Global Active Power histro
doPlot1 <- function(plotData){
  
  #plot params
  plotTitle <- "Global Active Power"
  xLabel <- "Global Active Power (kilowatts)"
  yLabel <- "Frequency"
  
  #plot to window
  hist(plotData$Global_active_power, main=plotTitle, col="red", xlab = xLabel, ylab = yLabel)
  
}

#plot2 - Global Active Power over DateTime
doPlot2 <- function(plotData){
  
  #plot params
  xLabel <- ""
  yLabel <- "Global Active Power (kilowatts)"
  
  #plot to window
  plot(plotData$DateTime,plotData$Global_active_power,type="l",xlab = xLabel, ylab = yLabel)
  
}


#plot3 - Meter values over DateTime
doPlot3 <- function(plotData){
  #plot params
  xLabel <- ""
  yLabel <- "Energy sub meetering"
  
  #plot to window
  yrange<-range(c(plotData$Sub_metering_1,plotData$Sub_metering_2,srcData$Sub_metering_3))
  plot(plotData$DateTime,plotData$Sub_metering_1,type="l",ylim=yrange,xlab = xLabel, ylab = yLabel, col=1)
  lines(plotData$DateTime,plotData$Sub_metering_2,type="l", col=2)
  lines(plotData$DateTime,plotData$Sub_metering_3,type="l", col=3)
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd=2, col=c(1, 2, 3))
  
}

#plot4 - Voltage over DateTime
doPlot4 <- function(plotData){
  #plot params
  xLabel <- "datetime"
  yLabel <- "Voltage"
  
  #plot to window
  plot(plotData$DateTime,plotData$Voltage,type="l",xlab = xLabel, ylab = yLabel)
  
}

#plot5 - Global Reactive Power over DateTime
doPlot5 <- function(plotData){
  #plot params
  xLabel <- "datetime"
  yLabel <- "Global_reactive_power"
  
  #plot to window
  plot(plotData$DateTime,plotData$Global_reactive_power,type="l",xlab = xLabel, ylab = yLabel)
  
}
