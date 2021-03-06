## function to generate plot4
## --------------------------
plot4 <- function(file = "household_power_consumption.txt",
                  directory = "C:/LocalData/__SUE/MOUNTS/64GB_SD_Card/D/_Data_Management_Strategical/Data_Science/_COURSERA_STUFF/Course_4_Exploratory_Data_Analysis/Project_1/Data",
                  startDate = "1/2/2007",
                  endDate = "2/2/2007",
                  summarize = FALSE) {
  
  ## call generalized function to read the relevant part of the data
  plotData <- getData(file, directory, startDate, endDate, summarize)
  
  ## need to reformat date and time vectors into one single vector
  date_time <- strptime(paste(plotData$Date, plotData$Time), "%d/%m/%Y %H:%M:%S")
  plotData  <- cbind(date_time, plotData[,-(1:2)])
  
  ## open device to create PNG file
  png("plot4.png", width = 480, height = 480)
  
  # Define the canvas for 2 x 2 plot 
  par(mfcol = c(2,2))
  
  # repeat plot 2 with different xlab
  plot(plotData$Global_active_power ~ plotData$date_time, type = "l", main = "", xlab = "", ylab = "Global Active Power")

  # repeat plot 3 with different legend
  plot(plotData$Sub_metering_1 ~ plotData$date_time, type = "l", main = "", xlab = "", ylab = "Energy sub metering")
  lines(plotData$Sub_metering_2 ~ plotData$date_time, col = "Red")
  lines(plotData$Sub_metering_3 ~ plotData$date_time, col = "Blue")
  legend("topright", col = c("Black", "Red", "Blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, bty ="n")

  # plot voltage along time line
  plot(plotData$Voltage ~ plotData$date_time, type = "l", main = "", xlab = "datetime", ylab = "Voltage")

  # plot reactive power along time line
  plot(plotData$Global_reactive_power ~ plotData$date_time, type = "l", main = "", xlab = "datetime", ylab = "Global_reactive_power")
  
  ## write PNG file
  dev.off()
}


getData <- function(file, directory, startDate, endDate, summarize) {
  ## 'file' is a character vector of length 1 indicating
  ## the name of the input data file
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'startDate' is a character vector indicating the 
  ## start date for the subset of data to be extracted
  
  ## 'endDate' is a character vector indicating the 
  ## end date for the subset of data to be extracted
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## code to read the data
  input.data <- read.table(paste(directory,file, sep = "/", collapse = NULL), header = TRUE, sep=";", na.strings="?")
  
  ## eliminate obsolete data - reduce to requested dates
  input.data <- subset(input.data, as.Date(Date,"%d/%m/%Y") >= as.Date(startDate, "%d/%m/%Y") & as.Date(Date,"%d/%m/%Y") <= as.Date(endDate, "%d/%m/%Y"), select = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity", "Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
  
  if (summarize == TRUE) {
    print(summary(input.data))
  }
  
  return (input.data)
}