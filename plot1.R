## function to generate plot1
## --------------------------
plot1 <- function(file = "household_power_consumption.txt",
                  directory = "C:/LocalData/__SUE/MOUNTS/64GB_SD_Card/D/_Data_Management_Strategical/Data_Science/_COURSERA_STUFF/Course_4_Exploratory_Data_Analysis/Project_1/Data",
                  startDate = "1/2/2007",
                  endDate = "2/2/2007",
                  summarize = FALSE) {
  
  ## call generalized function to read the relevant part of the data
  plotData <- getData(file, directory, startDate, endDate, summarize)
  
  ## open device to create PNG file
  png("plot1.png", width = 480, height = 480)
  
  ## create histogram with requested parameters
  hist(plotData$Global_active_power, col = "Red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  
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
  input.data <- read.table(paste(directory,file, sep = "/", collapse = NULL), header = TRUE, sep=";", quote="\"", na.strings="?")
  
  ## eliminate obsolete data - reduce to requested dates
  input.data <- subset(input.data, as.Date(Date,"%d/%m/%Y") >= as.Date(startDate, "%d/%m/%Y") & as.Date(Date,"%d/%m/%Y") <= as.Date(endDate, "%d/%m/%Y"), select = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  
  if (summarize == TRUE) {
    print(summary(input.data))
  }
  
  return (input.data)
}