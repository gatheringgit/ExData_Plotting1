plot1 <- function(){
        library(lubridate); library(dplyr)
        ## read in the first number of rows up to and including midnight Feb 2, 2007
        edaDataset <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", quote = "", dec = ".", numerals = "allow.loss", na.strings = "?", colClasses = c(rep("character", 2), rep("numeric", 7)), nrows = 69517)
        ## subset the rows from Feb 1st 2007 thru midnight Feb 2nd 2007
        edaDataset1 <- edaDataset[66637:69517, ]
        ## combine the data in the Date and Time columns to permit changing the format and class to a datatime
        datestimes <- paste(c(edaDataset1$Date), c(edaDataset1$Time))
        my_datestimes <- dmy_hms(datestimes)
        ## create and add new column to the dataset with the datetime info
        edaDataset1 <- mutate(edaDataset1, calendar_day_by_minute = my_datestimes)
        ## elminate the Date and Time character class columns by selecting columns 3 thru 10
        edaDataset_final <- select(edaDataset1, 3:10)
        ## create plot1, and then copy to a png
                hist(edaDataset_final$Global_active_power, col = "Red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
                dev.copy(png, file = "plot1.png")
                dev.off()
        
}