plot3 <- function(){
        library(lubridate); library(dplyr)
        ## read in the first number of rows up to and including midnight Feb 2, 2007
        edaDataset <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", quote = "", dec = ".", numerals = "allow.loss", na.strings = "?", colClasses = c(rep("character", 2), rep("numeric", 7)), nrows = 69517)
                ## subset the rows from Feb 1st 2007 thru midnight Feb 2nd 2007
                edaDataset1 <- edaDataset[66637:69517, ]
                        ## combine the data in the Date and Time columns to permit changing the format and class to a datetime
                        datestimes <- paste(c(edaDataset1$Date), c(edaDataset1$Time))
                        my_datestimes <- dmy_hms(datestimes)
                        ## create and add new column to the dataset with the datetime info
                        edaDataset1 <- mutate(edaDataset1, calendar_day_by_minute = my_datestimes)
                                ## elminate the Date and Time character class columns by selecting columns 3 thru 10
                                edaDataset_final <- select(edaDataset1, 3:10)
        ## set up plot3 then add data in following steps with lines command
        with(edaDataset_final, plot.default(calendar_day_by_minute, Sub_metering_1, xlab = "", ylab = "Energy sub metering", mar = c(3, 5, 2, 2), oma = c(0, 2, 0, 0), type="n"))
                lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_1, type = "l", col = "black")
                lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_2, type = "l", col = "red")     
                lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_3, type = "l", col = "blue")
                        ## add legend, then copy to png
                        legend("topright", inset = c(.002, .002), lty = 1, ncol = 1.25, cex = 0.75, xjust = 1, yjust = 0, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                        dev.copy(png, file = "plot3.png")
                        dev.off()
}