plot4 <- function(){
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
                ## set-up multi graphics in one view for plot4
                par(mfrow = c(2, 2), mar = c(4, 4, 4, 2), oma = c(2, 2, 0, 2))
                        with(edaDataset_final, {
                                plot(calendar_day_by_minute, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
                                plot(calendar_day_by_minute, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
                                ##plot3()
                                plot.default(calendar_day_by_minute, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type="n")
                                        lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_1, type = "l", col = "black")
                                        lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_2, type = "l", col = "red")     
                                        lines(edaDataset_final$calendar_day_by_minute, edaDataset_final$Sub_metering_3, type = "l", col = "blue")
                                        legend("topright", inset = c(.05, 0), lty = 1,  ncol = 1.25, cex = 0.6, x.intersp = 1, y.intersp = 0.5, col = c("black", "red", "blue"), bty = "n", adj = 0, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                                plot(calendar_day_by_minute, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
                        })
                                dev.copy(png, file = "plot4.png")
                                dev.off()
}
