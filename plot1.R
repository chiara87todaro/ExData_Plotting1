# This function takes a data set as input and returns a png file in which is shown
# the histogram of the "Global_active_power" variable
plot1<-function(data){
   png(filename = "plot1.png",width = 480, height = 480)
   
   with(data,hist(Global_active_power,col = "red",main="Global Active Power",xlab = "Global Active Power (kilowatts)"))
   
   dev.off()
}