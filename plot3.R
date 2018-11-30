# This function takes a data set as input and returns a png file in which is shown
# the trend of the "Sub_metering_x" variables, where x=1,2,3 over 2-days period determined by 
# the "Date" variable

## Plot3: submetering measurements over the 2-day period
plot3<-function(data){
   # creates labels for the date time with week days
   ww<-wday(data$Date,label = TRUE);
   labelIndices<-table(ww)   # Sun  Mon  Tue  Wed  Thu  Fri  Sat 
                             #   0    0    0    0 1440 1440    0 
   aux<-labelIndices!=0;labelIndices[aux];
   labelPosition<-rbind(1,labelIndices[aux][[1]]+1,sum(labelIndices)-1);
   aux2<-which(labelIndices!=0);
   aux3<-aux2[length(aux2)]+1
   aux4<-c(aux2,aux3);aux4
   labelLabels<-levels(ww)[aux4]
   
 
   png(filename = "plot3.png",width = 480, height = 480)
   
   plot(1,type="n",xlab=NA,ylab="Energy sub metering",xaxt="n",xlim = c(0,sum(labelIndices)),ylim=c(0,max(data$Sub_metering_1)))
   with(data,lines(Sub_metering_1,type="l",col="black"))
   with(data,lines(Sub_metering_2,type="l",col="red"))
   with(data,lines(Sub_metering_3,type="l",col="blue"))
   axis(side=1, at=labelPosition,labels =labelLabels)
   legend("topright",col=c("black","red","blue"),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd = 1)
   
   dev.off()
}