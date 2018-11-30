# This function takes a data set as input and returns a png file in which is shown
# the trend of the following variables over 2-days period determined by 
# the "Date" variable, each in a different panel:
# "Global_active_power" variable
# "Voltage" variable
# "Sub_metering_x" variables, where x=1,2,3 
# "Global_reactive_power" variable

plot4<-function(data){
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
   
  
   png(filename = "plot4.png",width = 480, height = 480)
   
   
   par(mfrow=c(2,2),mar=c(4,4,1,1))
   
   with(data,plot(Global_active_power,type="l",xlab=NA,ylab="Global Active Power",xaxt="n")) 
   axis(side=1, at=labelPosition,labels=labelLabels)
   
   with(data,plot(Voltage,type="l",xlab="datetime",ylab="Voltage",xaxt="n")) 
   axis(side=1, at=labelPosition,labels=labelLabels)
   
   plot(1,type="n",xlab=NA,ylab="Energy sub metering",xaxt="n",xlim = c(0,sum(labelIndices)),ylim=c(0,max(dataFeb$Sub_metering_1)))
   with(data,lines(Sub_metering_1,type="l",col="black"))
   with(data,lines(Sub_metering_2,type="l",col="red"))
   with(data,lines(Sub_metering_3,type="l",col="blue"))
   axis(side=1, at=labelPosition,labels =c("Thu","Fri","Sat"))
   legend("topright",col=c("black","red","blue"),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd = 1)
   
   with(data,plot(Global_reactive_power,type="l",xlab="datetime",xaxt="n"))
   axis(side=1, at=labelPosition,labels=labelLabels)
   
   dev.off()
}