# This function takes a data set as input and returns a png file in which is shown
# the trend of the "Global_active_power" variable over 2-days period determined by 
# the "Date" variable

plot2<-function(data){
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
   
   
   png(filename = "plot2.png",width = 480, height = 480)
   
   with(data,plot(Global_active_power,type="l",xlab=NA,ylab="Global Active Power (kilowatts)",xaxt="n")) 
   axis(side=1, at=labelPosition,labels =labelLabels)
   
   dev.off()
}
