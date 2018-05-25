#### UBER ASSIGNMENT ####
#Loading the required packages
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)


#Getting uber request data into R
Uber.Request<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

#Observing the structure of dataframe Uber.Requests                         
str(Uber.Request)
#or you can also have a glimpse of data frame using Uber.Requests
glimpse(Uber.Request)

####Handling data quality issues/Data Cleaning####

#checking for any duplicate rows
cat ('there are',(nrow(Uber.Request)-nrow(Uber.Request %>% unique)),'duplicate rows')
#there are 0 duplicate rows


# checking for any NA values in all columns, one after other

# checking for any NA values in column Request.id
anyNA(Uber.Request$Request.id)          # no NA's
# checking for any NA values in column 
anyNA(Uber.Request$Driver.id)           # NA's present(2650)
# checking for any NA values in column 
anyNA(Uber.Request$Pickup.point)        # no NA's
# checking for any NA values in column 
anyNA(Uber.Request$Status)              # no NA's
# checking for any NA values in column 
anyNA(Uber.Request$Request.timestamp)   # no NA's
# checking for any NA values in column 
anyNA(Uber.Request$Drop.timestamp)      # NA's present(3914)

# It looks appropriate to leave the NA's untouched as they 
# look valid in the columns Driver.id and Drop.timestamp the NA's
# in the respective columns are present when there is a value
# "no cars available" in column status

#checking for any spelling mistakes in categorical columns

#In column Pickup.point

unique(Uber.Request$Pickup.point)

# there are only two unique values "Airport" "City"  in column Pickup.point.There are no spelling mistakes

#In column Status
unique(Uber.Request$Status)

#there are only three  unique values Trip Completed,Cancelled,No Cars Available in column Status.There are no spelling mistakes

###########DATA PREPERATION####################
####Handling date and time columns Request.timestamp and Drop.timestamp which are read as type character####

##Request.timestamp##

#Parsing Request.timestamp and storing it in the column Request_Date

Uber.Request$Request_Date<- lubridate::parse_date_time(Uber.Request$Request.timestamp,orders = c("d/m/Y H:M","d-m-Y H-M-S"))

# checking if there are any NA's coerced because of invalid data values
(Uber.Request$Request.timestamp %>% is.na %>% sum) == (Uber.Request$Request_Date %>% is.na %>% sum)
#It gives TRUE means  NA's are not coerced.This also means there are no invalid data values in Request.timestamp

#spliting date from Request_Date and storing it in column Request.Date
Uber.Request$Request.Date<- as.Date(Uber.Request$Request_Date)

#Extracting Day of the week from Request.Date and storing it in Request.Day column
Uber.Request$Request.Day<- weekdays(Uber.Request$Request_Date)


#spliting date form Request_Date and storing it in column Request.Time
Uber.Request$Request.Time<-format(Uber.Request$Request_Date,"%H:%M:%S")

#Extracting hours mins and sec from column Request.timestamp
Uber.Request$Request.hour<-lubridate::hour(Uber.Request$Request_Date)
Uber.Request$Request.minute<-lubridate::minute(Uber.Request$Request_Date)
Uber.Request$Request.second<-lubridate::second(Uber.Request$Request_Date)

#grouping the data into different time slots morning,noon,evening and night 
#based on values in request.hour

#defineing a timeslot function
timeslot<-function(request.hour){
  if(request.hour>=4 & request.hour<10){
    return("Morning Slot")
  }else if(request.hour>=10 & request.hour<16){
    return("Noon Slot")
  }else if(request.hour>=16 & request.hour<22){
    return("Evening Slot")
  }else if(request.hour>=22 | request.hour<4){
    return("Night Slot")
  }else{
    return(NA)
  }
}
#creating timeslot column using timeslot function
Uber.Request$Request.TimeSlot<-sapply(Uber.Request$Request.hour,timeslot) %>% unlist
#checking for any coerced NA values in column Request.TimeSlot
anyNA(Uber.Request$Request.TimeSlot)#FALSE no NA's


#Dropping request_date column
Uber.Request$Request.timestamp<-NULL

#####Drop.timestamp#####

#Parsing Drop.timestamp and storing it in the column Drop_Date

Uber.Request$Drop_Date<- parse_date_time(Uber.Request$Drop.timestamp,orders = c("d/m/Y H:M","d-m-Y H-M-S"))

# checking if there are any NA's coerced because of invalid data values
(Uber.Request$Drop.timestamp %>% is.na %>% sum) == (Uber.Request$Drop_Date%>% is.na %>% sum)
#It gives TRUE means  NA's are not coerced. This also means there are no invalid data values in Drop.timestamp

#spliting date form Drop_Date and storing it in column Drop.Date
Uber.Request$Drop.Date<- as.Date(Uber.Request$Drop_Date)

#spliting date form Drop_Date and storing it in column Drop.Time
Uber.Request$Drop.Time<-format(Uber.Request$Drop_Date,"%T")

#Extracting hours mins and sec from column Drop.timestamp
Uber.Request$Drop.hour<-lubridate::hour(Uber.Request$Drop_Date)
Uber.Request$Drop.minute<-lubridate::minute(Uber.Request$Drop_Date)
Uber.Request$Drop.second<-lubridate::second(Uber.Request$Drop_Date)

#Dropping request_date column
Uber.Request$Drop.timestamp<-NULL


#checking for the time duration of data present
data.interval<-as.interval(min(Uber.Request$Request_Date),max(Uber.Request$Request_Date))

#checking the duration of the interval
as.duration(data.interval)
#Total 5 Days of data is present

######DATA ANALYSIS#####

# Since Request.id and Driver.id are id or unique values there isn't much sense analysing them

#Defining the  Pickup.point  variable as factor and ordering it's levels in a particuler order
Uber.Request$Pickup.point<-factor(Uber.Request$Pickup.point,levels=c("City","Airport"))

#Analysing variable Pickup.point by plotting a bar chart on it (and looking for some insights)
ggplot(Uber.Request,aes(Pickup.point,fill=Pickup.point))+geom_bar(col="black")+annotate("text",x=c("Airport","City"),y=c(3450,3700),label=c("48%","51%"))+theme_bw()
#The above plot shows that ,there isn't much difference between Airport and city pickup requests


#Analysing variable Status by plotting a bar chart on it (and looking for some insights)
ggplot(Uber.Request,aes(Status,fill=Status))+geom_bar(col="black")+annotate("text",x=c("Cancelled","No Cars Available","Trip Completed"),y=c(1400,2800,2950),label=c("19%","39%","41%"))+theme_bw()
#The above plot clearly depicts that only 41% of the requests from city and 
#airport gets completed and the remaining 59% trips either get cancelled or 
#there is  no car availability
##########################################################################
#The proportions of above plot can be obtained from the following code
prop.table(table(Uber.Request$Pickup.point))
prop.table(table(Uber.Request$Status))
##########################################################################

#segemnting pickup.point over status
ggplot(Uber.Request,aes(x=Pickup.point,fill=Status))+geom_bar(position = "dodge",col="black")+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+theme_bw()
#The above plot shows that for most of the  Airport pickup requests there are 
#no cars available and most requests that get cancelled are city pickup requests

#Analysing variable Request.hour by plotting a bar chart on it (and looking for
#some insights)
ggplot(Uber.Request,aes(Request.hour))+geom_bar(fill="royalblue1",col="black")+annotate("text",x=c(7.2,19),y=c(500,550),label=("High Request Rate"))+theme_bw()
#The above plot clearly depicts that there are high request rates from 5AM TO 10AM
#and 5pm to 10 pm

#To get a better understanding of requests raised at different hours of the day
#lets plot a chart on variable timeslot(which contains hours grouped into 
#different timeslots)

#Defining the  timeslot  variable as factor and ordering it's levels in a particuler order
Uber.Request$Request.TimeSlot<-factor(Uber.Request$Request.TimeSlot,levels=c("Morning Slot","Noon Slot","Evening Slot","Night Slot"))

#plotting a bargraph on time slots
ggplot(Uber.Request,aes(x=Request.TimeSlot,fill=Request.TimeSlot))+xlab("TimeSlot")+geom_bar(col="black")+annotate("text",x=c("Evening Slot","Morning Slot","Night Slot","Noon Slot"),y=c(2590,2400,975,1150),label=c("37%","34%","13%","16%"))+theme_bw()

#the proportions can be obtained from the following code
prop.table(table(Uber.Request$Request.TimeSlot))
#From the above plot it is clear that most of the requests are raised in 
#morning(34%) and evening(37%) slots

#segmenting the timeslot variable by pickup point may give some more information
ggplot(Uber.Request,aes(x=Request.TimeSlot,fill=Pickup.point))+xlab("Timeslot")+geom_bar(position = "dodge",col="black")+ geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+theme_bw()
#During morning slot city pickup requests are high 
#and during evening slot airport pickup requests are high


#segmenting the timeslot variable by Status may give some information
ggplot(Uber.Request,aes(x=Request.TimeSlot,fill=Status))+xlab("Timeslot")+geom_bar(position = "dodge",col="black")+ geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+theme_bw()

#segmenting the timeslot variable by both Status and pickup point may give some more information
ggplot(Uber.Request,aes(x=Request.TimeSlot,fill=Status))+xlab("Timeslot")+geom_bar(col="black")+theme_bw()+facet_grid(Pickup.point~Status)+stat_count(aes(label = ..count..), geom = "text",vjust=-0.5,size=2)+theme(axis.text.x = element_text(face="bold", color = "#993333",size = 10,angle=90))+theme_bw()
#From the above plot it is clear that most city requests get cancelled in the 
#morningslot(during which there is high city request rate) and for most of the
#airport requests during the evening slot(during which there is high airport request rate) 
#there are no cars available

#From the above plots it can be assumed that although there is not much difference  city requests and airport requests ,
#city requests are problametic requests because most of city requests gets cancelled by drivers.
#Most Uber drivers are not ready to drive to airport because they may have to wait long before they get a trip back to city
#This is the same reason for most airport requests cars are not available

###############SUPPLY AND DEMAND GAP CALCULATION#######################
demandd<-function(status){
  if (status=="Cancelled"|status=="No Cars Available"|status=="Trip Completed"){
    return("demand")
  }else{
    return(NA)
  }
}


supply<-function(status){
  if (status=="Cancelled"|status=="No Cars Available"){
    return( "gap")
  }else{
    return("supply")
  }
}


#creating supply column using supply function
Uber.Request$supply<-sapply(Uber.Request$Status,supply) %>% unlist

#checking for NA values in Uber.Request$supply
anyNA(Uber.Request$supply)  #No NA's

#creating demand column using demand function
Uber.Request$demand<-sapply(Uber.Request$Status,demandd) %>% unlist
#checking for NA values in Uber.Request$demand
anyNA(Uber.Request$demand)  #No NA's

#Finding supply and demand gap
demand<-sum
addmargins(table(Uber.Request$supply),FUN = demand)

#so overall gap is 2831,it would be  better if we look at this values in propotions
addmargins(prop.table(table(Uber.Request$supply)),FUN = demand)
# gap is 58% of demand

#Demand And Supply plot
ggplot(Uber.Request,aes(x=supply,fill=supply))+geom_bar(col="black")+geom_bar(aes(x=demand),col="black",fill="royalblue2")+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+theme_bw()+annotate("text",x="demand",y=6970,label="6745")


#Finding supply and demand gap for each time slot seperately

#morning slot
addmargins(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Morning Slot"]),FUN = demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Morning Slot"])),FUN=demand)
#For morning slot the gap is about 59%

#noon slot
addmargins(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Noon Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Noon Slot"])),FUN=demand)
#For noon slot the gap is about 40%

#evening slot
addmargins(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Evening Slot"]),FUN=demand)
#proportion
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Evening Slot"])),FUN=demand)
#For evening slot the gap is about 65%

#night slot
addmargins(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Night Slot"]),FUN=demand)
#proportion
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Request.TimeSlot=="Night Slot"])),FUN = demand)
#For night slot the gap is about 54%

#From the above calculations the gap for evening slot(4pm to 10pm) is high(65%)

#Demand And Supply plot for each time slot
ggplot(Uber.Request,aes(x=supply,fill=supply))+geom_bar(col="black")+ theme_bw()+facet_wrap(~Request.TimeSlot)+geom_text(stat='count',aes(label=..count..),vjust=1,position = position_dodge(width = 1))

#or 
ggplot(Uber.Request,aes(x=supply,fill=supply))+theme_bw()+geom_bar(col="black")+geom_bar(aes(x=demand),col="black",fill="royalblue2")+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+facet_grid(~Uber.Request$Request.TimeSlot)
#From the above graph the gap for evening slot(4pm to 10pm) is high


#Finding Gap for each pickup point seperately

#City
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"])),FUN=demand)
#gap for city pickup requests is 57% 

#city pickup requests demand and supply gap on diffrent time slots

#morning slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Morning Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Morning Slot"])),FUN=demand)
#gap for city pickup requests on morning slots is 71%

#noon slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Noon Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Noon Slot"])),FUN=demand)
#gap for city pickup requests on noon slots is 47 %

#evening slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Evening Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Evening Slot"])),FUN=demand)
#gap for city pickup requests on evening slots is 27%

#night slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Night Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="City"& Uber.Request$Request.TimeSlot=="Night Slot"])),FUN=demand)
#gap for city pickup requests on night slots is 55%

#From the above calculations the gap for city pickup requests is high on morning slots i.e 71%


#Airport
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"])),FUN=demand)
#gap for airport pickup requests is 59%

#Airport pickup requests demand and supply gap on diffrent time slots
#morning slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Morning Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Morning Slot"])),FUN=demand)
#gap for airport pickup requests  on morning slots is 16%

#noon slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Noon Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Noon Slot"])),FUN=demand)
#gap for airport pickup requests  on noon slots is 30%

#evening slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Evening Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Evening Slot"])),FUN=demand)
#gap for airport pickup requests  on evening slots is 77%

#night slot
addmargins(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Night Slot"]),FUN=demand)
#proportions
addmargins(prop.table(table(Uber.Request$supply[Uber.Request$Pickup.point=="Airport"& Uber.Request$Request.TimeSlot=="Night Slot"])),FUN=demand)
#gap for airport pickup requests  on night slots is 54%

#From the above calculations the gap for Airport pickup requests is high on evening slots i.e 77%

#If we consider all time slots the gap for airport pickup request is high  which is 59% than city pickup requests 54%

#Plot showing demand and supply gap for different pickup requests on various timeslots
ggplot(Uber.Request,aes(x=supply,fill=supply))+geom_bar(col="black")+theme_bw()+geom_bar(aes(x=demand),col="black",fill="royalblue3")+facet_grid(Pickup.point~Request.TimeSlot)+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))



#In my opinion the reason for the supply demand gap is mainly  due to drivers not being ready to take airport trips from city pickups
#because of this there is a huge gap of demand and supply in the morning slot for city pickup requests.As drivers are not ready 
#to take airport trips from city there will be shortage of vehicles at airport.Hence customers who want to book uber will get no cars available as indication
#so there is a high gap for airport pickup requests in the evening slot.


#Possible suggestions to fill the supply demand gap
#1) Increasing trip rates for airport pickups and drops.Which may make drivers interested to take up the trips with out cancelling .
#2)Making drivers work on shift basis,where one shift starts in the morning at city and the other start at the airport during evening and so on 
#3)A fixed number of cars should be specially assigned for airport trips and they should accept only airport pickups and drops
#4)Keeping drivers updated with the flight schedule details on a reguler basis,will also help them plan their work productively



