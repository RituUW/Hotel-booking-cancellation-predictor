HB<-read.csv("HotelBookings.csv",stringsAsFactors = TRUE)
View(HB)
head(HB)
str(HB)
install.packages("ggplot2")
library(ggplot2) # to use ggplot2 from computer into R
#generate scatterplot # data used is mpg data frame
names(HB)
unique(HB[c("children")])

unique(HB[c("babies")])
unique(HB[c("reserved_room_type")])
unique(HB[c("days_in_waiting_list")])


sum(is.na(HB$company))  #[1] 112593
sum(is.na(HB$agent))  #16340


HB$Booking_Year <- format(HB$reservation_status_date, format="%Y")

HB$Booking_Year<-format(as.Date(HB$reservation_status_date),"%Y")

HB$Booking_Year <- format(HB$arrival_date, format="%Y")
View(HB)
ggplot(data=HB)+geom_point(aes(x=displ,y=hwy))
ggplot(data=mpg)+
  geom_point(aes(x=displ,y=hwy,color=class))+
  labs(y="highway miles per gallon")
mower.df<-read.csv("RidingMowers.csv",stringsAsFactors = TRUE)
head(mower.df)
names(mower.df)
ggplot(data=mower.df)+geom_point(aes(x=Income,y=Lot_Size))
ggplot(data=mower.df)+geom_point(aes(x=Income,y=Lot_Size,color=Ownership))
#---bar charts
ggplot(data=HB)+geom_bar(aes(x=is_canceled))

#height of bar corresponds to mean of highway efficiency for each bar
ggplot(data=mpg)+geom_bar(aes(x=class,y=hwy),stat="summary",fun="mean")
#exercise
ggplot(data=mower.df)+geom_bar(aes(x=Ownership,y=Income),stat="summary",fun="mean")


str(HB)
