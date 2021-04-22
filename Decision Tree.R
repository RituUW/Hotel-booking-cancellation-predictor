HB2<-read.csv("hotel_bookings_Cleansed_3.csv",stringsAsFactors = TRUE)


HB2$Net_Cancellations<-ifelse(HB2$previous_cancellations >=HB2$previous_bookings_not_canceled,1,0)
library("dplyr")
selected.df <- dplyr::select(HB2, -c("country","agent","meal",
                                     "adults", "children", "babies","company",
                                     "arrival_date_year","arrival_date_month","arrival_date_week_number",
                                     "arrival_date_day_of_month",
                                     "previous_bookings_not_canceled",
                                     "reservation_status","reservation_status_date"))

View(selected.df)
set.seed(52)
train.index <- sample(1:nrow(selected.df), nrow(selected.df)*0.7) 
# Build training and validation set by indexing
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

library(rpart)

library(rpart.plot)
#build a tree
default.ct <- rpart(is_canceled ~ ., data = train.df, method = "class")
#plot tree       
prp(default.ct,type=1, extra = 1)
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

#prp(my.ct,  type=1, extra = 1)
library(caret)
# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred, factor(valid.df$is_canceled),positive='1')




#==============================================================================

# Correlation Matrix

  #selected1.df<-lapply(selected.df[,c("hotel","distribution_channel","reserved_room_type","assigned_room_type",
  # "market_segment","deposit_type","customer_type")],as.numeric)
  
  #selected_rest.df<-dplyr::select(selected.df,-c("hotel","distribution_channel","reserved_room_type","assigned_room_type",
  #  "market_segment","deposit_type","customer_type"))
  
  
  
  #selected2.df<-data.frame(selected_rest.df,selected1.df)
  #View(selected3.df)
#x<-selected2.df
#y<-selected2.df$is_canceled
#df<-cor(x,y)

#================================================================================















#View(HB2)
# Creation of New Features
#HB2['stays_in_total_nights'] = HB2['stays_in_week_nights'] + HB2['stays_in_weekend_nights']
#HB2['total_guests'] = HB2['adults'] + HB2['children'] + HB2['babies']
#HB2$Net_Cancellations<-ifelse(HB2$previous_cancellations >=HB2$previous_bookings_not_canceled,1,0)
#View(HB2)
#library("dplyr")
#selected.df <- dplyr::select(HB2, -c("country","agent","meal",
                                   #  "adults", "children", "babies","company",
                                    # "arrival_date_year","arrival_date_month","arrival_date_week_number","arrival_date_day_of_month",
                                    # "previous_bookings_not_canceled",
                                    # "reservation_status","reservation_status_date"))

#set.seed(52)
#train.index <- sample(1:nrow(selected.df), nrow(selected.df)*0.7) 
# Build training and validation set by indexing
#train.df <- selected.df[train.index, ]
#valid.df <- selected.df[-train.index, ]

#library(rpart)

#library(rpart.plot)
#build a tree
#default.ct <- rpart(is_canceled ~ ., data = train.df, method = "class")
#plot tree       
#prp(default.ct,type=1, extra = 1)
#default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

#prp(my.ct,  type=1, extra = 1)
#library(caret)
# generate confusion matrix for validation data
#confusionMatrix(default.ct.point.pred, factor(valid.df$is_canceled),positive='1')
#str(selected.df)

#selected1.df<-lapply(selected.df[,c("hotel","distribution_channel","reserved_room_type","assigned_room_type",
                                   # "market_segment","deposit_type","customer_type")],as.numeric)

#selected_rest.df<-dplyr::select(selected.df,-c("hotel","distribution_channel","reserved_room_type","assigned_room_type",
                                             #  "market_segment","deposit_type","customer_type"))



#selected2.df<-data.frame(selected_rest.df,selected1.df)
#View(selected3.df)
#x<-selected2.df
#y<-selected2.df$is_canceled
#df<-cor(x,y)

#data_frame[order(data_frame$c1),]
#sort(df, decreasing = TRUE, na.last = TRUE)


#View(HB)
#names(HB)
#------
#summary(HB)
#str(HB)
#### Remove rows with sum==0 there are 180 rows#####
#A reservation with no customers seems rather odd, we will remove those rows

#HB2<-HB[HB$adults+HB$children+HB$babies!=0,]
#nrow(HB2)
#119210
#write.csv(HB2,"/Users/ritu/Documents/Foster Quarter 2/data mining and analytics/Final Project/Gold3 Project/hotel_bookings_Cleansed_2.csv")
#levels(HB$reserved_room_type)
#levels(HB$assigned_room_type)


#View(HB2)

# turns out there are bookings with duration of stay being zero, that does not help, so we will drop these rows.
#HB3 <-HB2[HB2$'stays_in_total_nights'> 0,]
# 645 ROWS REMOVED
#write.csv(HB3,"/Users/ritu/Documents/Foster Quarter 2/data mining and analytics/Final Project/Gold3 Project/hotel_bookings_Cleansed_3.csv")
#118565


#nrow(HB3)
#df = df.drop(columns=['adults', 'children', 'babies'])

# Check if room type was changed
#df['room_type_changed'] = (df['reserved_room_type'] != df['assigned_room_type']).astype('int')
####feature engineering###
#HB2$reserved_room_type <- factor(HB2$reserved_room_type, levels=levels(HB2$assigned_room_type))
#HB2$Room<-ifelse(HB2$reserved_room_type==HB2$assigned_room_type,1,0)
#HB2$Net_Cancellations<-ifelse(HB2$previous_cancellations >=HB2$previous_bookings_not_canceled,1,0)
#View(HB2)
#nrow(HB2)
#str(HB2)
#set.seed(52)  # set seed for reproducing the partition
# Random sample indexes
# select variables/predictors randomly
#selected.var <- c(10, 1, 8, 4, 2, 9, 13)

#REMOVE RESERVATION_STATUS COLUMN as we wont hvae this column at the time of prediction column 24
# Remove city column lot of null values 112593 column 18
#dropping agent and city of origin column
#library("dplyr")
#selected.df <- dplyr::select(HB3, -c("X","country","agent","meal",
                                     #"adults", "children", "babies","company",
                                     #"arrival_date_year","arrival_date_month","arrival_date_week_number","arrival_date_day_of_month",
                                     #"previous_bookings_not_canceled",
                                     #"assigned_room_type","reservation_status","reservation_status_date"))
#,"hotel","stays_in_weekend_nights", 
                                     #"stays_in_week_nights","is_repeated_guest","adr" 
#View(selected.df)
#str(selected.df)

#selected_rest.df<-dplyr::select(selected.df,-c("hotel"))

#selected2.df<-lapply(selected.df[,c("hotel")],as.numeric)

#selected3.df<-data.frame(selected_rest.df,selected2.df)
#View(selected3.df)

#View(selected3.df[,c("is_canceled","deposit_type","Room")])


#set.seed(52)
#train.index <- sample(1:nrow(selected.df), nrow(selected.df)*0.7) 
# Build training and validation set by indexing
#train.df <- selected.df[train.index, ]
#valid.df <- selected.df[-train.index, ]
#View(train.df)
# if not installed, run:
#install.packages("rpart")
#library(rpart)
# if not installed, run:
#install.packages("rpart.plot")
#library(rpart.plot)
#build a tree
#default.ct <- rpart(is_canceled ~ ., data = train.df, method = "class")
#plot tree       
#prp(default.ct)
# to add extra details
#prp(default.ct)
# grow full tree
#full.ct <- rpart(is_canceled ~ ., data = train.df, method = "class", 
# control = rpart.control(cp = -1, minsplit = 1)) 
#my.ct <- rpart(is_canceled ~ ., data = train.df, method = "class", 
               #control = rpart.control(maxdepth =  15))
#prp(my.ct,  type=1, extra = 1)
# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
# Otherwise, a probablity of belonging to each class

######################################################################################

library("dplyr")
selected.df <- dplyr::select(HB2, -c("X","country","agent","meal",
                                     "adults", "children", "babies","company",
                                     "arrival_date_year","arrival_date_month","arrival_date_week_number","arrival_date_day_of_month",
                                     "previous_bookings_not_canceled",
                                     "reservation_status","reservation_status_date"))

set.seed(52)
train.index <- sample(1:nrow(selected.df), nrow(selected.df)*0.7) 
# Build training and validation set by indexing
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

library(rpart)

library(rpart.plot)
#build a tree
default.ct <- rpart(is_canceled ~ ., data = train.df, method = "class")
#plot tree       
prp(default.ct,type=1, extra = 1)
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

#prp(my.ct,  type=1, extra = 1)
library(caret)
# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred, factor(valid.df$is_canceled),positive='1')
#str(selected.df)
#factor(CHAS)
#library(ggplot2)
#ggplot(data=HB2)+geom_boxplot(aes(x=factor(is_canceled),y=lead_time))
#ggplot(data=HB2)+geom_boxplot(aes(x=factor(is_canceled),y=booking_changes))
#ggplot(data=HB2)+geom_boxplot(aes(x=factor(is_canceled),y=adr))
#ggplot(data=HB2)+g#eom_boxplot(aes(x=factor(is_canceled),y=children))

#str(HB2)


#