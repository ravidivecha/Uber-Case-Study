###### Uber Case Study Solution
setwd("C:\\Users\\eravdiv\\Desktop\\Ravi\\PG-Data-Science\\Uber-Case-Study")

library(dplyr)
library(stringr)
library(ggplot2)

Uber_Airport_Data <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

### Data Cleaning and Preparation - Hints

# Identify the data quality issues and clean the data so that you can use it for analysis.
# Ensure that the dates and time are in the proper format. Derive new variables which will 
# be useful for analysis.

Uber_Airport_Data$Request.timestamp <- gsub("-","/" ,Uber_Airport_Data$Request.timestamp)
Uber_Airport_Data$Drop.timestamp <- gsub("-","/" ,Uber_Airport_Data$Drop.timestamp)

# Seconds is ignored for this analysis since it will not make any significant difference in the analysis.

Uber_Airport_Data$Request.timestamp<- as.POSIXct(Uber_Airport_Data$Request.timestamp, format = "%d/%m/%Y %H:%M")
Uber_Airport_Data$Drop.timestamp<- as.POSIXct(Uber_Airport_Data$Drop.timestamp, format = "%d/%m/%Y %H:%M")

## Create new columns for year, month, date, hour, minute

Uber_Airport_Data$Request_Year <- format(Uber_Airport_Data$Request.timestamp, "%Y")
Uber_Airport_Data$Request_Month <- format(Uber_Airport_Data$Request.timestamp, "%m")
Uber_Airport_Data$Request_Date <- format(Uber_Airport_Data$Request.timestamp, "%d")
Uber_Airport_Data$Request_Hour <- format(Uber_Airport_Data$Request.timestamp, "%H")
Uber_Airport_Data$Request_Min <- format(Uber_Airport_Data$Request.timestamp, "%M")

Uber_Airport_Data$Drop_Year <- format(Uber_Airport_Data$Drop.timestamp, "%Y")
Uber_Airport_Data$Drop_Month <- format(Uber_Airport_Data$Drop.timestamp, "%m")
Uber_Airport_Data$Drop_Date <- format(Uber_Airport_Data$Drop.timestamp, "%d")
Uber_Airport_Data$Drop_Hour <- format(Uber_Airport_Data$Drop.timestamp, "%H")
Uber_Airport_Data$Drop_Min <- format(Uber_Airport_Data$Drop.timestamp, "%M")

## Identify Demand Supply Gap by separating trips completed and all the others.
Uber_Airport_Data$DemandSupplyGap <- ifelse(Uber_Airport_Data$Status == "Trip Completed", "Yes", "No")

Uber_Airport_Data$Request_Hour <- as.numeric(Uber_Airport_Data$Request_Hour)

## Time slots are idenfied based on the hour of day to further categorize the demand supply gap.
for (i in 1:length(Uber_Airport_Data$Request_Hour)) {
  if (Uber_Airport_Data$Request_Hour[i] %in% c(23, 0, 1, 2, 3, 4)) {
    Uber_Airport_Data$TimeOFDay[i] <- "Night(11pm-4am)"
  } 
  if (Uber_Airport_Data$Request_Hour[i] %in% c(5,6,7,8,9,10)) {
    Uber_Airport_Data$TimeOFDay[i] <- "Morning(5am-10am)"
  }
  if (Uber_Airport_Data$Request_Hour[i] %in% c(11,12,13,14,15,16)) {
      Uber_Airport_Data$TimeOFDay[i] <- "Afternoon(11am-4pm)"
  } 
  if (Uber_Airport_Data$Request_Hour[i] %in% c(17,18,19,20,21,22)) {
    Uber_Airport_Data$TimeOFDay[i] <- "Evening(5pm-10pm)"
  }  
}

# Obtain the trip time for completed rides
Uber_Airport_Data$Trip_Time <- (as.numeric(Uber_Airport_Data$Drop.timestamp)-as.numeric(Uber_Airport_Data$Request.timestamp))/60

# Data shows that days to be Monday to Friday and the average demand reamins the same throughout the weekdays.
table(Uber_Airport_Data$Request_Date)

##Write the cleansed data to .csv file.

write.csv(Uber_Airport_Data, file = "Uber_Data_from_R.csv", row.names = FALSE)

## Results Expected
# 1. Visually identify the most pressing problems for Uber. 
# 2. Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
# 3. identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots 
# (early mornings, late evenings etc.) using plots
# 4. Find out the gap between supply and demand and show the same using plots.
# 5. Find the time slots when the highest gap exists
# 6. Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
# What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. 
# You may accompany the write-up with plot(s).
# Recommend some ways to resolve the supply-demand gap.


# 1. Visually identify the most pressing problems for Uber. 
# This plot shows that there are more trips not completed than those which are completed.

Uber_Problem <- data.frame(DemandSupplyGap = unique(Uber_Airport_Data$DemandSupplyGap))
Uber_Problem$Count <- c(length(which(Uber_Airport_Data$DemandSupplyGap == "Yes")), length(which(Uber_Airport_Data$DemandSupplyGap == "No"))) 
Uber_Problem$Percentage <- c(round(length(which(Uber_Airport_Data$DemandSupplyGap == "Yes"))/length(Uber_Airport_Data$DemandSupplyGap)*100, 2), round(length(which(Uber_Airport_Data$DemandSupplyGap == "No"))/length(Uber_Airport_Data$DemandSupplyGap)*100,2)) 

ggplot(Uber_Problem, aes(x= Uber_Problem$DemandSupplyGap, y = Uber_Problem$Count, fill = Uber_Problem$Count)) + geom_bar(stat = "identity", show.legend = FALSE) + geom_text(aes(y = Uber_Problem$Count/2,label = paste(Uber_Problem$Count, "\n", Uber_Problem$Percentage, "%")), colour = "white", size = 6) + theme_bw() + labs(title = "Uber Problem", x = "Trip Completed", y = "Status_Count") 


# 2. Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
# Time slot vs status of request
# This plot shows that during morning hours there are more cancellations and during evening hours there are lack of cabs.
# Distribution of pickup point vs status
# This plot shows that there are many occurrances of no cars availabe when the pickup point is airport which indicates shortage of cabs.
# This plot also shows that many cab drivers dont want to go to airport from the city and therefore cancel the rides. There is also shortage
# of cabs to travel from the city.

filter1 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "Trip Completed"))
filter2 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "No Cars Available"))
filter3 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "Cancelled"))
filter4 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "Trip Completed"))
filter5 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "No Cars Available"))
filter6 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "Cancelled"))


len1 <- length(which(Uber_Airport_Data$Pickup.point == "Airport"))
len2 <- length(which(Uber_Airport_Data$Pickup.point == "City"))

len3 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "Trip Completed"))
len4 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "No Cars Available"))
len5 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$Status == "Cancelled"))
len6 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "Trip Completed"))
len7 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "No Cars Available"))
len8 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$Status == "Cancelled"))


Uber_PickUp_Status <- data.frame(PickUp = c("Airport", "Airport", "Airport", "City", "City", "City"))
Uber_PickUp_Status$Status <- c("Trip Completed", "No Cars Available", "Cancelled", "Trip Completed", "No Cars Available", "Cancelled" ) 
Uber_PickUp_Status$Count <- c(filter1, filter2, filter3, filter4, filter5, filter6)
Uber_PickUp_Status$Percentage <- c(round(len3/len1*100,2), round(len4/len1*100,2), round(len5/len1*100,2), round(len6/len2*100,2), round(len7/len2*100,2), round(len8/len2*100,2))

ggplot(Uber_PickUp_Status, aes(x = Uber_PickUp_Status$PickUp, y = Uber_PickUp_Status$Count, fill = Status )) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(y = Uber_PickUp_Status$Count/2,label = paste(Uber_PickUp_Status$Count, "\n", Uber_PickUp_Status$Percentage, "%")), colour = "white", size = 6, position = position_dodge(.9)) + theme_bw() + labs(title = "PickUp vs Status", x = "Pickup Point", y = "Status_Count") 


# 3. identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots 
# (early mornings, late evenings etc.) using plots
# Time slot vs pickup point
# The plot shows that during 5am-9am there are requests to trvel from city to airport
# and from 5pm to 10pm maximum requests are to travel from airport to city.

ggplot(Uber_Airport_Data, aes(x = factor(Uber_Airport_Data$Request_Hour), y = Uber_Airport_Data$Pickup.point, fill = Pickup.point)) + geom_bar(stat = "identity") + theme_bw() + labs(title = "PickUp vs Requested Hour", x = "Requested Hour", y = "Pickup Point")

filter7 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
filter8 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
filter9 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
filter10 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))
filter11 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
filter12 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
filter13 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
filter14 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

leb9 <- length(which(Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb10 <- length(which(Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb11 <- length(which(Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb12 <- length(which(Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

leb13 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb14 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb15 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb16 <- length(which(Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))
leb17 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb18 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb19 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb20 <- length(which(Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

Uber_PickUp_TimeOfDay <- data.frame(PickUp = c("Airport", "City", "Airport", "City", "Airport", "City", "Airport", "City"))
Uber_PickUp_TimeOfDay$TimeOfDay <- c("Morning(5am-10am)", "Morning(5am-10am)", "Afternoon(11am-4pm)", "Afternoon(11am-4pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Night(11pm-4am)", "Night(11pm-4am)")
Uber_PickUp_TimeOfDay$Count <- c(filter7, filter11, filter8, filter12, filter9, filter13, filter10, filter14)
Uber_PickUp_TimeOfDay$Percentage <- c(round(leb13/leb9*100,2), round(leb17/leb9*100,2), round(leb14/leb10*100,2), round(leb18/leb10*100,2), round(leb15/leb11*100,2), round(leb19/leb11*100,2), round(leb16/leb12*100,2), round(leb20/leb12*100, 2))

ggplot(Uber_PickUp_TimeOfDay, aes(x = Uber_PickUp_TimeOfDay$TimeOfDay, y = Uber_PickUp_TimeOfDay$Count, fill = PickUp)) + geom_bar(stat = "identity", position = "dodge")  + geom_text(aes(y = Uber_PickUp_TimeOfDay$Count/2,label = paste(Uber_PickUp_TimeOfDay$Percentage, "%")), colour = "white", size = 6, position = position_dodge(.9)) + theme_bw() + labs(title = "TimeOfDay vs Pickup", x = "TimeOfDay", y = "Pickup") 


# 4. Find out the gap between supply and demand and show the same using plots.
# This chart shows the trips completed and not completed in a specific hour of the day.

ggplot(Uber_Airport_Data, aes(x = factor(Uber_Airport_Data$Request_Hour), y = Uber_Airport_Data$DemandSupplyGap, fill = DemandSupplyGap)) + geom_bar(stat = "identity") + theme_bw() + labs(title = "Demand Supply Gap vs Requested Hour", x = "TimeOfDay", y = "Demand Supply Gap") 

filter15 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
filter16 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
filter17 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
filter18 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))
filter19 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
filter20 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
filter21 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
filter22 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

leb20 <- length(which(Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb21 <- length(which(Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb22 <- length(which(Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb23 <- length(which(Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

leb24 <- length(which(Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb25 <- length(which(Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb26 <- length(which(Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb27 <- length(which(Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))
leb28 <- length(which(Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))
leb29 <- length(which(Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Afternoon(11am-4pm)"))
leb30 <- length(which(Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)"))
leb31 <- length(which(Uber_Airport_Data$DemandSupplyGap == "No" & Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)"))

Uber_PickUp_DemandSupplyGap <- data.frame(DemandSupplyGap = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"))
Uber_PickUp_DemandSupplyGap$TimeOfDay <- c("Morning(5am-10am)", "Morning(5am-10am)", "Afternoon(11am-4pm)", "Afternoon(11am-4pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Night(11pm-4am)", "Night(11pm-4am)")
Uber_PickUp_DemandSupplyGap$Count <- c(filter15, filter19, filter16, filter20, filter17, filter21, filter18, filter22)
Uber_PickUp_DemandSupplyGap$Percentage <- c(round(leb24/leb20*100,2), round(leb28/leb20*100,2), round(leb25/leb21*100,2), round(leb29/leb21*100,2), round(leb26/leb22*100,2), round(leb30/leb22*100,2), round(leb27/leb23*100,2), round(leb31/leb23*100, 2))

ggplot(Uber_PickUp_DemandSupplyGap, aes(x = Uber_PickUp_DemandSupplyGap$TimeOfDay, y = Uber_PickUp_DemandSupplyGap$Count, fill = DemandSupplyGap)) + geom_bar(stat = "identity", position = "dodge")  + geom_text(aes(y = Uber_PickUp_DemandSupplyGap$Count/2,label = paste(Uber_PickUp_DemandSupplyGap$Percentage, "%")), colour = "white", size = 6, position = position_dodge(.9)) + theme_bw() + labs(title = "TimeOfDay vs Demand Supply Gap", x = "TimeOfDay", y = "Demand Supply Gap") 


# 5. Find the time slots when the highest gap exists
# Write code to show difference in demand supply gap to identify the peak timeslot

NightGap <- sum((Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)" & Uber_Airport_Data$DemandSupplyGap == "Yes")) - sum((Uber_Airport_Data$TimeOFDay == "Night(11pm-4am)" & Uber_Airport_Data$DemandSupplyGap == "No"))
MorningGap <- sum((Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$DemandSupplyGap == "Yes")) - sum((Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$DemandSupplyGap == "No"))
AfternoonGap <- sum((Uber_Airport_Data$TimeOFDay ==  "Afternoon(11am-4pm)" & Uber_Airport_Data$DemandSupplyGap == "Yes")) - sum((Uber_Airport_Data$TimeOFDay ==  "Afternoon(11am-4pm)" & Uber_Airport_Data$DemandSupplyGap == "No"))
EveningGap <- sum((Uber_Airport_Data$TimeOFDay ==   "Evening(5pm-10pm)" & Uber_Airport_Data$DemandSupplyGap == "Yes")) - sum((Uber_Airport_Data$TimeOFDay ==   "Evening(5pm-10pm)" & Uber_Airport_Data$DemandSupplyGap == "No"))

TimeSlot_Gap <- data.frame(c("Night(11pm-4am)", "Morning(5am-10am)", "Afternoon(11am-4pm)", "Evening(5pm-10pm)" ), c(NightGap, MorningGap, AfternoonGap, EveningGap))

print(paste("Time slot with maximum gap in demand and supply is",TimeSlot_Gap$c..Night.11pm.4am.....Morning.5am.10am.....Afternoon.11am.4pm....[which(TimeSlot_Gap$c.NightGap..MorningGap..AfternoonGap..EveningGap. == min(TimeSlot_Gap$c.NightGap..MorningGap..AfternoonGap..EveningGap.))]))

# 6. Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
# The below graphs show that in evening time pickup from airport is most affected due to unavailablity of cabs.

filter23 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "Trip Completed"))
filter24 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "Cancelled"))
filter25 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "No Cars Available"))
filter26 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "Trip Completed"))
filter27 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "Cancelled"))
filter28 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Evening(5pm-10pm)" & Uber_Airport_Data$Status == "No Cars Available"))
filter29 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "Trip Completed"))
filter30 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "Cancelled"))
filter31 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "Airport" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "No Cars Available"))
filter32 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "Trip Completed"))
filter33 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "Cancelled"))
filter34 <- nrow(filter(Uber_Airport_Data, Uber_Airport_Data$Pickup.point == "City" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)" & Uber_Airport_Data$Status == "No Cars Available"))

leb33 <- length(which(Uber_Airport_Data$DemandSupplyGap == "Yes" & Uber_Airport_Data$TimeOFDay == "Morning(5am-10am)"))

Uber_PickUp_DemandSupplyGap_Time <- data.frame(TimeSlot = c("Evening(5pm-10pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Evening(5pm-10pm)", "Morning(5am-10am)", "Morning(5am-10am)", "Morning(5am-10am)", "Morning(5am-10am)", "Morning(5am-10am)", "Morning(5am-10am)"))
Uber_PickUp_DemandSupplyGap_Time$Pickup <- c("Airport", "Airport", "Airport", "City", "City", "City", "Airport", "Airport", "Airport", "City", "City", "City")
Uber_PickUp_DemandSupplyGap_Time$Status <- c("Trip Completed", "Cancelled", "No Cars Available", "Trip Completed", "Cancelled", "No Cars Available", "Trip Completed", "Cancelled", "No Cars Available", "Trip Completed", "Cancelled", "No Cars Available")
Uber_PickUp_DemandSupplyGap_Time$Count <- c(filter23, filter24, filter25, filter26, filter27, filter28, filter29, filter30, filter31, filter32, filter33, filter34)
Uber_PickUp_DemandSupplyGap_Time$Percentage <- c(round(filter23/leb15*100,2), round(filter24/leb15*100,2), round(filter25/leb15*100,2),round(filter26/leb19*100,2),round(filter27/leb19*100,2),round(filter28/leb19*100,2),round(filter29/leb13*100,2),round(filter30/leb13*100,2),round(filter31/leb13*100,2),round(filter32/leb17*100,2),round(filter33/leb17*100,2),round(filter34/leb17*100,2))

ggplot(Uber_PickUp_DemandSupplyGap_Time, aes(x= Uber_PickUp_DemandSupplyGap_Time$Pickup, y = Uber_PickUp_DemandSupplyGap_Time$Count, fill = Status)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Uber_PickUp_DemandSupplyGap_Time$TimeSlot) + theme_bw() + geom_text(aes(y = Uber_PickUp_DemandSupplyGap_Time$Count/2,label = paste(Uber_PickUp_DemandSupplyGap_Time$Percentage, "%")), colour = "white", size = 4, position = position_dodge(.9)) + theme_bw() + labs(title = "Evening/Morning Demand Supply Gap", x = "Evening Pickup                                                                                                 Morning Pickup", y = "Trip Status") 

## Average Time of completed trips

mean(Uber_Airport_Data$Trip_Time, na.rm = TRUE)
