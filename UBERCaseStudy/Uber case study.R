#------------------------------------------------------------------------------------ 
#                                 UBER CASE STUDY
#------------------------------------------------------------------------------------ 

#---------------------------Fetching data from csv to R ----------------------------- 

uber <- read.csv("Uber Request Data.csv")
View(uber)

library(ggplot2)


# --------------------------DATA CLEANING AND PREPARATION-----------------------------

# 1. Check for duplicate
sum(duplicated(uber$Request.id))  
# > 0  No duplicate

# 2. Standardising Dates
typeof(uber$Request.timestamp) #it is showing as integer
#converting to char
uber$Request.timestamp <- as.character(uber$Request.timestamp) 


typeof(uber$Drop.timestamp) #it is showing as integer
#converting to char
uber$Drop.timestamp <- as.character(uber$Drop.timestamp) 


# Both columns dates are not in proper format. It contains "11/7/2016 11:51" as well as "13-07-2016 08:33:16" format
# To make both in same format, 2 things needs to be done
#   1. append :00 for the date formats "11/7/2016 11:51"
uber$Request.timestamp <- ifelse(grepl("/", uber$Request.timestamp), paste(uber$Request.timestamp, ":00", sep = ""), uber$Request.timestamp)
uber$Drop.timestamp <- ifelse(grepl("/", uber$Drop.timestamp), paste(uber$Drop.timestamp, ":00", sep = ""), uber$Drop.timestamp)

#   2. replace "/" with "-"
uber$Request.timestamp <- ifelse(grepl("/", uber$Request.timestamp), gsub("/", "-", uber$Request.timestamp), uber$Request.timestamp)
uber$Drop.timestamp <- ifelse(grepl("/", uber$Drop.timestamp), gsub("/", "-", uber$Drop.timestamp), uber$Drop.timestamp)

# converting to POSIXlt
uber$Request.timestamp <- as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp <- as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")


# Alternate for date conversion

# using "lubridate" package to get the dates in proper format

# install.packages("lubridate")     # if not installed pls install this
# library(lubridate)
# uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, c("dmy_HMS", "dmy_HM"), tz="Asia/Calcutta")
# uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, c("dmy_HMS", "dmy_HM"), tz = "Asia/Calcutta")







# ------------------------------DERIVING NEW VARIABLES---------------------------------------

# 1st derived column --------
# Checking what can be derived from status column

table(uber$Status)
# >    Cancelled   No Cars Available    Trip Completed 
# >      1264              2650              2831 

# Here Supply is only the trips completed and demand would be all the requests
# We will get demand by nrow of uber, but for supply we can derive a column

uber$supply <- ifelse( uber$Status == "Trip Completed", 1, 0 )

table(uber$supply) # to cross verify the trips completed number
# >  0    1 
# > 3914 2831 



# 2nd derived column ----------
# We need time slots for requests, following is the time slots i ahve considered
# 1. Early moring 4am to 6:59am 
# 2. Morning      7am to 11:59am
# 3. Afternoon    12 to 3:59pm
# 4. Evening      4pm to 7:59pm
# 5. Night        8 to 11:59pm
# 6. Late night   12am to 3:59am

uber$Req.hour <- format(uber$Request.timestamp, "%H")
str(uber$Req.hour) #its char
# so converting to numeric
uber$Req.hour <- as.numeric(uber$Req.hour)
uber$Req.timeslot <- ifelse(uber$Req.hour >= 4 & uber$Req.hour < 7, "Early morning",
                      ifelse(uber$Req.hour >= 7 & uber$Req.hour < 12, "Morning",
                        ifelse(uber$Req.hour >= 12 & uber$Req.hour < 16, "Afternoon",
                          ifelse(uber$Req.hour >= 16 & uber$Req.hour < 20, "Evening",
                            ifelse(uber$Req.hour >= 20 & uber$Req.hour < 24, "Night", "Late Night")))))




#write.csv(x = uber, file = "uber.csv")


# ------------------------------ANALYSING DATA--------------------------------------

# 1. Checking data for outliers
ggplot(uber, aes(x = uber$Request.timestamp)) + geom_histogram()
# this graph gives the information of how the cab booking requests have been spread across dates
# Nothing much interesting from this graph, Almost the data is pretty much similar

# 2.Checking any particular time booking has demand
ggplot(uber, aes(x = uber$Req.hour)) + geom_histogram()
# Usually Evening time has more demand than any time of day

# 3. Trying to find something about, time taken to complete the trip
ggplot(uber, aes(x = as.numeric(uber$Drop.timestamp - uber$Request.timestamp), fill = uber$Req.timeslot)) + geom_histogram() 
# Majority of the trips takes 40 to 60 min to complete


# ------------------------------RESULTS EXPECTED--------------------------------------

# 1. Visually identify the most pressing problems for Uber. 

#--------
# This plot visualies the frequency of requests that gets cancelled or show no cars available 
# across different time slots
ggplot(uber, aes(uber$Status, ..count..)) +
  geom_bar(aes(fill = uber$Req.timeslot), position = "dodge", width = 0.7) +
  theme_minimal() + 
  scale_x_discrete(limits = c("Cancelled", "No Cars Available")) 
# After visualising the graph, we can see that the frequency of requests that gets cancelled 
# are more towards beginning of the day i.e Early morning and morning, Where as the requests that shows no cars available 
# is more towards end of the day i.e. Evening and Night.

# The reason for choosing dodge bar is it clearly differentiates/compares values sid by side 
# using bars, which will be very clear for analysing.
# --------



#--------
# This plot visualies the frequency of requests that gets cancelled or show no cars available 
# at Airport/City
ggplot(uber, aes(uber$Status, ..count..)) +
  geom_bar(aes(fill = uber$Pickup.point), position = "dodge", width = 0.7) +
  theme_minimal() + 
  scale_x_discrete(limits = c("Cancelled", "No Cars Available")) 
# After careful observation we can see that ther is lot of demand in airport but there are no 
# enough cabs available at the airport

# The reason for choosing dodge bar is it clearly differentiates/compares values sid by side 
# using bars, which will be very clear for analysing.
#--------





# 2. Find out the gap between supply and demand and show the same using plots.
#   - Find the time slots when the highest gap exists
#   - Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

# Assumptions made: 
#   Here Supply is only the trips completed and demand would be all the requests
#   We will get demand by nrow of uber, and supply by supply column which is 1.

ggplot(uber, aes(x = uber$Req.timeslot, fill = factor(uber$supply))) + 
  geom_bar() + facet_wrap(~uber$Pickup.point)

# In this graph 
#   0 : Gap between supply and demand
#   1 : Supply of cabs
#   total of each stacked bar  is demand at that particular time slot



# 3. What do you think is the reason for this issue for the supply-demand gap?
# Ans:
#   These are few observation from the graph, from which we will get the insight of why there is 
# a supply-demand gap
#   - There are lot of demand/requests from city to airport in the morning time (4am to 12pm) 
#   - Where as there is no demand in the airport at morning times (4am to 12pm)
#   - The demand for cabs in the airport is high only in the evening and night times (4pm to 12am)
#   - Hence the driver going from city to airportin the morning might not get the trip back 
#     to city immediately. He might need to wait for longer period in the airport.




# 4. Recommend some ways to resolve the supply-demand gap.
# It is presented in the ppt




