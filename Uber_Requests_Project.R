# Title
# Examining Uber Trip Data.

# 1. **Dedication:**   
# This project is dedicated to my two daughters.  Love self first to understand how to love another.

# 2. **Introduction:**   
# I will analyze Uber Request Data from ANUPAM MAJHI's Kaggle(https://www.kaggle.com/datasets/anupammajhi/uber-request-data).

## 2.1 **Objective:**  
# Uber is one of the top ride sharing companies in the world. In this document we will explore Uber request data. 

## 2.2 **Data Installation:**  
# Upload following packages and libraries for data exploration. 

library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(ggpmisc)
library(janitor)
library(lubridate)
library(viridisLite)
library(broom)
library(xfun)
library(htmltools)
library(ggfortify)
library(gtsummary)
library(vroom)
library(gtools)
library(hrbrthemes)
library(plotrix)
library(GGally)
library(timeDate)
library(parsnip)
library(GGally)
library(scales)

options(scipen = 999)
options(timeout = 320)
gc()

# 3. **Data Analysis:**   

# For chromebook users, if the following doesn't work try:

Tools <- xfun::pkg_load2(c("htmltools", "mime")) 
Download_Link <- xfun::embed_files(c('UBER_Request.csv'))
Download_Link
if(interactive() ) htmltools::browsable(Download_Link)

UBER_Data <- read.csv(
  'UBER_Request.csv')


dim(UBER_Data)
summary(UBER_Data)
any(is.na(UBER_Data))
sum(is.na(UBER_Data))
colSums(is.na(UBER_Data))
view(UBER_Data)
table(UBER_Data$Pickup.point)
table(UBER_Data$Status)

e <- ggplot(UBER_Data, aes(Request.id))
e + geom_density(aes(fill = factor(Status)), alpha= 0.8)+
  labs(
    title = " All Uber Trip Statuses",
    subtitle = "Uber Trips Project 2023",
    caption = "Portions of this data is from the Reference Section.",
    x = "Total Number of Unique Requests",
    y = "Density",
  )+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic",hjust = 0.5))

e <- ggplot(data = UBER_Data, mapping = aes(x = Pickup.point, y = Request.id/1782.002, fill = Status))
e +  geom_bar(stat = "identity")+
  labs(
    title = " Status of All Airport and City Uber Requests ",
    subtitle = "Uber Trips Project 2023",
    caption = "Portions of this data is from the Reference Section.",
    x = "Pick Up Location",
    y = "Total Number of Unique Requests",
  )+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(face = "italic",hjust = 0.5))+
  coord_flip()

# which Driver completed the most trips 
most_trips <- UBER_Data %>%
  filter(Status == "Trip Completed") %>%
  group_by(Driver.id, Status == "Trip Completed") %>%
  arrange(Driver.id >= 1, Status == "Trip Completed") %>%
  tally()

colnames(most_trips) <- c("Driver.id", "Trip_Completed", "Total")

most_trips[which.max(most_trips$Total), ]
most_trips[which(most_trips$Total > 15), ]

qplot(data = most_trips, x = Driver.id, y = Total > 15)


# Find out who completed the least amount of trips 

min_trips <- UBER_Data %>%
  filter(Status == "Trip Completed") %>%
  group_by(Driver.id, Status == "Trip Completed") %>%
  arrange(Driver.id >= 1, Status == "Trip Completed") %>%
  tally()

colnames(min_trips) <- c("Driver.id", "Trip_Completed", "Total")

min_trips[which.min(most_trips$Total), ]
min_trips[which(min_trips$Total <5), ]

qplot(data = min_trips, x = Driver.id, y = Total < 5)

# Average Total of cancellations 

mean(most_trips$Total)

# Find out who had the most cancellations 

most_can <- UBER_Data %>%
  filter(Status == "Cancelled") %>%
  group_by(Driver.id, Status == "Cancelled") %>%
  arrange(Driver.id >= 1, Status == "Cancelled") %>%
  tally()


colnames(most_can) <- c("Driver.id", "Cancelled", "Total")

most_can[which.max(most_can$Total), ]

most_can[which(most_can$Total >11), ]

qplot(data = most_can, x = Driver.id, y = Total > 11)

# Find out who had the least amount of cancellations

min_can <- UBER_Data %>%
  filter(Status == "Cancelled") %>%
  group_by(Driver.id, Status == "Cancelled") %>%
  arrange(Driver.id >= 1, Status == "Cancelled") %>%
  tally()

colnames(min_can) <- c("Driver.id", "Cancelled", "Total")

min_can[which.min(min_can$Total), ]

min_can[which(min_can$Total <2), ]

qplot(data = min_can, x = Driver.id, y = Total < 2)

# Average Total of cancellations 
mean(min_can$Total)

# Find out the ratio of completed trips vice cancelled trips
mean(most_trips$Total)/mean(min_can$Total)

# Which had the most cancellations, Airport or City?
Pickup_Can <- UBER_Data %>%
  filter(Status == "Cancelled") %>%
  group_by(Pickup.point == "Airport", Pickup.point == "City", Status == "Cancelled") %>%
  arrange(Pickup.point, Status == "Cancelled") %>%
  tally()

colnames(Pickup_Can) <- c("Airport", "City", "Status", "Total")

Pickup_Can[which.max(Pickup_Can$Total), ]
Pickup_Can[which.min(Pickup_Can$Total), ]

qplot(data = Pickup_Can, x = Airport == TRUE, y = Total)

# To get more from the data, lets break it down so we can determine more specific data 

x <- UBER_Data 

y <- x %>% separate(Request.timestamp, c("date", "time"), " ") 
y$date <- str_replace_all(y$date, '-', '/')
y$date <- format(as.Date(y$date), "%Y, %m, %d")
y$date <- str_replace_all(y$date, '20', '2016')
y$time <- format(strptime(y$time, format = '%H:%M'), '%R')

y <- y %>% separate(Drop.timestamp, c("ddate", "ttime"), " ") 
y$ddate <- str_replace_all(y$ddate, '-', '/')
y$ddate <- format(as.Date(y$ddate), "%Y, %m, %d")
y$ddate <- str_replace_all(y$ddate, '20', '2016')
y$ttime <- format(strptime(y$ttime, format = '%H:%M'), '%R')

colnames(y) <- c("Request.id", "Pickup.point", "Driver.id", "Status", "Request.date", 
                 "Request.time", "Dropoff.Date", "Dropoff.Time")

# There are alot of NAs in this data less replace them with a coded time and date. if 
# the time is "00:00" that indicates the data is NA. if the date is "17, 07, 2016" 
# it indicates the data  is NA.

y$Driver.id[is.na(y$Driver.id)] <- 0
y$Request.time[is.na(y$Request.time)] <- "00:00"
y$Dropoff.Time[is.na(y$Dropoff.Time)] <- "00:00"
y$Dropoff.Date[is.na(y$Dropoff.Date)] <- "17, 07, 2016"


ggplot(data.frame(y), aes(x=Status)) +
  geom_bar()

percent(1264/6745)
percent(2650/6745)
percent(2831/6745)

a <- 1264
b <- 2650
c <- 2831

Update_Category <- c("Cancelled\n 19%" = a, "No Available\n  Cars 39%" = b, "Trip\n Completed 42%" = c)
pie3D(Update_Category,
      col=c("yellow", "red", "green"),
      labels = names(Update_Category),
      radius=0.3,
      height = .6,
      labelcex = .70,
      explode=0.4,
      shade = 0.8,
      theta = .7,
      main=" Overall Uber Trip Data")

# Trips Completed vs Trips Not Completed 

1264+2650
percent(3914/6745)
percent(2831/6745)

nc <- 3914
tc <- 2831

Categ <- c("Trips\n Not Completed 58%" = nc, "Trips\n Completed 42%" = tc)
pie3D(Categ,
      col=c("blue", "green"),
      labels = names(Categ),
      radius=0.8,
      height = .3,
      labelcex = .70,
      explode=0.4,
      shade = 0.8,
      theta = .7,
      main=" Completed vs Not Competed Uber Trip Data")

# What time was the most cancellations
TD_Can <- y %>%
  filter(Status == "Cancelled") %>%
  group_by(Request.time, Status == "Cancelled") %>%
  arrange(Request.time, Status == "Cancelled") %>%
  tally()

colnames(TD_Can) <- c("Request_Time", "Status", "Total")

TD_Can[which.max(TD_Can$Total), ]
TD_Can[which.min(TD_Can$Total), ]

qplot(data = TD_Can, x = Request_Time, y = Total)

# What day were the most cancellations
D_Can <- y %>%
  filter(Status == "Cancelled") %>%
  group_by(Request.date, Status == "Cancelled") %>%
  arrange(Request.date, Status == "Cancelled") %>%
  tally()

colnames(D_Can) <- c("Request_Date", "Status", "Total")

D_Can[which.max(D_Can$Total), ]
D_Can[which.min(D_Can$Total), ]

qplot(data = D_Can, x = Request_Date, y = Total)


# What time has the most trips
T_comp <- y %>%
  filter(Status == "Trip Completed") %>%
  group_by(Request.time, Status == "Trip Completed") %>%
  arrange(Request.time, Status == "Trip Completed") %>%
  tally()

colnames(T_comp) <- c("Request_Time", "Status", "Total")

T_comp[which.max(T_comp$Total), ]
T_comp[which.min(T_comp$Total), ]


# What day were the most trips

d_comp <- y %>%
  filter(Status == "Trip Completed") %>%
  group_by(Request.date, Status == "Trip Completed") %>%
  arrange(Request.date, Status == "Trip Completed") %>%
  tally()

colnames(d_comp) <- c("Request_Date", "Status", "Total")

d_comp[which.max(d_comp$Total), ]
d_comp[which.min(d_comp$Total), ]

qplot(data = d_comp, x = Request_Date, y = Total)


# 4. Machine Learning 

#Now lets create a Logistic regression machine learning algorithm that will predict if a trip will be cancelled or not available.

y$Trips <- ifelse(y$Status == "Trip Completed", "Trip Completed", "Trip Not Completed")

y <- y %>%
  mutate(Trip_Not_Completed = ifelse(Status %in% c("No Cars Available", "Cancelled"), 1, 0),
         Trips = ifelse(Status == "Trip Completed", 1, 0)) %>%
  select(-Status)

y = subset(y, select = -c(Trip_Not_Completed) )

any(is.na(y))
sum(is.na(y$Driver.id))
table(which(is.na(y), arr.ind=TRUE))

as.numeric(y$Request.id, y$Driver.id, y$Dropoff.Time, y$Request.time)

y$Request.time <- factor(y$Request.time, ordered = FALSE)
y$Dropoff.Time  <- factor(y$Dropoff.Time, ordered = FALSE)

set.seed(123)
train_index <- createDataPartition(y$Trips, p = 0.7, list = FALSE, times = 1)
train <- y[train_index, ]
test <- y[-train_index, ]

log_model_samp <- glm(Trips ~ Request.id + Pickup.point + Request.time + Driver.id, data = train, 
                      family = binomial(link = "logit"))

# View the model output
summary(log_model_samp)

sapply(log_model_samp$xlevels, function(x) print(length(x)))

log_model_samp$xlevels[["Request.time"]]<- union(log_model_samp$xlevels[["Request.time"]], levels(test$"Request.time"))
log_model_samp$xlevels[["Dropoff.Time"]]<- union(log_model_samp$xlevels[["Dropoff.Time"]], levels(test$"Dropoff.Time"))
log_model_samp$xlevels[["Pickup.point"]]<- union(log_model_samp$xlevels[["Pickup.point"]], levels(test$"Pickup.point"))

# Use the model to make predictions on the testing set
predictions <- predict(log_model_samp, newdata = test, type = "response")

hist(log_model_samp$residuals)

# Convert probabilities to class predictions
predicted_classes <- ifelse(predictions > 0.5, "Trip Completed", "Trip Not Completed")


# Calculate the accuracy of the model
accuracy <- mean((predictions <= 0.5) == (test$Trips == "0"))

# Print the accuracy of the model

cat("Accuracy of the model:", accuracy)

# Observe the predicted vs actual from Test set
table(log_model_samp$model$Trips)

table(test$Trips)

# Average Trip Not Completed in the Original DataSet
sum(y$Trips == 0)/sum(y$Trips == 0, y$Trips == 1)

# Average Trip Not Completed in the Train Data Set
sum(train$Trips == 0)/sum(train$Trips == 0, train$Trips == 1)

# Average Trip Not Completed in the Model
sum(log_model_samp$model$Trips == 0)/sum(log_model_samp$model$Trips == 0, 
                                         log_model_samp$model$Trips == 1)

#5. Conclusion.

# The accuracy of the model is less than 60%. This is basically a coin flip. Without the distance or other critical data it is hard to train the algorithm.
