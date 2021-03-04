#############################################################################

# Topics Covered in this GBM study:
#  
# Step 1: Import the data
# Step 2: Clean/Transform the dataset
# Step 3: Create train/test set - line 422
# Step 4: Build the model - line 444
# Step 5: Make prediction- line 498
# Step 6: Measure performance - line 556
# Step 7: Conclusion -line 588

##############################################################################
#Load packages
# packages --------------------------------------------------
library(tidyverse)
library(rsample) # data splitting
library(randomForest) # basic implementation
library(ranger) # a faster implementation of randomForest
library(caret) # an aggregator package for performing many machine learning models
library(ggthemes)
library(scales)
library(wesanderson)
library(styler)


# Step 1: Import the data

#Import the data
#change the working directory
setwd("~/Documents/DataScienceR-master/Decision Tree")
bike <- readr::read_csv("Bike-Sharing-Dataset/day.csv")

head(bike)
str(bike)


# Step 2: Clean/Transform the dataset

#Wrangling the data

# Attribute Information:
# - instant: record index
# - dteday : date
# - season : season (1:winter, 2:spring, 3:summer, 4:fall)
# - yr : year (0: 2011, 1:2012)
# - mnth : month ( 1 to 12)
# - hr : hour (0 to 23)
# - holiday : weather day is holiday or not (extracted from https://dchr.dc.gov/page/holiday-schedules)
# - weekday : day of the week
# - workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
# + weathersit :
#   - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#   - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#   - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#   - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# - temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
# - atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
# - hum: Normalized humidity. The values are divided to 100 (max)
# - windspeed: Normalized wind speed. The values are divided to 67 (max)
# - casual: count of casual users
# - registered: count of registered users
# - cnt: count of total rental bikes including both casual and registered

# this records the weekday variable into a character variable
# test 
# bike %>%
#   mutate(
#     weekday_chr =
#       case_when(
#         weekday == 0 ~ "Sunday",
#         weekday == 1 ~ "Monday",
#         weekday == 2 ~ "Tuesday",
#         weekday == 3 ~ "Wednesday",
#         weekday == 4 ~ "Thursday",
#         weekday == 5 ~ "Friday",
#         weekday == 6 ~ "Saturday",
#         TRUE ~ "other")) %>% 
#     dplyr::count(weekday, weekday_chr) %>%
#     tidyr::spread(weekday, n)

# assign
bike <- bike %>%
  mutate(
    weekday_chr =
      case_when(
        weekday == 0 ~ "Sunday",
        weekday == 1 ~ "Monday",
        weekday == 2 ~ "Tuesday",
        weekday == 3 ~ "Wednesday",
        weekday == 4 ~ "Thursday",
        weekday == 5 ~ "Friday",
        weekday == 6 ~ "Saturday",
        TRUE ~ "other"))

# verify
bike %>% 
dplyr::count(weekday, weekday_chr) %>% 
tidyr::spread(weekday, n)


# Weekdays (factor) ---

# test factor variable
# bike %>%
#   mutate(
#     weekday_fct = factor(x = weekday,
#              levels = c(0,1,2,3,4,5,6),
#              labels = c("Sunday",
#                        "Monday",
#                        "Tuesday",
#                        "Wednesday",
#                        "Thursday",
#                        "Friday",
#                        "Saturday"))) %>%
#   dplyr::count(weekday, weekday_fct) %>%
#   tidyr::spread(weekday, n)

# assign factor variable
bike <- bike %>%
  mutate(
    weekday_fct = factor(x = weekday,
                         levels = c(0,1,2,3,4,5,6),
                         labels = c("Sunday",
                                    "Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday")))

# verify factor variable
bike %>% 
dplyr::count(weekday, weekday_fct) %>% 
tidyr::spread(weekday, n)

# Holidays ----
# assign
bike <- bike %>%
  mutate(holiday_chr =
           case_when(
             holiday == 0 ~ "Non-Holiday",
             holiday == 1 ~ "Holiday"))
# assign
bike <- bike %>%
  mutate(
    holiday_fct = factor(x = holiday,
                         levels = c(0,1),
                         labels = c("Non-Holiday",
                                    "Holiday")))

# Working days ----
# assign
bike <- bike %>%
  mutate(
    workingday_chr =
      case_when(
        workingday == 0 ~ "Non-Working Day",
        workingday == 1 ~ "Working Day",
        TRUE ~ "other")) 
# assign
bike <- bike %>%
  mutate(
    workingday_fct = factor(x = workingday,
                            levels = c(0,1),
                            labels = c("Non-Working Day",
                                       "Working Day")))

# Seasons ----
# assign
bike <- bike %>%
  mutate(
    season_chr =
      case_when(
        season == 1 ~ "Spring",
        season == 2 ~ "Summer",
        season == 3 ~ "Fall",
        season == 4 ~ "Winter",
        TRUE ~ "other"
      ))
# assign
bike <- bike %>%
  mutate(
    season_fct = factor(x = season,
                        levels = c(1, 2, 3, 4),
                        labels = c("Spring",
                                   "Summer",
                                   "Fall",
                                   "Winter"))) 


# Weather situation ----
# assign
bike <- bike %>%
  mutate(
    weathersit_chr =
      case_when(
        weathersit == 1 ~ "Good",
        weathersit == 2 ~ "Clouds/Mist",
        weathersit == 3 ~ "Rain/Snow/Storm"))
# assign 
bike <- bike %>%
  mutate(
    weathersit_fct = factor(x = weathersit,
                            levels = c(1, 2, 3),
                            labels = c("Good",
                                       "Clouds/Mist",
                                       "Rain/Snow/Storm")))

# Months ----
# assign
bike <- bike %>% 
  mutate(month_ord = 
           lubridate::month(mnth, label = TRUE))
# verify
bike %>% 
dplyr::count(month_ord, mnth) %>% 
tidyr::spread(month_ord, n)

# assign
bike <- bike %>%
  mutate(
    month_fct = factor(x = mnth,
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       labels = c("January", "February", "March", "April", "May",
                                  "June", "July", "August", "September", "October",
                                  "November", "December")))

# verify
bike %>% 
dplyr::count(month_ord, month_fct) %>%
tidyr::spread(month_fct, n)

# Year ----
# assign
bike <- bike %>%
  mutate(
    yr_chr =
      case_when(
        yr == 0 ~ "2011",
        yr == 1 ~ "2012"))
# assign
bike <- bike %>%
  mutate(
    yr_fct = factor(x = yr,
                    levels = c(0, 1),
                    labels = c("2011",
                               "2012")))


# normalize temperatures ----
bike <- bike %>%
  mutate(temp = as.integer(temp * (39 - (-8)) + (-8)))

bike <- bike %>%
  mutate(atemp = atemp * (50 - (16)) + (16))

# ~ windspeed ----
bike <- bike %>%
  mutate(windspeed = as.integer(67 * bike$windspeed))

# ~ humidity ----
bike <- bike %>%
  mutate(hum = as.integer(100 * bike$hum))

# ~ convert to date ----
bike <- bike %>%
  mutate(dteday = as.Date(dteday))

# check df
bike %>% dplyr::glimpse()

# rename the data frame so these don't get confused
BikeData <- bike

# reorganize variables for easier inspection

BikeData <- BikeData %>% 
  dplyr::select(
    dplyr::starts_with("week"),
    dplyr::starts_with("holi"),
    dplyr::starts_with("seas"),
    dplyr::starts_with("work"),
    dplyr::starts_with("month"),
    dplyr::starts_with("yr"),
    dplyr::starts_with("weath"),
    dplyr::everything())

# check df
dplyr::glimpse(BikeData)

#Exploratory data analysis
#Summarizing the bike table into summary statistics that will give us a better understanding of the underlying distribution for each variable in the BikeData data frame. 
summary(BikeData)


#Exploring the impact of weather conditions on bike rentals

#Bikers are vulnerable to weather conditions, and weather conditions might impact their likelihood of choosing a bike over other transportation options. 
#If weather conditions are influential in transportation decisions, we would expect to see relationships between the number of bike rentals and weather features including temperature, humidity and wind speed.

# ~ rentals by temperature ----
ggRentalsByTemp <- BikeData %>% 
  ggplot(aes(y = cnt, 
             x = temp, 
             color = weekday_fct)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(se = FALSE,
              show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Temperature (°C)") +
  ggtitle("Bike Rental Volume By Temperature")

ggRentalsByTemp
#The exploratory data analysis showed us that bike rentals seem to drop off at a certain temperature (~20˚C).

#`geom_smooth()` using method = 'loess' and formula 'y ~ x'

#We would also expect windier conditions to negatively impact bike rentals. Let’s analyze the data.
# ggRentalVolByWindSpeed ----
ggRentalVolByWindSpeed <- ggplot(bike) +
  geom_point(aes(y = cnt, 
                 x = windspeed, 
                 color = weekday_fct),
             show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Windspeed") +
  ggtitle("Rental Volume By Windspeed")

ggRentalVolByWindSpeed

#Exploring the impact of holidays on bike rental volume

#Holidays might influence bike riders in different ways. 
#For instance, we can think of holidays as increasing opportunities for bike riders to enjoy being on the road with fewer drivers, since typically fewer people drive on holidays. 
#We could also consider a situation where bike enthusiasts only prefer to ride their bikes on summer or spring holidays (considering the information we’ve learned about the influences of weather conditions on bike rentals).

# ~ rentals by holidays ----
ggRentalVolByHoliday <- ggplot(BikeData) +
  geom_density(aes(x = cnt,
                   fill = holiday_chr), 
               alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") +
  
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(title = "Bike Rental Density By Holiday",
       fill = "Holiday",
       x = "Average Bike Rentals",
       y = "Density")

ggRentalVolByHoliday

#The exploratory data analysis showed us that bike rentals were lower on holidays compared to non-holidays.




##Model with gradient boosting machine in R

#The following tutorial will use a gradient boosting machine (GBM) to figure out what drives bike rental behavior.

#GBM is unique compared to other decision tree algorithms because it builds models sequentially with higher weights given to those cases that were poorly predicted in previous models, thus improving accuracy incrementally instead of simply taking an average of all models like a random forest algorithm would. By reducing the error iteratively to produce what will become the final model, GBM is an efficient and powerful algorithm for classification and regression problems.

#Implementing GBM in R allows for a nice selection of exploratory plots including parameter contribution, and partial dependence plots which provide a visual representation of the effect across values of a feature in the model.

# packages --------------------------------------------------------------
library(rsample)
library(caret)
library(ggthemes)
library(scales)
library(wesanderson)
library(tidyverse)

install.packages("gbm")
library(gbm)
install.packages("Metrics")
library(Metrics)
library(here)

# the data
BikeData %>% glimpse()

#Variables of interest
#outcome variable cnt (the ‘count of total rental bikes including both casual and registered’)
#the 11 features we want to help explain.
BikeDataModel <- BikeData %>% dplyr::select(cnt,
                                            season_fct,
                                            yr_fct,
                                            month_fct,
                                            holiday_fct,
                                            weekday_fct,
                                            workingday_fct,
                                            weathersit_fct,
                                            temp,
                                            atemp,
                                            hum,
                                            windspeed)

BikeDataModel %>% dplyr::glimpse()

#Exporting Data to an Excel Spreadsheet
library(xlsx)
write.xlsx(BikeDataModel, "BikeDataModel.xlsx")


# Step 3: Create train/test set

#The testing and training split
#We want to build training and testing data sets that are representative of the original bike data set. 
#To achieve this, we will randomly select observations for two subsets of data. 
#We’ll also specify the sampling process, so we get an equal proportion of bike rentals (cnt) in our BikeTest and BikeTrain data sets.
#(we’ll randomize our data into training and test sets with a 70% / 30% split).

#The BikeTrain has the data we will use to build a model and demonstrate it’s performance.
#The BikeTest is for testing the model on data our model hasn’t seen (i.e. the BikeTest data).

#Having testing and training data sets allows us to:
# 1) build and select the best model, and 
# 2) then assess the final model’s performance using ‘fresh’ data.

set.seed(123)
BikeSplit <- initial_split(BikeDataModel, prop = .7)
BikeTrain <- training(BikeSplit)
BikeTest  <- testing(BikeSplit)



# Step 4: Build the model

# model
set.seed(123)
bike_fit_1 <- gbm::gbm(cnt ~., 
                       # the formula for the model (recall that the period means, "all 
                       # variables in the data set")
                       data = BikeTrain, 
                       # data set
                       verbose = TRUE, 
                       # Logical indicating whether or not to print
                       # out progress and performance indicators
                       shrinkage = 0.01, 
                       # a shrinkage parameter applied to each tree in the expansion. 
                       # Also known as the learning rate or step-size reduction; 0.001 
                       # to 0.1 usually work, but a smaller learning rate typically 
                       # requires more trees.
                       interaction.depth = 3, 
                       # Integer specifying the maximum depth of each tree (i.e., the 
                       # highest level of variable interactions allowed). A value of 1 
                       # implies an additive model, a value of 2 implies a model with up
                       #  to 2-way interactions
                       n.minobsinnode = 5,
                       # Integer specifying the minimum number of observations in the 
                       # terminal nodes of the trees. Note that this is the actual number 
                       # of observations, not the total weight.
                       n.trees = 5000, 
                       # Integer specifying the total number of trees to fit. This is 
                       # equivalent to the number of iterations and the number of basis 
                       # functions in the additive expansion.
                       cv.folds = 10
                       # Number of cross-validation folds to perform. If cv.folds>1 then
                       # gbm, in addition to the usual fit, will perform a 
                       # cross-validation, calculate an estimate of generalization error
                       #  returned in cv.error
)

#Cross-fold validation randomly divides our training data into k sets that are relatively equal in size. 
#Our model will be fit using all the sets with the exclusion of the first fold. The model error of the fit is estimated with the hold-out sets.
#Each set is used to measure the model error and an average is calculated across the various sets.


#Store and explore
#use the gbm::gbm.perf() function to see the the error rate at each number of learners.
# model performance
perf_gbm1 = gbm.perf(bike_fit_1, method = "cv")

#In the visualization plot we can see that the blue line represents the optimal number of trees with our cross validation (cv). 
#GBM can be sensitive to over-fitting, so using the method = "cv" in our estimate protects against this.
perf_gbm1
#The optimal number of trees is perf_gbm1 = 1715



# Step 5: Make prediction

#Make predictions
#Now we can predict our bike rentals using the predict() function with 
#our test set and the optimal number of trees based on our perf.gbm1 estimate.

bike_prediction_1 <- stats::predict(
  # the model from above
  object = bike_fit_1, 
  # the testing data
  newdata = BikeTest,
  # this is the number we calculated above
  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = BikeTest$cnt, 
                           predicted = bike_prediction_1)
print(rmse_fit1)
#RMSE = The root mean squared error (RMSE) is used to measure the prediction error in our model(s).
#Lower values of RMSE indicate better fit. 
#RMSE is a good measure of how accurately the model predicts the response, 
#and it is the most important criterion for fit if the main purpose of the model is prediction. 
summary(BikeData$cnt)

#GBM offers partial dependency plots to explore the correlations between a feature in our model and our outcome. 
#Ambient temperature is associated with increased numbers of bike rentals until close to 35 degrees when riders tend to be less likely to rent a bike.
gbm::plot.gbm(bike_fit_1, i.var = 9)

#Similarly we can look at the interaction of two features on bike rentals. 
#Below we can see that riders are more likely to rent a bike after Monday, despite wind speed.
gbm::plot.gbm(bike_fit_1, i.var = c(5, 11))


#We can visualize the impact of different features on predicting bike rentals using the relative influence provided by GBM. 
# summarize model
BikeEffects <- tibble::as_tibble(gbm::summary.gbm(bike_fit_1, 
                                                  plotit = FALSE))
BikeEffects %>% utils::head()

#We can then plot the top ten features by impact using ggpplot and our new data frame containing our model summary (BikeEffects).
# plot effects
BikeEffects %>% 
  # arrange descending to get the top influencers
  dplyr::arrange(desc(rel.inf)) %>%
  # sort to top 10
  dplyr::top_n(10) %>%
  # plot these data using columns
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  geom_col() +
  # flip
  coord_flip() +
  # format
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  xlab('Features') +
  ylab('Relative Influence') +
  ggtitle("Top 10 Drivers of Bike Rentals")



# Step 6: Measure performance

#We can visualize the distribution of our predicted compared with 
#actual bike rentals by predicting these values and plotting the difference.

# Predicted bike rentals
BikeTest$predicted <- base::as.integer(predict(bike_fit_1, 
                                               newdata = BikeTest, 
                                               n.trees = perf_gbm1))

# plot predicted vs actual
ggplot(BikeTest) +
  geom_point(aes(y = predicted, 
                 x = cnt, 
                 color = predicted - cnt), alpha = 0.7) +
  # add theme
  theme_fivethirtyeight() +
  # strip text
  theme(axis.title = element_text()) + 
  # add format to labels
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  # add axes/legend titles
  scale_color_continuous(name = "Predicted - Actual", 
                         labels = comma) +
  ylab('Predicted Bike Rentals') +
  xlab('Actual Bike Rentals') +
  ggtitle('Predicted vs Actual Bike Rentals') 

#We can see that our model did a fairly good job predicting bike rentals.


# Step 7: Conclusion 

#What did we learn?
#We learned the ambient temperature is the largest influencer for predicting bike rentals and that rental numbers go down when the temperature reaches ~35 degrees. 
#We can also see holiday (or non-holiday) was not much of an influencer for predicting bike rentals.


