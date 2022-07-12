#Class Project

# 1. Access the Data Set

getwd()
housing <- read.csv("housing.csv")
# > housing <- read.csv("housing.csv")

housing$ocean_proximity <- as.factor(housing$ocean_proximity)
levels(housing$ocean_proximity)
# > housing$ocean_proximity <- as.factor(housing$ocean_proximity)
# > levels(housing$ocean_proximity)
# [1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"



# 2. EDA and Data Visualization
attach(housing)
head(housing)
tail(housing)
# > head(housing)
#   longitude latitude housing_median_age total_rooms total_bedrooms population households median_income median_house_value ocean_proximity
# 1   -122.23    37.88                 41         880            129        322        126        8.3252             452600        NEAR BAY
# 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014             358500        NEAR BAY
# 3   -122.24    37.85                 52        1467            190        496        177        7.2574             352100        NEAR BAY
# 4   -122.25    37.85                 52        1274            235        558        219        5.6431             341300        NEAR BAY
# 5   -122.25    37.85                 52        1627            280        565        259        3.8462             342200        NEAR BAY
# 6   -122.25    37.85                 52         919            213        413        193        4.0368             269700        NEAR BAY
# > tail(housing)
# longitude latitude housing_median_age total_rooms total_bedrooms population households median_income median_house_value ocean_proximity
# 20635   -121.56    39.27                 28        2332            395       1041        344        3.7125             116800          INLAND
# 20636   -121.09    39.48                 25        1665            374        845        330        1.5603              78100          INLAND
# 20637   -121.21    39.49                 18         697            150        356        114        2.5568              77100          INLAND
# 20638   -121.22    39.43                 17        2254            485       1007        433        1.7000              92300          INLAND
# 20639   -121.32    39.43                 18        1860            409        741        349        1.8672              84700          INLAND
# 20640   -121.24    39.37                 16        2785            616       1387        530        2.3886              89400          INLAND

summary(housing)
# > summary(housing)
# longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population      households     median_income     median_house_value   ocean_proximity
# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001                      
#                                                                     NA's   :207 

#All columns are numeric outside of Ocean Proximity.
#Total Bedrooms has NA's and must be dealt with

cor(housing[,-10])
# > cor(housing[,-10])
#                      longitude    latitude housing_median_age total_rooms total_bedrooms   population  households median_income median_house_value
# longitude           1.00000000 -0.92466443        -0.10819681  0.04456798             NA  0.099773223  0.05531009  -0.015175865        -0.04596662
# latitude           -0.92466443  1.00000000         0.01117267 -0.03609960             NA -0.108784747 -0.07103543  -0.079809127        -0.14416028
# housing_median_age -0.10819681  0.01117267         1.00000000 -0.36126220             NA -0.296244240 -0.30291601  -0.119033990         0.10562341
# total_rooms         0.04456798 -0.03609960        -0.36126220  1.00000000             NA  0.857125973  0.91848449   0.198049645         0.13415311
# total_bedrooms              NA          NA                 NA          NA              1           NA          NA            NA                 NA
# population          0.09977322 -0.10878475        -0.29624424  0.85712597             NA  1.000000000  0.90722227   0.004834346        -0.02464968
# households          0.05531009 -0.07103543        -0.30291601  0.91848449             NA  0.907222266  1.00000000   0.013033052         0.06584265
# median_income      -0.01517587 -0.07980913        -0.11903399  0.19804965             NA  0.004834346  0.01303305   1.000000000         0.68807521
# median_house_value -0.04596662 -0.14416028         0.10562341  0.13415311             NA -0.024649679  0.06584265   0.688075208         1.00000000


hist(longitude,col = "blue")
hist(latitude,col = "blue")
hist(housing_median_age,col = "blue")
hist(total_rooms,col = "blue",xlim = c(0,21000))
hist(total_bedrooms,col = "blue", xlim = c(0,4000))
hist(population,col = "blue", xlim = c(0,11000))
hist(households,col = "blue", xlim = c(0,4000))
hist(median_income,col = "blue")
hist(median_house_value,col = "blue")

# > hist(longitude,col = "blue")
# > hist(latitude,col = "blue")
# > hist(housing_median_age,col = "blue")
# > hist(total_rooms,col = "blue",xlim = c(0,21000))
# > hist(total_bedrooms,col = "blue", xlim = c(0,4000))
# > hist(population,col = "blue", xlim = c(0,11000))
# > hist(households,col = "blue", xlim = c(0,4000))
# > hist(median_income,col = "blue")
# > hist(median_house_value,col = "blue")

boxplot(longitude, col= "gold")
boxplot(latitude, col= "gold")
boxplot(housing_median_age, col= "gold")
boxplot(total_rooms, col= "gold")
boxplot(total_bedrooms, col= "gold")
boxplot(population, col= "gold")
boxplot(households, col= "gold")
boxplot(median_income, col= "gold")
boxplot(median_house_value, col= "gold")

# > boxplot(longitude, col= "gold")
# > boxplot(latitude, col= "gold")
# > boxplot(housing_median_age, col= "gold")
# > boxplot(total_rooms, col= "gold")
# > boxplot(total_bedrooms, col= "gold")
# > boxplot(population, col= "gold")
# > boxplot(households, col= "gold")
# > boxplot(median_income, col= "gold")
# > boxplot(median_house_value, col= "gold")

boxplot(housing_median_age~ocean_proximity, col = "green")
boxplot(median_income~ocean_proximity, col = "green")
boxplot(median_house_value~ocean_proximity, col = "green")

# > boxplot(housing_median_age~ocean_proximity, col = "green")
# > boxplot(median_income~ocean_proximity, col = "green")
# > boxplot(median_house_value~ocean_proximity, col = "green")


# 3 Data Tranformation 

install.packages("Hmisc")
library(Hmisc)
housing$total_bedrooms<-impute(total_bedrooms,median)
summary(housing$total_bedrooms)

# > housing$total_bedrooms<-impute(total_bedrooms,median)
# > summary(total_bedrooms)
# 
# 207 values imputed to 435 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   297.0   435.0   536.8   643.2  6445.0 

class(housing$total_bedrooms)
# > class(housing$total_bedrooms)
# [1] "impute"
housing$total_bedrooms <- as.numeric(housing$total_bedrooms)
class(housing$total_bedrooms)



install.packages('fastDummies')
library(fastDummies)
levels(housing$ocean_proximity)
cat <- dummy_cols(housing, select_columns = "ocean_proximity")
cat <- subset(cat, select = -c(1:10))
head(cat)
cat <- rename(cat, INLAND = ocean_proximity_INLAND, ISLAND = ocean_proximity_ISLAND)
cat2 <- rename(cat,c("<1H OCEAN" = "ocean_proximity_<1H OCEAN"))
cat2 <- rename(cat2, c("NEAR BAY" = "ocean_proximity_NEAR BAY"))
cat2 <- rename(cat2, c("NEAR OCEAN" = "ocean_proximity_NEAR OCEAN"))
names(cat2)
# > levels(housing$ocean_proximity)
# [1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"
# > cat <- dummy_cols(housing, select_columns = "ocean_proximity")
# > cat <- subset(cat, select = -c(1:10))
# > head(cat)
# ocean_proximity_<1H OCEAN ocean_proximity_INLAND ocean_proximity_ISLAND ocean_proximity_NEAR BAY ocean_proximity_NEAR OCEAN
# 1                         0                      0                      0                        1                          0
# 2                         0                      0                      0                        1                          0
# 3                         0                      0                      0                        1                          0
# 4                         0                      0                      0                        1                          0
# 5                         0                      0                      0                        1                          0
# 6                         0                      0                      0                        1                          0
# > cat <- rename(cat, INLAND = ocean_proximity_INLAND, ISLAND = ocean_proximity_ISLAND)
# > cat2 <- rename(cat,c("<1H OCEAN" = "ocean_proximity_<1H OCEAN"))
# > cat2 <- rename(cat2, c("NEAR BAY" = "ocean_proximity_NEAR BAY"))
# > cat2 <- rename(cat2, c("NEAR OCEAN" = "ocean_proximity_NEAR OCEAN"))
# > names(cat2)
# [1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

housing2 <- subset(housing, select = -(ocean_proximity))
# > housing2 <- subset(housing, select = -(ocean_proximity))
detach(housing)
# > detach(housing)

housing2$total_bedrooms <- mean(housing2$total_bedrooms)
housing2$total_rooms <- mean(housing2$total_rooms)
# > housing2$total_bedrooms <- mean(housing2$total_bedrooms)
# > housing2$total_rooms <- mean(housing2$total_rooms)


install.packages("dplyr")
library(dplyr)

housing2 <- rename(housing2, mean_number_bedrooms = total_bedrooms, mean_number_rooms = total_rooms)
?rename
# > rename(housing2, mean_number_bedrooms = total_bedrooms, mean_number_rooms = total_rooms)
# longitude latitude housing_median_age mean_number_rooms mean_number_bedrooms population households median_income median_house_value
# 1     -122.23    37.88                 41          2635.763             536.8389        322        126        8.3252             452600
# 2     -122.22    37.86                 21          2635.763             536.8389       2401       1138        8.3014             358500
# 3     -122.24    37.85                 52          2635.763             536.8389        496        177        7.2574             352100
# 4     -122.25    37.85                 52          2635.763             536.8389        558        219        5.6431             341300
# 5     -122.25    37.85                 52          2635.763             536.8389        565        259        3.8462             342200
# 6     -122.25    37.85                 52          2635.763             536.8389        413        193        4.0368             269700
summary(housing2)

housing3 <- as.data.frame(scale(housing2[,-9]))
head(housing3)
# > head(housing3)
# longitude latitude housing_median_age mean_number_rooms mean_number_bedrooms population households median_income
# 1 -1.327803 1.052523          0.9821189         0.9999758                  NaN -0.9744050 -0.9770092    2.34470896
# 2 -1.322812 1.043159         -0.6070042         0.9999758                  NaN  0.8614180  1.6699206    2.33218146
# 3 -1.332794 1.038478          1.8561366         0.9999758                  NaN -0.8207575 -0.8436165    1.78265622
# 4 -1.337785 1.038478          1.8561366         0.9999758                  NaN -0.7660095 -0.7337637    0.93294491
# 5 -1.337785 1.038478          1.8561366         0.9999758                  NaN -0.7598283 -0.6291419   -0.01288068
# 6 -1.337785 1.038478          1.8561366         0.9999758                  NaN -0.8940491 -0.8017678    0.08744452

#Because it did not properly scale bedrooms I will manually do it with the same values as rooms

housing3$mean_number_bedrooms[is.nan(housing3$mean_number_bedrooms)] <- 0.9999758  
head(housing3)
# > housing3$mean_number_bedrooms[is.nan(housing3$mean_number_bedrooms)] <- 0.9999758  
# > head(housing3)
# longitude latitude housing_median_age mean_number_rooms mean_number_bedrooms population households median_income
# 1 -1.327803 1.052523          0.9821189         0.9999758            0.9999758 -0.9744050 -0.9770092    2.34470896
# 2 -1.322812 1.043159         -0.6070042         0.9999758            0.9999758  0.8614180  1.6699206    2.33218146
# 3 -1.332794 1.038478          1.8561366         0.9999758            0.9999758 -0.8207575 -0.8436165    1.78265622
# 4 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.7660095 -0.7337637    0.93294491
# 5 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.7598283 -0.6291419   -0.01288068
# 6 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.8940491 -0.8017678    0.08744452


x <- housing2[,9, drop=FALSE]
# > x <- housing2[,9, drop=FALSE]

cleaned_housing <- cbind(cat2,housing3)
cleaned_housing <- cbind(cleaned_housing, x)
head(cleaned_housing) 
# > cleaned_housing <- cbind(cat2,housing3)
# > cleaned_housing <- cbind(cleaned_housing, x)
# > head(cleaned_housing)              
# <1H OCEAN INLAND ISLAND NEAR BAY NEAR OCEAN longitude latitude housing_median_age mean_number_rooms mean_number_bedrooms population households median_income median_house_value
# 1         0      0      0        1          0 -1.327803 1.052523          0.9821189         0.9999758            0.9999758 -0.9744050 -0.9770092    2.34470896             452600
# 2         0      0      0        1          0 -1.322812 1.043159         -0.6070042         0.9999758            0.9999758  0.8614180  1.6699206    2.33218146             358500
# 3         0      0      0        1          0 -1.332794 1.038478          1.8561366         0.9999758            0.9999758 -0.8207575 -0.8436165    1.78265622             352100
# 4         0      0      0        1          0 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.7660095 -0.7337637    0.93294491             341300
# 5         0      0      0        1          0 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.7598283 -0.6291419   -0.01288068             342200
# 6         0      0      0        1          0 -1.337785 1.038478          1.8561366         0.9999758            0.9999758 -0.8940491 -0.8017678    0.08744452             269700




#4 Create training and Test Sets

n <- nrow(cleaned_housing)  
ntrain <- round(n*0.7)    
set.seed(314)   
tindex <- sample(n, ntrain) 

train <- cleaned_housing[tindex,]  
test <- cleaned_housing[-tindex,]  



#5 Supervised Machine Learning - Regression

train_x <- train[,-14]
train_y <- train[,14]

# > train_x <- train[,-14]
# > train_y <- train[,14]
class(train_x)
class(train_y)
# > class(train_x)
# [1] "data.frame"
# > class(train_y)
# [1] "numeric"

install.packages("randomForest")
library(randomForest)

rf = randomForest(x=train_x, y=train_y , ntree=500, importance=TRUE)
names(rf)
# > rf = randomForest(x=train_x, y=train_y , ntree=500, importance=TRUE)
# > names(rf)
# [1] "call"            "type"            "predicted"       "mse"             "rsq"             "oob.times"       "importance"      "importanceSD"    "localImportance"
# [10] "proximity"       "ntree"           "mtry"            "forest"          "coefs"           "y"               "test"            "inbag" 


# 6. Evaluating Model Performance 

sqrt(rf$mse[length(rf$mse)])
# > sqrt(rf$mse[length(rf$mse)])
# [1] 50932.33

test_x <- test[,-14]
test_y <- test[,14]
# > test_x <- test[,-14]
# > test_y <- test[,14]

prediction <- predict(rf, newdata = test_x, type = "class")
# > prediction <- predict(rf, newdata = test_x, type = "class")


rmse <- function(y_hat, y)
{
  return(sqrt(mean((y_hat-y)^2)))
}

rmse_train <- rmse(predict(rf),train_y)
rmse_train

rmse_test <- rmse(predict(rf, newdata=test_x), 
                  test_y)
rmse_test

# > rmse <- function(y_hat, y)
#   + {
#     +   return(sqrt(mean((y_hat-y)^2)))
#     + }
# > rmse_train <- rmse(predict(rf),train_y)
# > rmse_train
# [1] 50932.33
# > rmse_test <- rmse(predict(rf, newdata=test_x), 
#                     +                   test_y)
# > rmse_tes
# > rmse_test
# [1] 49343.29

# Our Test RSME and Train RSME are very similar suggesting our model makes a good prediction 

varImpPlot(rf)
# > varImpPlot(rf)

#From our VARIMPPLOT we can see Median Income is the most important variable across the board. 
#The changes in the other variables may be due to our dummy coding of ocean proximity 
