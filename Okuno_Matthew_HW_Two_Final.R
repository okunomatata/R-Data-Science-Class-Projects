# Question 1
# Using the sqldf() function found in the sqldf package to select data from
# the CO2 data set, execute the SQL statement required to calculate the average
# value for uptake grouped by Type. Please use only SQL for the solution to this
# question
head(CO2)
#This is in base R
aggregate(x=CO2[,c("uptake")], 
          by=data.frame(CO2$Type), 
          FUN="mean")

install.packages("sqldf")
library(sqldf)
#Using SQL
sqldf("SELECT Type, AVG(uptake) FROM CO2 GROUP BY Type")


# This is in base R
# > head(CO2)
# Plant   Type  Treatment conc uptake
# 1   Qn1 Quebec nonchilled   95   16.0
# 2   Qn1 Quebec nonchilled  175   30.4
# 3   Qn1 Quebec nonchilled  250   34.8
# 4   Qn1 Quebec nonchilled  350   37.2
# 5   Qn1 Quebec nonchilled  500   35.3
# 6   Qn1 Quebec nonchilled  675   39.2
# > aggregate(x=CO2[,c("uptake")], 
#             +           by=data.frame(CO2$Type), 
#             +           FUN="mean")
# CO2.Type        x
# 1      Quebec 33.54286
# 2 Mississippi 20.88333
# 
#Using SQL
# > sqldf("SELECT Type, AVG(uptake) FROM CO2 GROUP BY Type")
# Type AVG(uptake)
# 1 Mississippi    20.88333
# 2      Quebec    33.54286





#Question 2
# Use the following vector assignment statements to provide data content for a
# new data frame:
#   Died.At <- c(22,40,72,41)
# Writer.At <- c(16, 18, 36, 36)
# First.Name <- c("John", "Edgar", "Walt", "Jane")
# Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
# Sex <- c("MALE", "MALE", "MALE", "FEMALE")
# Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-
# 03-26","1817-07-18")
# Write some data munging code to performing the following operations:
#   . Create a new data frame df with the above data for each of six columns.
# . Use the appropriate as.() function to cast the Sex variable to a factor.
# . The variable names are inconvenient so write R code to change them to:
#   age_at_death, age_as_writer, first_name, surname,
# gender, date_died [Hint: remember the names() function for data
#                    frames.]
# . Say "John Doe" died on his birthday, calculate and display the birthdate
# value based on the variables date_died and age_at_death


Died.At <- c(22,40,72,41)
Writer.At <- c(16, 18, 36, 36)
First.Name <- c("John", "Edgar", "Walt", "Jane")
Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
Sex <- c("MALE", "MALE", "MALE", "FEMALE")
Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-
03-26","1817-07-18")
?data.frame
df <- data.frame(Died.At , Writer.At, First.Name, Second.Name, Sex, Date.Of.Death)
df$Sex <- as.factor(df$Sex)
names(df) <- c("age_at_death" , "age_as_writer", "first_name", "surname",
               "gender", "date_died") 
df$date_died <- as.Date(df$date_died)
df$date_died[1] - df$age_at_death[1]
install.packages("lubridate")
library(lubridate)
bday = ymd(df$date_died[1]) - years (df$age_at_death[1])
print(paste("John Doe died on his birthday, he was born on ",bday))

# > Died.At <- c(22,40,72,41)
# > Writer.At <- c(16, 18, 36, 36)
# > First.Name <- c("John", "Edgar", "Walt", "Jane")
# > Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
# > Sex <- c("MALE", "MALE", "MALE", "FEMALE")
# > Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-
# + 03-26","1817-07-18")
# > ?data.frame
# > df <- data.frame(Died.At , Writer.At, First.Name, Second.Name, Sex, Date.Of.Death)
# > df$Sex <- as.factor(df$Sex)
# > names(df) <- c("age_at_death" , "age_as_writer", "first_name", "surname",
#                  +                "gender", "date_died") 
# > df$date_died <- as.Date(df$date_died)
# > df$date_died[1] - df$age_at_death[1]
# > bday = ymd(df$date_died[1]) - years (df$age_at_death[1])
# > print(paste("John Doe died on his birthday, he was born on ",bday))
# [1] "John Doe died on his birthday, he was born on  1993-05-10"







#Question 3
install.packages("reshape2")
library(reshape2)
product <- c("A", "B") 
height <- c(10,20) 
width <- c(5,10) 
weight <- c(2,NA) 
observations_wide <- data.frame(product, height, 
                                  width, weight) 
observations_wide 


long <- melt(data=observations_wide,id.vars="product", na.rm = TRUE)
long
?melt
sorted <- long[order(long$product),]
sorted

# > product <- c("A", "B") 
# > height <- c(10,20) 
# > width <- c(5,10) 
# > weight <- c(2,NA) 
# > observations_wide <- data.frame(product, height, 
#                                   +                                   width, weight) 
# > observations_wide 
# product height width weight
# 1       A     10     5      2
# 2       B     20    10     NA
# > long <- melt(data=observations_wide,id.vars="product", na.rm = TRUE)
# > long
# product variable value
# 1       A   height    10
# 2       B   height    20
# 3       A    width     5
# 4       B    width    10
# 5       A   weight     2
# > ?melt
# > sorted <- long[order(long$product),]
# > sorted
# product variable value
# 1       A   height    10
# 3       A    width     5
# 5       A   weight     2
# 2       B   height    20
# 4       B    width    10

# #Question 4
# Using the mtcars data set, write an R script that calculates the average miles per 
# gallon (mpg variable) by number of cylinders in the car (cyl variable). The 
# output should be the following: 
#   4        6        8  
# 26.66364 19.74286 15.10000 
# [HINT: one solution uses a loop function along with split()] 
data(mtcars)
install.packages("dpylr")
library(dplyr)
mean = mtcars |>
  group_by(cyl) |>
  summarize(mean_mpg=mean(mpg)) |>
  arrange(cyl)
mean
df <-as.data.frame(t(mean))
df
?split
#Looping method
y <- sapply(split(mtcars$mpg, mtcars$cyl), mean)  
y
 
# > mean = mtcars |>
#   +   group_by(cyl) |>
#   +   summarize(mean_mpg=mean(mpg)) |>
#   +   arrange(cyl)
# > mean
# # A tibble: 3 x 2
# cyl mean_mpg
# <dbl>    <dbl>
#   1     4     26.7
# 2     6     19.7
# 3     8     15.1
# > df <-as.data.frame(t(mean))
# > df
# V1       V2   V3
# cyl       4.00000  6.00000  8.0
# mean_mpg 26.66364 19.74286 15.1
# > ?split
# > #Looping method
#   > y <- sapply(split(mtcars$mpg, mtcars$cyl), mean)  
# > y
# 4        6        8 
# 26.66364 19.74286 15.10000 




# #Question 5
# Using the mtcars data set, write an R script to calculate the absolute difference 
# between the average horsepower of 4-cylinder cars and the average horsepower 
# of 8-cylinder cars. [Hint:  you may wish to use the base R abs() function for 
#                      calculating the absolute value of a number.] 

cylmean <- tapply(mtcars$hp,mtcars$cyl, mean)
cylmean
abs(cylmean[3] - cylmean[1])

# > cylmean <- tapply(mtcars$hp,mtcars$cyl, mean)
# > cylmean
# 4         6         8 
# 82.63636 122.28571 209.21429 
# > abs(cylmean[3] - cylmean[1])
# 8 
# 126.5779 



# Question 6
# Using the airquality data set, provide the R code that calculates mean value 
# of the Temp variable when the Month variable is equal to 6? 

data(airquality)
tapply(airquality$Temp, airquality$Month==6, mean, na.rm=TRUE)[[2]]
#another method
mean(airquality$Temp[airquality$Month==6])


# > data(airquality)
# > tapply(airquality$Temp, airquality$Month==6, mean, na.rm=TRUE)[[2]]
# [1] 79.1
# > #another method
#   > mean(airquality$Temp[airquality$Month==6])
# [1] 79.1




#Question 7 
# Write the dplyr code required to calculate the mean mpg for each 
# transmission type (0 = automatic, 1 = manual) using the am variable in the 
# mtcars data set. Sort the resulting list by mean mpg.  Use a single dplyr 
# statement with multiple pipes in the solution. The resulting output should be: 
#   
#   # A tibble: 2 x 2 
#   #    am mean_mpg 
#   # <dbl>    <dbl> 
#   #1    0     17.1 
#   #2    1     24.4 
#   
#   [Hint: considering using the following dplyr functions: group_by, 
#    summarize, and arrange].
install.packages("dplyr")
library(dplyr)
data(mtcars)
attach(mtcars)
mtcars %>% 
  group_by(am)%>%
  summarize(mean_mgp = mean(mpg))
detach(mtcars)

# > data(mtcars)
# > attach(mtcars)
# > mtcars %>% 
#   +   group_by(am)%>%
#   +   summarize(mean_mgp = mean(mpg))
# # A tibble: 2 x 2
# am mean_mgp
# <dbl>    <dbl>
#   1     0     17.1
# 2     1     24.4
# > detach(mtcars)










#Question 8
data(mtcars)
attach(mtcars)
install.packages("scatterplot3d")
library(scatterplot3d)
head(mtcars)
scatterplot3d(wt,disp,mpg,
              main = "Weight, Displacement and MPG Scatter Plot",
              col.axis = "green",
              col.grid = "red",
              pch=am,
              xlab = "Weight (1000's lbs)",
              ylab = "Displacment in Cubic In",
              zlab = "Miles Per Gallon"
              )

?mtcars
detach(mtcars)

# > library(scatterplot3d)
# > head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
# > scatterplot3d(wt,disp,mpg,
#                 +               main = "Weight, Displacement and MPG Scatter Plot",
#                 +               col.axis = "green",
#                 +               col.grid = "red",
#                 +               pch=am,
#                 +               xlab = "Weight (1000's lbs)",
#                 +               ylab = "Displacment in Cubic In",
#                 +               zlab = "Miles Per Gallon"
#                 +               )
# > ?mtcars
# > detach(mtcars)








# Question 9 
# Using the CO2 data set, produce a histogram for the uptake variable with the 
# following specifications:  
#   . Use 20 cells (aka buckets, breaks) for the plot 
# . Use the color name "cornflowerblue" for the plot 
# . Store the histogram in an object variable 
# . Extract from the object and display the break points (cell boundaries) 
# . Extract from the object and display the counts for each cell 

data(CO2)
attach(CO2)
object <- hist(uptake,col="cornflowerblue",breaks=20, main = "Uptake")
brk <- object$breaks
cnt <- object$counts
brk
cnt
detach(CO2)
 
# > data(CO2)
# > attach(CO2)
# > object <- hist(uptake,col="cornflowerblue",breaks=20, main = "Uptake")
# > brk <- object$breaks
# > cnt <- object$counts
# > brk
# [1]  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46
# > cnt
# [1] 1 1 6 5 5 5 7 3 1 2 5 3 7 5 6 3 8 5 4 2
# > detach(CO2)






# Question 10
# Provide the R code necessary to reproduce the boxplot data visualization below 
# using the mtcars data set. [HINT: make sure the width of the boxes represents 
#                             the number of observations in the groups] 
par(mar=c(5,6,4,1)+.1)
data(mtcars)
attach(mtcars)
head(mtcars)
boxplot(mpg~gear,
        main = "Car Milage Data",
        xlab = "Number of Forward Gears",
        ylab = "Miles Per Gallon",
        varwidth = TRUE)

# > par(mar=c(5,6,4,1)+.1)
# > data(mtcars)
# > attach(mtcars)
# > head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
# > boxplot(mpg~gear,
#           +         main = "Car Milage Data",
#           +         xlab = "Number of Forward Gears",
#           +         ylab = "Miles Per Gallon",
#           +         varwidth = TRUE)
