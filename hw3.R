#Question 1


install.packages("ISLR")
library(ISLR)
data(Auto)
pairs(Auto)
#We can see certain pairs that look pretty linear such as HP and Displacement
cor(Auto[sapply(Auto,is.numeric)])
#We see which variables exactly have the highest correlation which appears to be cylinders and displacement 
lm1 <- lm(Auto$mpg~Auto$horsepower)
summary(lm1)
# > summary(lm1)
# 
# Call:
#   lm(formula = x$mpg ~ x$horsepower)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.5710  -3.2592  -0.3435   2.7630  16.9240 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  39.935861   0.717499   55.66   <2e-16 ***
#   x$horsepower -0.157845   0.006446  -24.49   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.906 on 390 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
# F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

#We can see using the significance codes that there is a strong relationship between the predictor and response varable; our P value is nearly zero
#We can see due to the negative slope that there is a negative relationship between the two
plot(Auto$horsepower,Auto$mpg)
abline(lm1, lwd = 3,col = "red")



lm2 <- lm(Auto$mpg~Auto$weight)
summary(lm2)
# > summary(lm2)
# 
# Call:
#   lm(formula = Auto$mpg ~ Auto$weight)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.9736  -2.7556  -0.3358   2.1379  16.5194 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 46.216524   0.798673   57.87   <2e-16 ***
#   Auto$weight -0.007647   0.000258  -29.64   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.333 on 390 degrees of freedom
# Multiple R-squared:  0.6926,	Adjusted R-squared:  0.6918 
# F-statistic: 878.8 on 1 and 390 DF,  p-value: < 2.2e-16

#We see the R Squared is .69 which is a highly correlated. Because it is a negative slope there is a negative correlation
plot(Auto$weight,Auto$mpg)
abline(lm2, lwd = 3,col = "red")


x <- Auto


lm4 <- lm(mpg~horsepower + weight, data = x)

lm3 <- lm(Auto$mpg~Auto$horsepower + Auto$weight)
summary(lm3)

# > summary(lm3)
# 
# Call:
#   lm(formula = Auto$mpg ~ Auto$horsepower + Auto$weight)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.0762  -2.7340  -0.3312   2.1752  16.2601 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     45.6402108  0.7931958  57.540  < 2e-16 ***
#   Auto$horsepower -0.0473029  0.0110851  -4.267 2.49e-05 ***
#   Auto$weight     -0.0057942  0.0005023 -11.535  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.24 on 389 degrees of freedom
# Multiple R-squared:  0.7064,	Adjusted R-squared:  0.7049 
# F-statistic: 467.9 on 2 and 389 DF,  p-value: < 2.2e-16


#We see strong relationships with our R-squared being .7 
# Both of our predictors have a negative relationship as we can see in the slope

par(mfrow=c(2,2))
plot(lm3)
#Our residuals vs fitted graph suggests a good linear trend. Our points are spread well.
#For the most part our residuals are normally distributed. We might want to take a look at observation 321
#Our scale location graph looks good as our points appear randomly spread
#Cooks distance lines are barely visible indicating there isn't a single great outlier which is good


horsepower<- 98
weight <-2500
mpg = 45.64 -.047*horsepower-.0058*weight
print(mpg)
# > horsepower<- 98
# > weight <-2500
# > mpg = 45.64 -.047*horsepower-.0058*weight
# > print(mpg)
# [1] 26.534

predict(lm4, newdata =  data.frame(horsepower = 98, weight= 2500))
# > predict(lm4, newdata =  data.frame(horsepower = 98, weight= 2500))
# 1 
# 26.51914 
# newdata <- data.frame(horsepower = 98,weight=2500)
# predict.glm(lm4,newdata, type = "response")      







#Question 2
data(Auto)
summary(Auto)

mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), yes = 1, no = 0)
Auto <- data.frame(Auto, mpg01)
# > mpg01 <- ifelse( mpg > median(mpg), yes = 1, no = 0)
# > Auto <- data.frame(Auto, mpg01)


cor(Auto[,-9])
# > cor(Auto[,-9])
# mpg  cylinders displacement horsepower     weight acceleration       year     origin      mpg01
# mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442    0.4233285  0.5805410  0.5652088  0.8369392
# cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273   -0.5046834 -0.3456474 -0.5689316 -0.7591939
# displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944   -0.5438005 -0.3698552 -0.6145351 -0.7534766
# horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377   -0.6891955 -0.4163615 -0.4551715 -0.6670526
# weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000   -0.4168392 -0.3091199 -0.5850054 -0.7577566
# acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392    1.0000000  0.2903161  0.2127458  0.3468215
# year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199    0.2903161  1.0000000  0.1815277  0.4299042
# origin        0.5652088 -0.5689316   -0.6145351 -0.4551715 -0.5850054    0.2127458  0.1815277  1.0000000  0.5136984
# mpg01         0.8369392 -0.7591939   -0.7534766 -0.6670526 -0.7577566    0.3468215  0.4299042  0.5136984  1.0000000
#Correlation analysis without name column as can only run with numeric
#We can see a high association with MPG itself, Cylinders, HP, Weight and Displacement



set.seed(314)
df <-sample(2, nrow(Auto), 
             replace = T, 
             prob = c(0.6, 0.4))
train<-Auto[df == 1,]
test<-Auto[df == 2,]

# > set.seed(314)
# > df <-sample(2, nrow(Auto), 
#               +              replace = T, 
#               +              prob = c(0.6, 0.4))
# > train<-Auto[df == 1,]
# > test<-Auto[df == 2,]
# > 


output <-  glm(mpg01 ~ cylinders + weight + displacement + horsepower,
                data = train,
                family = "binomial")
# > output <-  glm(mpg01 ~ cylinders + weight + displacement + horsepower,
#                  +                 data = train,
#                  +                 family = binomial)
summary(output)
# > output
# 
# Call:  glm(formula = mpg01 ~ cylinders + weight + displacement + horsepower, 
#            family = binomial, data = train)
# 
# Coefficients:
#   (Intercept)     cylinders        weight  displacement    horsepower  
# 11.4487599     0.5488944    -0.0009747    -0.0323441    -0.0635397  
# 
# Degrees of Freedom: 231 Total (i.e. Null);  227 Residual
# Null Deviance:	    321.6 
# Residual Deviance: 118.1 	AIC: 128.1

output2 <-  predict(output, newdata =  test, type = "response")
output2
output3 <-  rep(0, length(output2))
output3[output2 > 0.5] <- 1
output3
mean(output3 !=test$mpg01)
# > output2 <-  predict(output, newdata =  test, type = "response")
# > output2
# 3           10           17           21           32           38           39           43           45           46           47           48           52           53 
# 6.581570e-04 3.374420e-06 3.632197e-01 8.759075e-01 8.558984e-01 8.945011e-02 4.245898e-05 2.721080e-06 1.801404e-06 2.991649e-02 8.997380e-01 5.231625e-02 9.902426e-01 9.812266e-01 
# 55           59           63           73           74           76           77           78           79           84           88           89           92           96 
# 9.953173e-01 9.656261e-01 3.985262e-05 6.636864e-04 1.754080e-03 3.524791e-04 4.391945e-01 9.208757e-01 7.911004e-01 9.638155e-01 1.876518e-04 1.396301e-03 1.704600e-05 1.525065e-08 
# 97           98          104          106          107          108          110          111          115          116          117          118          119          123 
# 2.375724e-05 9.544534e-02 1.013939e-05 1.449353e-05 1.233969e-05 1.377611e-01 9.003517e-01 8.652642e-01 9.274680e-01 1.712263e-04 1.266968e-07 9.985164e-01 9.536299e-01 5.371142e-01 
# 124          125          128          129          135          137          139          140          142          145          146          148          150          152 
# 3.118946e-01 2.784484e-05 1.252996e-01 4.976745e-02 1.579726e-02 1.048190e-03 2.434055e-04 6.460103e-04 9.542635e-01 9.981206e-01 9.941276e-01 9.804167e-01 7.638236e-01 9.924802e-01 
# 153          155          158          162          163          164          166          172          175          176          177          181          182          186 
# 1.476858e-01 2.203203e-01 1.207966e-04 2.158724e-02 1.437887e-02 9.443331e-02 5.935680e-02 6.403485e-01 5.348284e-01 9.878436e-01 1.665995e-01 4.551981e-01 9.962562e-01 9.629279e-01 
# 187          188          189          191          192          195          196          201          205          206          208          214          220          221 
# 9.506205e-01 8.852106e-04 3.157312e-04 9.334589e-05 1.150308e-01 1.843553e-01 9.963435e-01 1.438788e-01 9.890967e-01 9.744462e-01 4.721207e-01 1.757913e-04 7.952425e-01 9.895597e-01 
# 222          223          224          230          231          232          234          236          238          239          240          242          244          247 
# 8.930394e-04 2.885899e-02 4.554032e-04 3.214124e-06 3.225700e-05 1.536981e-06 9.749134e-01 9.716363e-01 9.887111e-01 9.600131e-01 9.868110e-01 7.526620e-01 7.042179e-01 9.972206e-01 
# 248          249          252          253          257          259          261          263          265          269          272          273          274          275 
# 9.882230e-01 9.941430e-01 1.946895e-03 5.486441e-02 9.688188e-02 6.324587e-02 4.509055e-02 1.390757e-03 2.776402e-03 8.006422e-01 3.211567e-01 6.403402e-01 7.838040e-01 6.577468e-01 
# 279          280          281          285          288          289          292          293          294          299          300          304          312          313 
# 9.868073e-01 9.832590e-01 3.919550e-02 5.734948e-02 2.926802e-04 1.162140e-03 1.403476e-02 1.035771e-04 9.876072e-01 7.282263e-04 8.120409e-01 9.918065e-01 9.813036e-01 9.915476e-01 
# 315          317          320          321          323          324          325          327          328          330          338          339          341          344 
# 6.742316e-01 1.752011e-01 9.255303e-01 8.288370e-01 9.907709e-01 3.095834e-01 9.910620e-01 9.955377e-01 9.587975e-01 9.904417e-01 9.669678e-01 8.195997e-01 5.496825e-01 9.966429e-01 
# 347          348          352          354          356          357          362          365          366          367          371          373          375          382 
# 9.857567e-01 9.921554e-01 9.872746e-01 9.680700e-01 9.631742e-01 9.566959e-01 2.913552e-01 3.070591e-03 4.253970e-01 2.119221e-01 8.919838e-01 5.929326e-01 9.738252e-01 9.711193e-01 
# 383          386          387          389          393          394 
# 9.893201e-01 2.745007e-01 1.118966e-01 6.652585e-02 7.175700e-01 9.940999e-01 
# > output3 <-  rep(0, length(output2))
# > output3[output2 > 0.5] <- 1
# > mean(output3)
# [1] 0.49375
# > output3
# [1] 0 0 0 1 1 0 0 0 0 0 1 0 1 1 1 1 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 0 0 0 0 0 1 0 1 1 0 0 1 1 0 0 0 0
# [89] 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 0 1 1 1 1 1 0 0 0 0 0 0 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 0 0 0 1 1
# > mean(output3 !=test$mpg01)
# [1] 0.10625
# > 




#Question 3
data(iris)
summary(iris)
set.seed(314)
kc <- kmeans(iris[,c(1,2)],3)
?kmeans
# > set.seed(314)
# > kc <- kmeans(iris[,c(1,2)],3)
kc$cluster
# > kc$cluster
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 2 1 2 1 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 1 2
# [89] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 1 1 1 2 1 1 1 1 1 1 2 2 1 1 1 1 2 1 2 1 2 1 1 2 2 1 1 1 1 1 2 2 1 1 1 2 1 1 1 2 1 1 1 2 1 1 2
kc$centers
# > kc$centers
# Sepal.Length Sepal.Width
# 1     6.812766    3.074468
# 2     5.773585    2.692453
# 3     5.006000    3.428000

plot(iris$Sepal.Length,iris$Sepal.Width,col=kc$cluster,pch=19,cex=1)
points(kc$centers,col=1:3,pch=3,cex=2,lwd=8)
# > plot(iris$Sepal.Length,iris$Sepal.Width,col=kc$cluster,pch=19,cex=1)
# > points(kc$centers,col=1:3,pch=3,cex=2,lwd=8)