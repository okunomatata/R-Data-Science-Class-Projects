getwd()
install.packages("haven")
library(haven)
cc <- readRDS("cumulative.Rds")

summary(cc)
install.packages("tidyverse")
library("tidyverse")
install.packages("ggthemes")
library(ggthemes)
#?filter
#We decided as a group that we were going to filter all data before 2008
subset = cc %>% filter(year >= 2008)


#I personally want to base my story around feature variables dealing with the 
#outcome of the 2016 presidential election
#See if it is possible to try and predict who someone voted for based on a set of features
subset2016 = subset %>% filter(year == 2016)
summary(subset2016)
not_all_na =  function(x)any(!is.na(x))
nona = subset2016 %>% select(where(not_all_na))
summary(nona)

#data1 <- as.data.frame(unclass(nona),                     # Convert all columns to factor
#                       stringsAsFactors = TRUE)
#This did not end up working
#We assume the weights have been factored into the data

#To do some EDA I wanted to create a correlation matrix or corrplot but realized I could not with catagorical variables
#cor(data1, method = "pearson", use = "complete.obs")
# cor(data2, method = "pearson", use = "complete.obs")
#install.packages("corrplot")
# library(corrplot)
# corrplot(data2, method = 'number'


#Removing redundant variables and variable we do not want to analyze
data2 <- select(nona,-c(case_id, cong, cong_up,weight, weight_cumulative, state, state_post, st_post, dist_up,cd,cd_up,
                          cd_post,cd_up_post,pid3,pid3_leaner, zipcode, starttime, weight_post,
                          intent_rep, intent_sen, intent_gov, voted_rep, voted_sen, voted_gov,sen1_icpsr,
                         sen2_icpsr,rep_icpsr,rep_current, dist_post, dist_up_post,county_fips,
                         tookpost,birthyr,hispanic,union_hh,voted_pres_12,intent_pres_party,voted_pres_party,
                         vv_party_gen,vv_party_prm,approval_sen1,approval_sen2,vv_turnout_pvm,
                         intent_gov_party,voted_gov_party,vv_turnout_gvm,intent_rep_party, voted_rep_party, intent_sen_party, 
                         voted_sen_party, intent_rep_chosen,intent_sen_chosen,intent_gov_chosen, voted_rep_chosen,
                         voted_gov_chosen,voted_sen_chosen,gov_current,sen2_current,sen1_current,voted_gov_chosen,
                         dist, pid7, intent_pres_16,vv_regstatus, citizen
                         ))
      

#I want to convert the certain columns to factor variables for better analysis                 
attach(data2)
names <- c('st','gender','race','age','educ','marstat','union','religion',"economy_retro",
           'newsint','approval_pres', 'approval_gov')
data2[,names] <- lapply(data2[,names],factor)

summary(data2)
detach(data2)


#Here we are going to filter the data set by only voteds of Clinton and Trump
filtered = filter(data2,voted_pres_16=='Hilary Clinton'| voted_pres_16 == "Donald Trump")
summary(filtered)
#Here we filter the data set by our west coast states
subset2 <- filter(filtered, st %in% c('CA', 'AZ','OR','NV','WA',"UT"))
summary(subset2)

#look up facet grid for histograms by category
#To make dummy encoding easier we are going to make sure the factor levels are labelled properly 
levels(subset2$gender) <- c("Male","Female")
levels(subset2$race) <- c("White","Black",'Hispanic','Asian','Native American', 'Mixed', 'Other', 'Middle Eastern')

levels(subset2$educ) <- c("No HS","High School Grad",'Some College','2-Year','4-Year','Post-Grad')
levels(subset2$marstat) <- c("Married","Separated","Divorced","Widowed","Single / Never Married", "Domestic Partnership")
levels(subset2$union) <- c("Yes, Currently","Yes, Formerly", "No Never")
levels(subset2$religion) <- c("Protestant", 'Roman Catholic', 'Mormon', 'Eastern or Greek Orthodox',  'Jewish', 'Muslim', 
                              'Buddhist', 'Hindu',  'Atheist',  'Agnostic','Nothing in Particular', 'Something Else')
levels(subset2$economy_retro) <- c('Gotten much better', 'Gotten better/ Somewhat Better', 'Stayed about the same', 
                                   'Gotten worse / Somewhat worse','Gotten much worse','Not Sure')
levels(subset2$newsint) <- c("Most of the time", 'Some of the time', 'Only now and then', 'Hardly at all', "Don't Know")
levels(subset2$approval_pres) <- c('Strongly Approve','Approve / Somewhat Approve' ,'Disapprove / Somewhat Disapprove' 
                                   ,'Strongly Disapprove', 'Never Heard / Not Sure','Neither Approve nor Disapprove')
levels(subset2$approval_gov) <- c('Strongly Approve','Approve / Somewhat Approve' ,'Disapprove / Somewhat Disapprove' 
                                   ,'Strongly Disapprove', 'Never Heard / Not Sure','Neither Approve nor Disapprove')

summary(subset2)

#I want to change our factor levels to ranges for certain variables
subset2$age <- as.numeric(subset2$age)
subset2$age <- cut(subset2$age, breaks= c(0,18,30,50,70,Inf))

summary(subset2$age)
summary(subset2)
class(subset2$age)
#I want to only analyze responses we can find useful so I am removing observation such as Prefer not to say, Don't Know, Etc. 
df <- subset(subset2, faminc!="Prefer not to say")
df <- subset(df,economy_retro!="Not Sure")
df <- subset(df,newsint!="Don't Know")
df <- subset(df,ideo5!="Not Sure")
summary(df)
#We will now complete the observations by removing any NA's. We will not have to remove any significant amount as most of the observations are complete. 

df1 <- na.omit(df)
#We lose less than 100 observations of over 6000 variables. 

#I am going to reset the levels of the factor. 
#This was challenging as the varables were already in preset brackets
summary(df1$faminc)
df1$faminc <- as.character(df1$faminc)
df1$income <- as.factor(ifelse(df1$faminc == "Less than 10k", "0-50K",
                             ifelse (df1$faminc == "10k - 20k", "0-50K",
                                     ifelse (df1$faminc == "20k - 30k", "0-50K",
                                             ifelse (df1$faminc == "30k - 40k", "0-50K",
                                                     ifelse (df1$faminc == "40k - 50k", "0-50K",
                                                             ifelse (df1$faminc == "50k - 60k", "51-100K",
                                                                     ifelse (df1$faminc == "60k - 70k", "51-100K",
                                                                             ifelse (df1$faminc == "70k - 80k", "51-100K",
                                                                                     ifelse (df1$faminc == "80k - 90k", "51-100K",
                                                                                             ifelse (df1$faminc == "90k - 100k", "51-100K", 
                                                                                                     ifelse (df1$faminc == "100k - 120k", "101-150K",
                                                                                                             ifelse (df1$faminc == "120k - 150k", "101-150K", "More than 150K")))))))))))))


df1$income <- factor(df1$income, levels=c("0-50K","51-100K", "101-150K", "More than 150K"))
summary(df1$income)
summary(df1)
df1 <- subset(df1, select =  -c(faminc))

#I will now do exploratory analysis using facet grids 

ggplot(data = df1, aes(x=gender, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.)+
  scale_y_log10()


#A feature  variable I would believe to show something interesting
ggplot(data = df1, aes(x=income, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()+ 
  scale_fill_manual(values=c("#1304ba", "#ba2204")) +
  labs(x="Income Bracket in Dollars", y= "Scaled count values", title="2016 Presidential Selection by Income Bracket", subtitle = "Divided by state") +
  guides(fill=guide_legend(title="2016 Presidential Selection"))+
  theme_solarized_2(base_size = 14, light=FALSE)




ggplot(data = df1, aes(x=newsint, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()


ggplot(data = df1, aes(x=age, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=economy_retro, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()+ 
  scale_fill_manual(values=c("#1304ba", "#ba2204")) +
  labs(x="In the past year, you would say the economy has...", y= "Scaled count values", title="2016 Presidential Selection by Retroactive Ecomonic Opinion", subtitle = "Divided by state") +
  guides(fill=guide_legend(title="2016 Presidential Selection"))+
  theme_clean()


#Here is a plot with some interesting insights above 

ggplot(data = df1, aes(x=union, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=race, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=marstat, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=employ, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=no_healthins, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=has_child, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=ownhome, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=no_milstat, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=religion, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()
ggplot(data = df1, aes(x=religion, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  scale_y_log10()
#Without state separation this time ^

ggplot(data = df1, aes(x=approval_pres, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()
#Another plot with interesting insights above 

ggplot(data = df1, aes(x=ideo5, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()+ 
  scale_fill_manual(values=c("#1304ba", "#ba2204")) +
  labs(x="Politically, I identify as", y= "Scaled count values", title="2016 Presidential Selection by Political Ideology", subtitle = "Political Ideology on a five point scale") +
  guides(fill=guide_legend(title="2016 Presidential Selection"))+
  theme_fivethirtyeight()
#Another plot of interest

ggplot(data = df1, aes(x=approval_rep, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()

ggplot(data = df1, aes(x=approval_gov, fill=voted_pres_16))+ geom_bar(position = "dodge") +
  facet_grid(st~.) +
  scale_y_log10()
#similar to pres

#I wanted to remove factor levels with no data
df1$voted_pres_16 <- factor(df1$voted_pres_16)
summary(df1$voted_pres_16)


#In order to create a linear model we need to separate our data into a test and training set
#The training set will be used to find the relationship between dependent and independent variables while the test set analyses the performance of the model.

set.seed(314)
df2 <-sample(2, nrow(df1), 
              replace = T, 
              prob = c(0.6, 0.4))
train<-df1[df2 == 1,]
test<-df1[df2 == 2,]



#I want to run a categorical multivariate regression to see which variables show significance
#Break up the data so it makes it easier to analyze

output <- glm(train$voted_pres_16~ train$economy_retro+ train$approval_pres+train$employ + train$gender, family = binomial)

output2 <-glm(train$voted_pres_16~train$income + train$age +train$newsint + train$no_healthins +train$race, family = binomial)

output3 <- glm(train$voted_pres_16~train$marstat+train$st+ train$has_child + train$educ, family = binomial)

output4 <- glm(train$voted_pres_16~train$union+train$ownhome + train$no_milstat + train$religion, family = binomial)



summary(output)
#we can remove  employment status 

summary(output2)
#We can remove news interest and race

summary(output3)
#Can remove has_child and state

summary(output4)
#Most here are significant 




#We create a new model after removing those variables 
output5 <- glm(train$voted_pres_16~ train$economy_retro+ train$approval_pres+ train$gender+train$income + train$age + train$no_healthins
                + train$marstat+train$educ+ train$union+train$ownhome + train$no_milstat + train$religion, family = binomial)

summary(output5)
 




#Analyze our model by making some predictions
p1<-predict(output5, train, 
             type = 'response')
head(p1)
head(train)



#I am going to create a confusion matrix to compare the number of true/false positives and negatives 
pre1<-ifelse(p1 > 0.5, 1, 0)
table<-table(Prediction = pre1, 
             Actual = train$voted_pres_16) 
table
#   Actual
# Prediction Hilary Clinton Donald Trump
# 0           2026          156
# 1             95         1310

#we can see there are 2026 True Negatives and 1310 True negatives


#We are going to see our misclassification error
1 - sum(diag(table)) / sum(table)
# > 1 - sum(diag(table)) / sum(table)
# [1] 0.06997491
# Our misclassification error comes out to 7% 



