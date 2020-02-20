getwd()
setwd("C:/R programming")

###LOADING DATASET###
weather_data<- read.csv("weatherAUS.csv")
View(weather_data)

dim(weather_data)
str(weather_data)

###CHECKING NA VALUES###
table(is.na(weather_data))  ##316559 NA values##

###REPLACING NA VALUES WITH MEAN VALUES###
for (i in 3:24) {
  weather_data[is.na(weather_data[,i]),i] <- mean(weather_data[,i],na.rm = TRUE)
}

colSums(is.na(weather_data))  ###ONLY FACTOR NA VALUES LEFT, NUMERICS REPLACED WITH MEAN

###OMITTING FACTOR NA values###
weather_data1 <- na.omit(weather_data) 
colSums(is.na(weather_data1))

str(weather_data1)
View(weather_data1)

###REMMOVING RISK_MM AS SPECIFIED IN THE QUESTION###
weather_data1$RISK_MM <- NULL

#####CHANGE LEVELS OF RAINTODAY AND RAINTOMORROW INTO 0 AND 1, 
weather_data1$RainToday <- (ifelse(weather_data1$RainToday == "Yes",1,0))
str(weather_data1$RainToday)
weather_data1$RainToday <- as.factor(weather_data1$RainToday)
str(weather_data1$RainToday)
View(weather_data1)

weather_data1$RainTomorrow<- (ifelse(weather_data1$RainTomorrow == "Yes",1,0))
str(weather_data1$RainTomorrow)
weather_data1$RainTomorrow <- as.factor(weather_data1$RainTomorrow)
str(weather_data1$RainTomorrow)
View(weather_data1)


###CHANGING DATE STRUCTURE###
weather_data1$Date<-  as.Date(weather_data1$Date)
str(weather_data1$Date)



####CORRELATION###
corr<- cor(weather_data1$MaxTemp, weather_data1$Evaporation) ### .45 -  low
corr1<- cor(weather_data1$MaxTemp, weather_data1$Sunshine)  ### .33 - low
corr2<- cor(weather_data1$Sunshine, weather_data1$Evaporation)  ### .29 - low
corr3<- cor(weather_data1$Humidity3pm, weather_data1$Evaporation) ### -.29 - low
corr4<- cor(weather_data1$MaxTemp, weather_data1$Humidity3pm) ### -.5 - medium
corr6<- cor(weather_data1$MaxTemp, weather_data1$Temp9am) ### .88 - high


###High correlation of max temp with both temp 9am and temp 3 pm####
######## Therefore We'll just use MaxTemp in the models###################



####VISUALISATIONS###

hist(weather_data1$MaxTemp)
hist(weather_data1$MinTemp)
hist(weather_data1$Sunshine)

corr7<- cor(weather_data1[,c(4,20,21)])
corr7
corrplot(corr7)

corr5<- cor(weather_data1[,c(4,6,7,14,15,20,21)])
install.packages("corrplot")
library(corrplot)
corrplot(corr, method = "circle")




####dividing dataset into 2 parts - 9 am variables and 3 pm variables######

wd9am <-subset(weather_data1, select= -c(11,13,15,17,19,21))
View(wd9am)

wd3pm <-subset(weather_data1, select= -c(10,12,14,16,18,20))
View(wd3pm)


########## 1. 9am VARIABLES ##############

###vARIABLE SELECTION###
names(wd9am)

##use Location, MaxTemp, Evaporation, Sunshine, WindGustDir, WindGustSpeed, Humidity9am, Cloud9am ,RainToday, RainTomorrow ##


model1 <- wd9am[, c("Location", "MaxTemp", "Evaporation" , 
                 "Sunshine" , "WindGustDir" , "WindGustSpeed" , "Humidity9am" , 
                 "Cloud9am" ,  "RainToday","RainTomorrow" )]
model1

###divide into training nd testing####
install.packages("bit64")
install.packages("caret") #to randomly divide data into training nd testing
install.packages("sqldf")

library(bit64)
library(data.table)
library(sqldf)
library(caret)
library(plyr)

###splitting 80% data into training###
ind1 <- as.vector(createDataPartition(model1$RainTomorrow, p=0.8, list = FALSE)) #use the model dataset not the original dataset#
ind1
length(ind1)


data_train1 <- as.data.frame(model1[ind1,]) 
data_train1
View(data_train1)

data_test1<- as.data.frame(model1[-ind1,]) 
data_test1
View(data_test1)

dim(data_test1)
dim(data_train1)


######FITTING MODEL#####
log_model1<- glm(RainTomorrow ~.,data = data_train1, family = "binomial")
log_model1
summary(log_model1)

fitted_prob1 <- predict(log_model1,
                      newdata = subset(data_test1, select=c(1:10)), type = "response")
fitted_prob1

fitted_results1 <- ifelse(fitted_prob1> 0.4,1,0)  
fitted_results1

###checking accuracy
Error1<- mean(fitted_results1 != data_test1$RainTomorrow)
print(paste('Acurracy is', 1- Error1))

##ROC#####
install.packages("ROCR")
library(ROCR)


ROCRpred1 <- prediction(fitted_results1, data_test1$RainTomorrow)
ROCRpred1

ROCRperf1  <- performance(ROCRpred1 , 'tpr','fpr')
ROCRperf1
plot(ROCRperf1 , colorize=TRUE)


###check3- auc
auc<- performance(ROCRpred1, measure= "auc")
auc
auc<- auc@y.values[{1}]
auc

## for threshold .4, the accuracy is 82% and auc is .71
## for threshold .5, the accuracy is 81% and auc is .6
## for threshold .6, the accuracy is 81% and auc is .67

###### therefore we'll take 0.4 as the threshold.



#####model according to 3 pm variables####



wd3pm <-subset(weather_data1, select= -c(10,12,14,16,18,20))
View(wd3pm)

###vARIABLE SELECTION###
names(wd3pm)



##use Location, MaxTemp, Evaporation, Sunshine, WindGustDir, WindGustSpeed, Humidity3pm, Cloud3pm ,RainToday, RainTomorrow ##

model2<- wd3pm[, c( "Location" , "MaxTemp", "Evaporation" , 
                    "Sunshine" , "WindGustDir" , "WindGustSpeed" , "Humidity3pm" , 
                    "Cloud3pm" ,  "RainToday","RainTomorrow" )]
model2

####DIVIDE INTO TRAINING AND TESTING DATA####
install.packages("bit64")
install.packages("caret") #to randomly divide data into training nd testing
install.packages("sqldf")

library(bit64)
library(data.table)
library(sqldf)
library(caret)
library(plyr)

###splitting 80% data into training###
ind2<- as.vector(createDataPartition(model2$RainTomorrow, p=0.8, list = FALSE)) #use the model dataset not the original dataset#
ind2
length(ind2)


data_train2<- as.data.frame(model2[ind2,]) #model,not data
data_train2
View(data_train2)

data_test2<- as.data.frame(model2[-ind2,]) #model,not data
data_test2
View(data_test2)

dim(data_test2)
dim(data_train2)



log_model2<- glm(RainTomorrow~.,data = data_train2, family = "binomial")
log_model2
summary(log_model2)

fitted_prob2<- predict(log_model2,
                      newdata = subset(data_test2, select=c(1:10)), type = "response")
fitted_prob2

fitted_results2<- ifelse(fitted_prob2> 0.5,1,0) 
fitted_results2  

###CHECKING ACCURACY###
Error2<- mean(fitted_results2 != data_test2$RainTomorrow)
print(paste('Acurracy is', 1- Error2))
####ACCURACY 84% FOR THRESHOLD= .4####

m,########ROC#####
install.packages("ROCR")
library(ROCR)


ROCRpred2<- prediction(fitted_results2, data_test2$RainTomorrow)
ROCRpred2

ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
ROCRperf2

plot(ROCRperf2, colorize=TRUE)



######check3- AUC#####
auc2<- performance(ROCRpred2, measure= "auc")
auc2

auc2<- auc2@y.values[{1}]
auc2



## for threshold .4, the accuracy is 84% and auc is .75
## for threshold .5, the accuracy is 84% and auc is .72
## for threshold .6, the accuracy is 83% and auc is .67

###### therefore we'll take 0.4 as the threshold.


