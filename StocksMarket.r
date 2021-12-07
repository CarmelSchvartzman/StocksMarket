############################################################################################
#########   Machine Learning applied to the Stocks Market in R Lang ########################
############################################################################################
#  by Carmel Shvartzman




########################################################################################
########################################################################################
#  1.                   Introduction / executive summary
########################################################################################
########################################################################################


#In this project, different  Machine Learning  methods, such as Neural Networks, 
#Random Forests, Support Vector Machines and Naive Bayes, will
#be applied to the Stocks Market daily prices, in order to predict the movement of
#the stocks the next day. 

#The algorithms used here, will be applied to stocks from two important companies,
#Apple Inc and Visa Inc. 

# The steps taken in order to build this Machine Learning project, are depicted 
# in the present Report, which includes several sections: 

# At first, in the __Data Analysis Section__, after loading the stocks prices 
# from Yahoo Finance, an exploratory analysis of the data will be done, 
# to get insights that would help in the development of the Models. 
# Because we'll successively use data from different companies, specifically 
# Apple Inc and Visa Inc, we'll have to split this __Data Analysis Section__ twice,
# to show the data loaded. In this sections, which will be spread inside the models,
# the dataset will be analyzed, data exploration will be performed, and
# visualization of the data will be thoroughly rendered.
 

# Following the exploratory section, in the __Modeling Section__, six 
# different models will be developed, to attain better predictions.
# These models will start from two Support Vector Machines (SVM) models, which will use
# two different kernels, the Polynomial kernel and the Sigmoid Kernel, and, passing
# through a Naive Bayes algorithm, the models will progressively become more
# complicated, with the utilization of a Random Forests Model, and finally, 
# a Neural Network Model, developed using resilient back-propagation with weight
# backtracking.
 
 
# The algorithms developed on this project will use several Stocks Market trading
# technical analysis indicators, such as Reversal Candles and Moving Averages.
 
# While training the Machine Learning models, at least two formulas will be used:
# we'll use both a narrow number of the fields performing an univariant training,
# and we'll be using other formulas including all the predictors. 
 
# The predictors created will be normalized and scaled, to be sure that each
# variable contributes equally to the analysis. Afterwards, factorization will be
# performed, in order to comply with training requisites.

# The prices dataset will be partitioned in Train set and Test Validation,
# containing the required continuity between the yesterday-today-tomorrow candles,
# because this is what the training algorithms will try to research and reveal: 
# the patterns of continuity between consecutive candles. Therefore, we cannot 
# just take prices hazardously and insert them in both training set and test set.
# Two methods of data partitioning will be used: we'll perform split holdout method
# to test the Models, and also K-fold Cross Validation, taking different chunks of
# data to validate, but always continuous data.

# All along this process of building the models,  their corresponding Accuracies
# will be computed, and the misclassifications committed will also be assessed.
# We'll perform Z-value, P-value, and AIC Analysis of the results, and a ROC Curve
# will be output. In addition to the accuracy and missclassification
# calculations, Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and
# confusion matrix will be computed, for checking performance achievements. Also,
# the revenue money predicted return will be calculated for some of the models,
# as an example of how can be computed the income obtained by applying the
# algorithms.

# And after all the six models had been validated against the Validation datasets,
# a  __Results Section__, will present the modeling results and will discuss the
# model performance.

# Also, a __Conclusion Section__ will give a summary of this Report, its limitations
# and future work that could be done.

 


########################################################################################
########################################################################################
#  2.                 Methods/Analysis Section - Visa Inc. Case
########################################################################################
########################################################################################

                    

# This Section explains the process and techniques used, performs data exploration 
# and data visualization, summarizes the insights gained, and addresses the 
# necessity of performing data cleaning.
# After the analysis of the data, comes a thorough explanation on
# the modeling approaches to the issue.



##########################################################
# 2.1 Techniques for Data Loading and Preparation
##########################################################




# Software Packages Installation:
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
library(dplyr) 
library(kableExtra) 
library(tidyr) 
library(stringr) 
library(forcats) 
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(caret)
library(data.table)
library(tidyverse)
library(TTR)
library(lubridate)
library(quantmod)



##########################################################
# 2.1.1 Loading Visa Stocks prices from Yahoo Finance
##########################################################


#Here the Visa Stocks prices will be loaded and used for training and testing
#some of the Machine Learning models.


# Trying to download the Visa stocks data from Yahoo:
# the data will be intended to be downloaded 10 times:   
i <- 1
maxTries <- 10
while (i < maxTries) {
 
  try(
    getSymbols("V", 
         from ="2017-01-01", 
         to = "2020-12-31", 
         src =  "yahoo", 
         adjust =  TRUE)
  )
  if (exists("V")) {
    break
  }  
   i <- i + 1
}



# Create a dataframe with the downloaded data:
price <- as.data.frame(V)
price$Date <- as.Date(rownames(price))
head(price)
names(price) <- c("Open","High","Low","Close","Volume","Adjusted","Date")

# Discarding the columns that are not needed:
price <- price %>% select(c(Date,Open,High,Low,Close))
price<-xts(price[, -1, drop=FALSE],  order.by=price[,1])




##########################################################
# 2.2              Data Analysis Exploration & Modeling
##########################################################


# In this section, we analyze the data provided and the relations between the predictors.
 

##########################################################
# 2.2.1 Preliminar Observations 
##########################################################

# At this stage, we take a look at the loaded dataset:


# Prices dataset glimpse: 
nrow(price)
str(`price`)
class(price)
head(price) 
length(price) 
ncol(price)




#At first glance, we can see that the data.frame is in tidy format, which means that
# each row represents only one observation, being the column names the
# prices features.

##########################################################
# 2.2.2 Features Analysis 
##########################################################


##########################################################
# 2.2.2.1 - Analysis of the Stock Prices
##########################################################




# Let's see some general data on the prices:



summary(price$Close)



#The mean closing price of the Visa stock is 146.24, the Minimal price is 77.44,
#and the maximal closing price is 218.36 usd.


# Creating a custom Theme for be used on the graphs GGPLOT2:
# Creating a custom Theme to be used on graphs :
neuralTheme <- theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "aliceblue"), 
        text = element_text( face = "italic",
                            colour = "black", size = 8,
                            lineheight = 0.9,  hjust = 0.5,
                            vjust = 0.5, angle = 0, 
                            #margin = margin(), 
                            debug = FALSE),
        axis.title = element_text(colour = "gray" ))



#To get a first approach to the Technical Trading Analysis features, the following
#graph shows the Visa prices of May 2019, represented as Bar icons, which place
#greater emphasis on the closing price of the stock in relation to the previous 
#day closing prices.
barChart(price['2019-05'], theme = "white") + neuralTheme


#However, the candlestick charts place the highest importance of the close as it
#relates to the open of the same day, which can be seen on the following charts,
#which show the same data for Visa stocks, using Japanese Candles, and two
#different formats:
#Another two ways of seeing the Ratings distribution :
candleChart(price['2019-05'],multi.col=TRUE,theme="white") + neuralTheme
chartSeries(price['2019-05'], theme = "white")  + neuralTheme



#When representing longer periods of data, the candlesticks charts can become
#quite difficult to follow. This two charts show the prices for the entire period
#of stock prices loaded for Visa :

chartSeries(price, subset = "2019-03::",theme = "white") + neuralTheme
chart_Series(price, subset = "2019/") 


#As before, here we will take a look at the data, using some Technical Analysis
#Indicators, such as Bollinger Bands and Moving Average Convergence Divergence
#(MACD). The following plots represent these indicators applied to the data
#that we have loaded from Yahoo right now:

chartSeries(price, TA="addBBands(n=20)", subset="2019-01-21::2019-12-31")
candleChart(price, TA=c(addMACD(),addVo()), subset = '2019') + neuralTheme


#Another Technical Analysis indicator, which we'll use on this project, is
#the Moving Average indicator, that looks as follows, when applied to the 
#stocks from Visa:

price$EMA <- EMA(Cl(price), n = 50)
add_TA(price$EMA, on = 1, col = "purple")
plot(price)

##########################################################
# 2.2.3 Data Cleaning, Preparation and Partition
##########################################################

##########################################################
# 2.2.3.1 Creating the predictors to train and test the Models
##########################################################

  

#In this section, the predictors for the Models will be calculated, using the
#stocks prices of Visa, downloaded from Yahoo:

library(tidyverse)

# This predictor will express the current candle type:
todayCandle<-data.frame(ifelse(price$Close > price$Open,"BULL","BEAR"))
# After creating this field, more predictors will be created using this column:

# This predictor will express the previous candle type: For this purpose, 
# the "lag" function will be used:
yesterdayCandle<-data.frame(lag(todayCandle$Close,n=1))

# This predictor will express the next (future) candle type: the "lead"
# function will be used:
tomorrowCandle<-data.frame(lead(todayCandle$Close, n = 1))


#The algorithm used on this project will research the reversal candle known
#as "Doji": representing indecision in the market, this reversal candle
#means that the battle between sellers and buyers have not yet seem to commit in
#either direction.    
#There are several types of Doji candles, but for this research, we'll use only
#three of them:

#1. the Standard Doji: is a single candlestick where the Open and Close prices
#are pretty much the same.
 
#2. the Dragonfly Doji: considered a bullish (UP) reversal pattern, is a candle
#where the price closes about the same as the opening price, and have a long 
#lower "shadow" (that means, the price went downwards, and later remonted),
#telling that the market is testing to find out where "support" or demand is,
#or in other words, the sellers were selling but found a strong opposition made
# by the stock buyers.

#3. Gravestone Doji: the opposite of the Dragonfly Doji, and considered a bearish
#(DOWN) reversal pattern, is a candle where the price closes about the same as 
#the opening price,  with a long upper "shadow", meaning that the price went
#upwards, but the buyers found a strong opposition made by the stock sellers.
#Investors see this pattern as a time to sell, or exit the market.

#As a first step, at the present stage of this development, we'll use only
#the Standard Doji candle. Further on this work, we'll use also other types
#of Doji candles.

# Researching Reversal Candles: Gravestone Doji and Dragonfly Doji candles:
# According to the prices of the stock, the size of the Doji Reversal candle will
# be fixed to 0.01 $ :
dojiSize <- 0.01

# Creating a predictor containing both Gravestone and Dragonfly Doji:
reversalCandle<-data.frame(ifelse(abs(price$Close - price$Open)<dojiSize,"YES","NO"))

# Checking the quantity of Reversal candles on the dataset:
countOfDoji<-length(reversalCandle[reversalCandle$Close == "YES",])
countOfDoji


#The algorithms used on the present project, will compute on Moving Averages. 
#In this section, will be used Exponential Moving Averages (EMAs), for daily
#periods of 20 and 7 days respectively. After calculating the Moving Averages,
#we will compute both the relative position of the current candle to the two 
#EMAs, and also the crossings between them:

ema7 <- EMA(price$Close, n = 7)
ema20 <- EMA(price$Close, n = 20)

# Using the above two EMAs, we can calculate whether the stocks price is
# actually above or below the moving averages:
relativePositionToEMA7<-data.frame(ifelse(price$Close > ema7,"ABOVE","BELOW"))
relativePositionToEMA20<-data.frame(ifelse(price$Close > ema20,"ABOVE","BELOW"))

# And also, we can calculate the crossings between the two moving averages:
movingAveragesCrossing <-data.frame(ifelse(ema7 > ema20,"ABOVE","BELOW"))


# The following predictor reflects the size of the candle, which will be
# measured according to the absolute difference between the opening-closing
# prices:
candleSize <-data.frame(abs(price$Close - price$Open))
head(candleSize)


# The following field will be useful to calculate our stocks yield in case
# the prediction was correct:
revenue <- lead(candleSize$Close, n = 1)


## Creating a Data frame with the predictors as columns:
dsPredictors<-data.frame(todayCandle,
                         yesterdayCandle,
                   reversalCandle,
                   relativePositionToEMA7,
                   relativePositionToEMA20,
                   movingAveragesCrossing ,
                   candleSize,revenue,
                   tomorrowCandle)

names(dsPredictors)<-c("todayCandle",
                       "yesterdayCandle",
                   "reversalCandle",
                   "relativePositionToEMA7",
                   "relativePositionToEMA20",
                   "movingAveragesCrossing" ,
                   "candleSize",
                   "revenue",
                   "tomorrowCandle")

# Taking a glimpse at the dataset:
head(dsPredictors,20)
head(yesterdayCandle,20)
head(todayCandle,20)
head(tomorrowCandle,20)

##########################################################
# 2.2.3.2 Data Cleaning
##########################################################


 

#The calculations performed previously, have produced NAs on the data, and in
#this section we'll clean it:

# Checking whether are there any NAs ? :
apply(price,2, function(x)sum(is.na(x)))
# The original dataset loaded had no NAs, and the only field with NAs is the 
# one that was created on code, "EMA", with the purpose of calculating the EMAs.
# This problem will be reflected on the dataframe with the predictors that
# was created right now:
apply(dsPredictors,2, function(x)sum(is.na(x)))
# Indeed, six columns with NAs can be detected.


# Eliminating the NAs caused by the EMAs computations:
dsPredictors<-slice(dsPredictors,21:length(dsPredictors$reversalCandle))
length(dsPredictors)
apply(dsPredictors,2, function(x)sum(is.na(x)))


# Now the NAs had been cleaned from the dataframe. The only fields with a
# NA (only one NA on the field), are the two columns which refers to 
# the future, "revenue" and "tomorrowCandle". 
# That means, the "lead" function writes NA because the final day does not exist.

##########################################################
# 2.2.3.3 Data Partition 
##########################################################





# The dataset will be partitioned in Train set (66% of the data) and
# a test validation dataset (33% of the price candles). 
# This partition must contain the required continuity between the
# yesterday-today-tomorrow candles, because this is what the training
# algorithm will try to research and reveal: the patterns of continuity
# between consecutive candles. Therefore, we cannot just take prices
# hazardously and insert them in both training set and test set.


# The data will be partitioned in a Train set, used to the Machine Learning
# algorithms training, and a Test dataset, used to validate the predictions made
# by using the Models.
# In this case, we will make predictions using a proportion of 2 to 1 between
# training data to test data:

trainRange<-1:200
testRange<-201:300

# After defining the ranges, the two datasets, Train and Test, are created:
train<-dsPredictors[trainRange,]
test<-dsPredictors[testRange,]


##########################################################
# 2.3 Modeling Section
##########################################################



# In this section we develop 3 Models in order to predict Stocks Market prices
# for the Visa data obtained from Yahoo:      
 
# 1. First the Formula to be used on the Models will be defined. 

# 2. Second, creation of the Support Vector Machines (SVM) Model 

# 3. Third, creation of the Naive Bayes Model 
 
# 4. Fourth, creation of the Random Forests Model
 



##########################################################
# 2.3.1 Defining the Formulas
########################################################## 



# First, we define the formula to be used on the Models calculation. Will we
# use all the columns as predictors? Or we'll use a narrow number of the fields
# to the computations? The predictions to be made are those concerning to the
# stocks prices direction tomorrow, i.e., the "tomorrowCandle". Therefore, taking
# apart this column, we'll use all other fields in order to perform the predictions.

# The predictions target is the field "tomorrowCandle":
goalToPredict<-"tomorrowCandle"

# Therefore, all other fields are taken as predictors:
allPredictors<-c("todayCandle","yesterdayCandle",
                   "reversalCandle","relativePositionToEMA7",
                 "relativePositionToEMA20","movingAveragesCrossing" ,
                   "candleSize") # ,"revenue"



# Creating the predictions formula:
allPredictors<-paste(allPredictors, collapse = "+" )
predictionsFormula<-as.formula(paste(goalToPredict,"~",allPredictors, sep=""))



#When calculating the accuracy of the models, we will both compute the errors found
#and also we'll want to calculate the profit rendered by the models. That's why
#we'll make a function to compute the yields, using the following logic: if the
#prediction was accurate, we'll earn the difference between the opening and the
#closing prices of the stocks tomorrow, i.e. the "revenue" field. This logic is
#expressed by the following function:

stocksYield <- function(dataframe,predictionMade){
  # predictionMade represents the prediction made by the machine learning model:
  dataframe$predictionMade <- predictionMade
  dataframe$prediReturn <- ifelse(
    dataframe$tomorrowCandle != dataframe$predictionMade,
    -dataframe$revenue,dataframe$revenue)
  dataframe$cumReturn <- cumsum(dataframe$prediReturn )
  return (dataframe)
}




##########################################################
# 2.3.2 Support Vector Machines (SVM) Model 
##########################################################


#In this section, a Support Vector Machines (SVM) Model is developed.

#On the algorithms of the present project, two different kernels will be used:
#the Polynomial kernel and the Sigmoid Kernel.
 

##########################################################
# 2.3.2.1 Using the Polynomial Kernel
########################################################## 




# As stated before, on the present project, two different kernels will be used:
# the Polynomial kernel and the Sigmoid Kernel.

# First, we will make predictions using the Polynomial Model, then we will perform
# the predictions by using the Sigmoid Model.


# Loading the Support Vector Machine software library:
library(e1071)


set.seed(42)
# Creating the Support Vector Machine model using Polynomial Kernel:
SupportVectorMachineModel <- svm(factor(tomorrowCandle) ~ todayCandle + 
    yesterdayCandle + reversalCandle + relativePositionToEMA7 +
      relativePositionToEMA20 + 
    movingAveragesCrossing + candleSize, data = train , kernel = "polynomial")



# Making the predictions by using the Support Vector Machine Model:
SupportVectorMachinePredictions <- predict(SupportVectorMachineModel, test)


# Calculating the profit results:
SupportVectorMachineResults <- stocksYield(test, SupportVectorMachinePredictions)


plot(SupportVectorMachineResults$prediReturn, type = "l")


plot(SupportVectorMachineResults$cumReturn, type = "l")
# The plots express respectively  predicted revenue and cumulative revenue, and
# can be seen that the results are not conveniently good.

# Computing the confusion matrix:
confusionmatrix.svm <- table(SupportVectorMachineResults$tomorrowCandle,
                             SupportVectorMachineResults$predictionMade)
print(confusionmatrix.svm)

# Calculating accuracy of the Support Vector Machine Model:
SupportVectorMachineErrors <- mean(SupportVectorMachineResults$tomorrowCandle !=
                    SupportVectorMachineResults$predictionMade)
print(paste("Accuracy", 1 - SupportVectorMachineErrors))

results <- data.frame(Model="Stocks Trading System expectations", 
                      Accuracy =  0.5100)

1 - SupportVectorMachineErrors -> totalAccuracy


results <- results %>% 
  add_row(Model="   First Model : Support Vector Machine - Polynomial kernel", 
                               Accuracy = 1 - SupportVectorMachineErrors
                               )
results



#The present model, Support Vector Machine based on the Polynomial kernel, does
#not achieve the goals of the project, which were of at least an accuracy of
#fifty one percent.




##########################################################
#  2.3.2.2 Using the Sigmoid Kernel 
##########################################################


# Now perform the predictions using the Sigmoid Kernel Model:


set.seed(42)
# Creating the Support Vector Machine model using Sigmoid Kernel:
SupportVectorMachineModel <- svm(factor(tomorrowCandle) ~ todayCandle + 
    yesterdayCandle + reversalCandle + relativePositionToEMA7 + relativePositionToEMA20 + 
    movingAveragesCrossing + candleSize, data = train , kernel = "sigmoid") 


# Making the predictions by using the SVM Model:
SupportVectorMachinePredictions <- predict(SupportVectorMachineModel, test)


# Calculating the profit results:
SupportVectorMachineResults <- stocksYield(test, SupportVectorMachinePredictions)


# Showing the revenues produced:
plot(SupportVectorMachineResults$prediReturn, type = "l")

# Cumulative revenue:
plot(SupportVectorMachineResults$cumReturn, type = "l")
# The plots above express respectively  predicted revenue and cumulative revenue, 
# and can be seen an improvement of the results.

# Computing and showing the confusion matrix:
confusionmatrix.svm <- table(SupportVectorMachineResults$tomorrowCandle,
                             SupportVectorMachineResults$predictionMade)
print(confusionmatrix.svm)

# Calculating accuracy of the Support Vector Machine Model:
SupportVectorMachineErrors <- mean(SupportVectorMachineResults$tomorrowCandle !=
                                     SupportVectorMachineResults$predictionMade)
print(paste("Accuracy", 1 - SupportVectorMachineErrors))

(1 - SupportVectorMachineErrors) + totalAccuracy -> totalAccuracy

# Adding the accuracy to the Results table:
results <- results %>% 
  add_row(Model="   Second Model : Support Vector Machine - Sigmoid kernel", 
                               Accuracy = 1 - SupportVectorMachineErrors
                               )
results


#The table expresses an improvement on accuracy, obtained from applying the 
#Sigmoid kernel to the Support Vector Machine. 




##########################################################
# 2.3.3  Naive Bayes Model
##########################################################




#In this section, we implement a Naive Bayes Model.
#For this project, being a problem of classification, since we try to predict whether
#the prices will be incremented or will fall tomorrow, the Naive Bayes algorithm can
#be very useful to make the predictions.


# Loading the Naive Bayes software library:
library("naivebayes")


# Creating the Naive Bayes Model:
set.seed(42)
NaiveBayesModel <- naive_bayes(predictionsFormula, data = train)


# The following graphs show the predictive power of the different predictors,
# according to the Naive Bayes Model:


plot(NaiveBayesModel)


#From the graphs we can extrapolate some useful indications. It seems that, if
#Today's candle is Bull, there are less probabilities that tomorrow's candle 
#will be Bull again, that means, that the price of the stock may stop increasing.
#And on the contrary, if today's candle is Bear, tomorrow there will be more chances
#that continue to be Bear. The same occurs with Yesterday's candle. 
#A more interesting thing happens with the predictor "RelativePositionToEMA7".
#It seems that, if today the price closes above the Moving Average, there are
#good chances that tomorrow prices will go down. Also, if the 7 days Moving 
#Average is above the 20 days long EMA, there are bigger chances that tomorrow's
#candle will be Bear, i.e. prices would go down.


# Making the predictions to guess the Next Day candle direction:
NaiveBayesPredictions <- predict(NaiveBayesModel, test)    
                                
NaiveBayesResults <- stocksYield(test, NaiveBayesPredictions)

# Showing the Prediction results:
plot(NaiveBayesResults$prediReturn, type = "l")

# Cumulative revenue:
plot(NaiveBayesResults$cumReturn, type = "l")
# The plots above show again an improvement of the results for 
# respectively predicted revenue and cumulative revenue.


# Computing and showing the confusion matrix:
confusionmatrix.nb <- table(NaiveBayesResults$tomorrowCandle,
                            NaiveBayesResults$predictionMade)
print(confusionmatrix.nb)
# Calculating accuracy of the Naive Bayes Model:
NaiveBayesErrors <- mean(NaiveBayesResults$tomorrowCandle !=
                   NaiveBayesResults$predictionMade)
print(paste("Accuracy", 1 - NaiveBayesErrors))

(1 - NaiveBayesErrors) + totalAccuracy -> totalAccuracy

# Adding the accuracy to the Results table:
results <- results %>% 
  add_row(Model="   Third Model : Naive Bayes Model", 
                               Accuracy = 1 - NaiveBayesErrors
                               )
results


# We can see an improvement of the accuracy obtained.




##########################################################
# 2.3.4  Random Forest Model
##########################################################


#Random Forests are Supervised Machine Learning algorithms used mainly for
#classification problems, although they can also solve regression ones.


#On the algorithm used in the present project, the number of data points in the nodes
#of the tree, will be 500.


# Loading the Random Forests software library:
library(randomForest)

 
set.seed(42)
# Creating the Random Forests model:
RandomForestModel <- randomForest(factor(tomorrowCandle) ~ todayCandle + 
    yesterdayCandle + reversalCandle + relativePositionToEMA7 +
      relativePositionToEMA20 + 
    movingAveragesCrossing + candleSize, data = train, ntree=500 )
# As discussed previously, incrementing the "ntree" parameter will get better 
# results at the risk of overfitting the model.

# Showing the model:
plot(RandomForestModel)

varImpPlot(RandomForestModel)


# Making the predictions:
RandomForestPredictions <- predict(RandomForestModel, test)


# Calculating the predicted revenues income returns:
RandomForestResults <- stocksYield(test, RandomForestPredictions)

# Showing the Prediction results:
plot(RandomForestResults$prediReturn, type = "l")


# Cumulative revenue:
plot(RandomForestResults$cumReturn, type = "l")

# Computing and showing the confusion matrix:
confusionmatrix.rf <- table(RandomForestResults$tomorrowCandle,
                            RandomForestResults$predictionMade)
print(confusionmatrix.rf)


# Calculating accuracy of the Random Forest Model:
RandomForestErrors <- mean(RandomForestResults$tomorrowCandle !=
                   RandomForestResults$predictionMade)
print(paste("Accuracy", 1 - RandomForestErrors))

(1 - RandomForestErrors) + totalAccuracy -> totalAccuracy


# Adding the accuracy to the Results table:
results <- results %>% 
  add_row(Model="   Fourth Model : Random Forest Model", 
                               Accuracy = 1 - RandomForestErrors
                               )
results


#This Fourth Model, the Random Forest Model, shows again an improvement of the
#accuracy obtained.



##########################################################
# 2.3.5 Accuracy & Performance Comparison - Visa Inc. Case
##########################################################



#We could by now perform a comparison between the models developed until now,
#calculating both their accuracy and the projected revenue that they would
#produce.
#The prices periods used to compute the models were the following:

print(paste("The Train dataset prices were computed between the dates ",
            rownames(train[1,]), " and " ,
            rownames(train[length(train$reversalCandle),])))
print(paste("The Train dataset prices were computed between the dates ",
            rownames(test[1,]), " and " ,
            rownames(test[length(test$reversalCandle),])))


#And the corresponding prices for those dates are the following:

# Plotting the training and testing sets :
chartSeries(price$Close[(21+trainRange),], theme = "white") + neuralTheme
chartSeries(price$Close[(21+testRange),], theme = "white") + neuralTheme


#The revenues produced by the previous three models developed, are the following:


library(ggplot2)
NaiveBayesRevenue <- NaiveBayesResults$cumReturn
SupportVectorMachineRevenue <- SupportVectorMachineResults$cumReturn
RandomForestRevenue <- RandomForestResults$cumReturn
total.cumReturn <- NaiveBayesRevenue + SupportVectorMachineRevenue +
  RandomForestRevenue

names(NaiveBayesRevenue) <- "cumreturn"
names(SupportVectorMachineRevenue) <- "cumreturn"
names(RandomForestRevenue) <- "cumreturn"


# Transform the vector into a data.frame
combined.cumReturn <- data.frame(c(1:length(NaiveBayesRevenue)),NaiveBayesRevenue,
                                 SupportVectorMachineRevenue,RandomForestRevenue,
                                 total.cumReturn)

# Plot all cumulative revenues for all three models:
print(ggplot(combined.cumReturn) + 
        geom_line(aes(combined.cumReturn[,1], 
                      SupportVectorMachineRevenue,
                      colour = "Support Vector Machine")) +
        geom_line(aes(combined.cumReturn[,1], NaiveBayesRevenue,
                      colour = "Naive Bayes")) +
        geom_line(aes(combined.cumReturn[,1], RandomForestRevenue,
                      colour = "Random Forest")) +
        geom_line(aes(combined.cumReturn[,1], total.cumReturn,
                      colour = "Total")) +
        ylab("Revenue") 
        
        )





# Cleaning up RAM memory:
 



rm( price, train, test, candleSize, combined.cumReturn, 
    ema20, ema7, movingAveragesCrossing, RandomForestModel,
    relativePositionToEMA20, relativePositionToEMA7, reversalCandle,
    todayCandle, yesterdayCandle, SupportVectorMachineResults, 
    dsPredictors, NaiveBayesModel, NaiveBayesResults, neuralTheme,
    RandomForestResults, tomorrowCandle, SupportVectorMachineModel, V)
gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)



########################################################################################
########################################################################################
#  3.                  Methods/Analysis Section - Apple Inc. Case
########################################################################################
########################################################################################


#Through the following sections, a Neural Network Model will be developed, exploring
#market stocks from another famous firm, Apple, and enhancing the predictors used
#previously. Accordingly, different computing software will be used, and compared.
#We will base our research upon two R  Neural Network software packages, Caret and
#Neuralnet, and compare the results obtained.


#In this project, because we have seven nodes in the input layer, and two on the
#output layer, we choose accordingly seven neurons, placed inside only
#one hidden layer.

#Another way of determining the number of neurons in the hidden layers, is applying
#the following formula:
#    neurons ~ sqrt(input layer nodes * output layer nodes)

#Using the above formula, and because we have seven input nodes and two output
#nodes, we could state the following:
#    neurons = sqrt(7 * 2) = sqrt(14) = 3.74 ~ 4
#Nonetheless, we found more accurate using four neurons as for this second method 
#of determining the number of hidden nodes.





##########################################################
# 3.1 Techniques for Data Loading and Preparation
##########################################################



#The data on which is based this Neural Network research, is different from the
# previously used. This time, there will be downloaded stocks prices from Apple.
# That's why on this section we explore the newly loaded dataset. 
#As a first necessary step, we install the required software and download
#the stocks prices from Yahoo Finance.


# Note: this process could take a couple of minutes
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(VIM)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(VIM)
library(ggthemes)
library(neuralnet)
library(dplyr) 
library(kableExtra) 
library(tidyr) 
library(stringr) 
library(forcats) 
library(ggplot2)
library(caret)
library(data.table)
library(tidyverse)
library(quantmod)
library(TTR)
library(lubridate)



##########################################################
# 3.1.1 Loading Apple Stocks prices from Yahoo Finance
##########################################################



#Here the Apple Stock prices will be loaded and used for training and testing
#the Machine Learning Neural Network model.
#We will try to download the data from Yahoo, performing 10 attempts:


i <- 1
maxTries <- 10
while (i < maxTries) {
 
  try(
    getSymbols("AAPL", 
         from ="2017-01-01", 
         to = "2020-12-31", 
         src =  "yahoo", 
         adjust =  TRUE)
  )
  if (exists("AAPL")) {
    break
  }  
   i <- i + 1
}

# Create a dataframe with the downloaded data:
APPLEPrices <- as.data.frame(AAPL)
APPLEPrices$Date <- as.Date(rownames(APPLEPrices))
head(APPLEPrices)
names(APPLEPrices) <- c("Open","High","Low","Close","Volume","Adjusted","Date")

# Discarding the columns that are not needed:
APPLEPrices <- APPLEPrices %>% select(c(Date,Open,High,Low,Close))
APPLEPrices<-xts(APPLEPrices[, -1, drop=FALSE],  order.by=APPLEPrices[,1])




##########################################################
# 3.2 Data Analysis Exploration & Modeling
##########################################################

 

##########################################################
# 3.2.1 Preliminar Observations
##########################################################



# In this section, we analyze the data provided and the relations between the
# predictors.
# At this stage, we take a look at the loaded dataset:

# Prices dataset glimpse: 
nrow(APPLEPrices)
str(`APPLEPrices`)
class(APPLEPrices)
head(APPLEPrices) 
length(APPLEPrices) 
ncol(APPLEPrices)



#At first glance, we can see that the Apple dataframe is in tidy format, 
#i.e. each row represents only one observation, being the column names the
# prices features.



##########################################################
# 3.2.2 Features Analysis
##########################################################



##########################################################
# 3.2.2.1 - Analysis of the Stock Prices
##########################################################



# Let's see some general data on the prices:
 
summary(APPLEPrices$Close)


#The mean closing price of the Visa stock is 21.792, the Minimal price is 6.868,
#and the maximal closing price is 136.690 usd.

#To get a first approach to the Technical Trading Analysis features, the following
#graph shows the Apple prices as of May 2019, represented by Bar icons, which place
#greater emphasis on the closing price of the stock in relation to the previous 
#day closing prices. Also, we create a new theme for Neural Networks.

# Creating a custom Theme for be used on graphs :
neuralTheme <- theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "aliceblue"), 
        text = element_text( face = "italic",
                            colour = "black", size = 8,
                            lineheight = 0.9,  hjust = 0.5,
                            vjust = 0.5, angle = 0, 
                            #margin = margin(), 
                            debug = FALSE),
        axis.title = element_text(colour = "gray" ))

barChart(APPLEPrices['2019-05'], theme = "white") + neuralTheme


#Bar charts are pretty well informative, however, the candlestick charts place
#the highest importance to the close prices as it relates to the open of the same
#day, which can be seen on the following charts,
#which show the same data for the Apple stocks, this time using Japanese Candles,
#in two different formats. Another two ways of seeing the Apple prices :

candleChart(APPLEPrices['2019-05'],multi.col=TRUE,theme="white") + neuralTheme
chartSeries(APPLEPrices['2019-05'], theme = "white")  + neuralTheme


#When representing longer periods of data, the candlesticks charts can become
#quite difficult to follow. This two charts show the prices for the entire period
#of stock prices loaded for Visa :

chartSeries(APPLEPrices, subset = "2019-03::",theme = "white") + neuralTheme
chart_Series(APPLEPrices, subset = "2019/") 


#The Technical Analysis usually builds upon Indicators, or mathematical
#computations,
#such as Bollinger Bands, MACD Moving Averages, etc. The following plots
#represents some of those indicators applied to the data that we have loaded
#from Yahoo right now:
chartSeries(APPLEPrices, TA="addBBands(n=20)", subset="2019-01-21::2019-12-31")
candleChart(APPLEPrices, TA=c(addMACD(),addVo()), subset = '2019') + neuralTheme



##########################################################
# 3.2.3 Data Cleaning, Preparation and Partition
##########################################################



##########################################################
# 3.2.3.1 Creating the predictors for the Model
##########################################################



#The next step, before proceeding to the partition of the data in training and
#test datasets, is to create the columns that will be used for the computations.

#The algorithms used on the present section, will compute on Moving Averages. 
#As previously explained, the Moving Averages are trend following indicators, 
#that show the trends of the market. They are useful in order to follow the 
#prices trends. 

#On previous steps of the present research, moving averages with a short
#periods, specifically seven days and twenty days, were used as predictors.
#They represent the short term trends. 

#On the present step, we'll use a fifty days moving average period, which shows 
#a medium trend. Short term traders use a 21-period moving average, 
#Medium-term will use a 50 day EMA, and long term traders use 200 periods EMAs.

#We'll use the computation for the Moving Average known as "exponential",
#which make it quicker to react to the price changes, where every point 
#included in the moving average decreases in weight over time.

#Therefore, on this point of this project, Exponential Moving Averages for daily
#periods of 20 and 50 days will be used respectively.

# Because for this algorithm, an Exponential Moving Average (EMA) of 50 days
# interval times will be used, such an indicator is shown here, to get an
# intuitive view of its utilization:
 
APPLEPrices$EMA <- EMA(Cl(APPLEPrices), n = 50)
candleChart(APPLEPrices, TA=c(addMACD(),addVo()), subset = '2019')
plot(APPLEPrices)

#Next, we will compute the predictors for the Models, using the stocks prices of
#Apple, downloaded from Yahoo.

#The following predictor will express the current day candle type:

todaysCandle<-data.frame(ifelse(APPLEPrices$Close > APPLEPrices$Open,"BULL","BEAR"))


# After creating this field, more predictors will be created using 
# the created column.
# The following predictor will express the previous candle type. For this purpose, 
# the "lag" function will be used:

yesterdaysCandle<-data.frame(lag(todaysCandle$Close,n=1))


# This predictor will express the next (future) candle type: the "lead"
# function will be used:

tomorrowsCandle<-data.frame(lead(todaysCandle$Close, n = 1))


#As stated above, a "Reversal" is when a trend change to the opposite direction.
#When the trend is going to change direction, the appearance of the candle usually
#changes too, revealing that the trend is about to change direction.

#The algorithm used on this project will research the reversal candle known
#as "Doji", which represents indecision in the market. 

#As explained before, on the present work we'll use three types of Doji candles:

#1. the Standard Doji: is a single candlestick where the Open and Close prices
#are pretty much the same.

#1. the Dragonfly Doji: considered a bullish (UP) reversal pattern, is a candle
#where the price closes about the same as the opening price, and have a long 
#lower "shadow" (that means, the price went downwards, and later remonted),
#telling that the market is testing to find out where "support" or demand is,
#or in other words, the sellers were selling but found a strong opposition made
# by the stock buyers.

#2. Gravestone Doji: the opposite of the Dragonfly Doji, and considered a bearish
#(DOWN) reversal pattern, is a candle where the price closes about the same as 
#the opening price,  with a long upper "shadow", meaning that the price went
#upwards, but the buyers found a strong opposition made by the stock sellers.
#Investors see this pattern as a time to sell, or exit the market.

#Now, in addition to the previously researched Standard Doji candle, we will also
#research both Gravestone Doji and Dragonfly Doji candles.
#According to the prices of the stock, the size of the Doji Reversal candle will
#be fixed to 0.01 usd :
dojiSize <- 0.01

# Calculation of the required size of the Doji candles:
today.size <-data.frame(abs(APPLEPrices$Close - APPLEPrices$Open))
size_avg <- mean(today.size$Close)

# Now we create a predictor containing both Gravestone and Dragonfly Dojis:

enhancedReversalCandle<-data.frame(ifelse(
  
  (abs(APPLEPrices$Close - APPLEPrices$Open)<dojiSize)
  &
  (
                          ((APPLEPrices$High - APPLEPrices$Low) >
                             size_avg)
                          |
                            ((APPLEPrices$Low - APPLEPrices$High) >
                             size_avg)
   )
                          ,"YES","NO")
  )




 #On this calculation, we choose as enhancedReversalCandle the ones that have a
 #"shadow" or tail bigger than the average size of all candles on data,
# that means, that the difference between the High and Low prices, or
 #the opposite, between Low and High, is greater than the average gap
# between Open-Close prices.

# Checking the quantity of Reversal candles on the dataset:

countOfDoji<-length(enhancedReversalCandle[enhancedReversalCandle$Close == "YES",])
countOfDoji

# As explained above, during an uptrend, prices tend to stay above the moving
# averages. And accordingly, in a downtrend, prices tend to stay below the moving
# average. The position of the moving average relative to the price indicates the
# trend.
# The next three predictors will express the Exponential Moving Averages (EMA) for
# 20 days and 50 days respectively, and its crosses:

movingAverage20 <- EMA(APPLEPrices$Close, n = 20)
ema20 <- EMA(APPLEPrices$Close, n = 50)
priceRelativeToEMA20<-data.frame(
  ifelse(APPLEPrices$Close > movingAverage20,"ABOVE","BELOW"))
priceRelativeToEMA50<-data.frame(ifelse(APPLEPrices$Close > ema20,"ABOVE","BELOW"))


# Now there is calculated the cross-over of those two EMAs:
 
EMA20_50Crossings <-data.frame(ifelse(movingAverage20 > ema20,"ABOVE","BELOW"))



# The following predictor will reflect the size of the candles:
candleSizeNN <-data.frame(abs(APPLEPrices$Close - APPLEPrices$Open))


# The following predictor will be useful to calculate the income or revenue 
# that will be obtained in case that the predictions were correct:

revenueNN <- lead(candleSizeNN$Close, n = 1)


## Next, a dataframe with the FEATURES as columns is created:
dsData<-data.frame(todaysCandle,
                   yesterdaysCandle,
                   enhancedReversalCandle,
                   priceRelativeToEMA20,
                   priceRelativeToEMA50,
                   EMA20_50Crossings ,
                   candleSizeNN,
                   revenueNN,
                   tomorrowsCandle)

names(dsData)<-c("todaysCandle",
                 "yesterdaysCandle",
                 "enhancedReversalCandle",
                 "priceRelativeToEMA20",
                 "priceRelativeToEMA50",
                 "EMA20_50Crossings" ,
                 "candleSizeNN",
                 "revenueNN",
                 "tomorrowsCandle")

# A quick glimpse of the dataframe:
head(dsData,20)
head(yesterdaysCandle,20)
head(todaysCandle,20)
head(tomorrowsCandle,20)



##########################################################
# 3.2.3.2 Data Cleaning 
##########################################################



#The calculations performed previously, have produced NAs on the data, and in
#this section we'll clean it. 
# Checking whether are there any NAs ? :

apply(APPLEPrices,2, function(x)sum(is.na(x)))



# The original dataset loaded had no NAs, and the only field with NAs is the 
# one that was created on code, with the purpose of calculating the EMAs.
# This problem will be reflected on the dataframe with the predictors that
# was created right now:

apply(dsData,2, function(x)sum(is.na(x)))


# Indeed, six columns with NAs can be detected.
# Eliminating the NAs produced by the EMAs computations:

dsData<-slice(dsData,51:length(dsData$enhancedReversalCandle))
nrow(dsData)
apply(dsData,2, function(x)sum(is.na(x)))


# Now the NAs had been cleaned from the dataframe. The only fields with a
# NA (only one NA on the field), are the two columns which refers to 
# the future. That means, the "lead" function writes NA because the final
# day does not exist.

# For the training purposes, not the entire dataset will be used, but a portion 
# of it, which in the following section will be partitioned in two dataframes:
# a training set and a test set. In a further section, the training dataset, that
# will be called "Train", will again be partitioned several times in 
# training-test sets, each time consisting of different sizes. This big dataset
# "Train" will be created now as follows:

predictorsDataset<-dsData[1:950,]



##########################################################
# 3.2.3.3 Predictors Analysis for the Neural Network
##########################################################

 
#In this section, we will specifically analyze the relevance of the predictors,
#i.e. will they be significant or not for the construction of the models.

# The predictor "candleSizeNN" is a quantitative variable. We'll check whether 
#this independent variable is significant in predicting the next day candle, 
#using the Roc curve:


library(pROC)
c <- roc(predictorsDataset$tomorrowsCandle ~ predictorsDataset$candleSizeNN)
c


# A determined variable is considered a good predictor if its area under
# the curve is bigger than 0.70.
# The area under the curve is :  0.5454   
# This means that this variable is not a good predictor of the next day candle.

# Now we check its confidence interval:

ci.auc(c)


# The variable is considered a good predictor if its CI does not include values
# below 0.70.
# In our case, the candleSizeNN  variable is entirely under 0.70, which means that
# it is not a good predictor.
# This is the Roc curve for this quantitative variable:

plot(c)


# We can see that the area under the curve is very small: if it would be a
# big area, it could be considered a good predictor, but it is not the case.

# We want to know if every variable value has influence on the dependent variable,
# i.e. "next day candle". If that's not the case, we would discard that
# value. For that sake, we use XTABS function:

xtabs(~ tomorrowsCandle + todaysCandle,predictorsDataset)
xtabs(~ tomorrowsCandle + yesterdaysCandle,predictorsDataset)
xtabs(~ tomorrowsCandle + enhancedReversalCandle,predictorsDataset)
xtabs(~ tomorrowsCandle + priceRelativeToEMA20,predictorsDataset)
xtabs(~ tomorrowsCandle +  EMA20_50Crossings ,predictorsDataset)


# We conclude that next day's BULL and BEAR candles are both represented 
# by all the independent variables values.


##########################################################
# 3.2.3.4 Formulas for the Neural Network
##########################################################



# First, we define the formula to be used on the Neural Network Models computation.
# Will we use all the columns as predictors? Or we'll use a narrow number of the
# fields to the computations? The predictions to be made are those concerning 
# to the stocks prices direction tomorrow, i.e., the "tomorrowCandle". 

# We will define three Neural Network models: the first one including 
# all independent variables, the second one will include two independent predictors,
# and the third one will allow us to perform an univariant Logistic Regression, i.e.
# a regression with only one predictor:

# Defining the dependent variable:
dependent <-"tomorrowsCandle"


# Discarding the 2 unnecessary columns (and perhaps discarding also the
# "enhancedReversalCandle" field, if no such type of candles were detected):
independent <- names(predictorsDataset)
if (countOfDoji > 0) {
  independent <- independent[!independent %in%
                             c("revenueNN","tomorrowsCandle")]
} else {
  independent <- independent[!independent %in%
                             c("revenueNN","tomorrowsCandle",
                               "enhancedReversalCandle")]
}


independent <- paste( independent, collapse = " + ")


#Next, we define the Formulas:

# Model #1 : this is the preferred model: Using all predictors for the Formula:
formula1<-as.formula(paste(dependent,"~",independent))

# Model #2 : using only two predictors:
predictors2 <-c("priceRelativeToEMA20","candleSizeNN" )
features<-paste(predictors2, collapse = "+" )
formula2<-as.formula(paste(dependent,"~",features, sep=""))

# Model #3 : Univariant Logistic Regression or regression with only one predictor:
predictors3 <-c("todaysCandle")
features<-paste(predictors3, collapse = "+" )
formula3<-as.formula(paste(dependent,"~",features, sep=""))





##########################################################
# 3.2.3.5  Data Normalization of the dataset
##########################################################



# In this section will be used a Neural Network, and since the dependent
# variable is definitely binary, diatomic, because its values can only be "BULL" 
# or "BEAR", we'll eventually use the activation function "Logistic". 

# The neuralnet() function that we are going to use, only takes numeric values
# as input. Therefore we must perform some kind of data cleaning and preparation
# in order to create our Neural Network.

# We normalize this variables to be sure that each variable contributes 
# equally to the analysis. The normalization of the data will be performed
# using Min-Max Normalization: (X – min(X)) / (max(X) – min(X))

# First, we'll normalize the variable predictorsDataset$candleSizeNN:it should represent
# the size of the candle. 
# Therefore, we should convert its double values to categorical ones,
# which will grade from 1 to 4: from the smaller candles to the bigger ones.

# Take a glimpse of the data:

str(predictorsDataset$candleSizeNN  )
min(predictorsDataset$candleSizeNN )
max(predictorsDataset$candleSizeNN )
class(predictorsDataset$candleSizeNN)  

# Normalization of the predictorsDataset$candleSizeNN field:
predictorsDataset$candleSizeNN <- 
  (predictorsDataset$candleSizeNN - min(predictorsDataset$candleSizeNN)) / 
  (max(predictorsDataset$candleSizeNN)  - min(predictorsDataset$candleSizeNN ))


# Creating a Candle Size variable:
m <-mean(predictorsDataset$candleSizeNN )
length(predictorsDataset$candleSizeNN[predictorsDataset$candleSizeNN > m])
length(predictorsDataset$candleSizeNN[predictorsDataset$candleSizeNN <= m])


candleSizeNN <- data.frame(
                                      ifelse(predictorsDataset$candleSizeNN > (m*1.5),4,
                                      ifelse(predictorsDataset$candleSizeNN > m,3,
                                      ifelse(predictorsDataset$candleSizeNN > (m/2),2,
                                      ifelse(predictorsDataset$candleSizeNN >= 0,1,3 
                                             )
                                      )
                                      )
                                      )
                                       
                    )
names(predictorsDataset$candleSizeNN)<-c("candleSizeNN")


#Normalization of the variable "candleSizeNN" :

#We'll be using Min-Max Normalization: (X – min(X)) / (max(X) – min(X)):

str(predictorsDataset)
min(candleSizeNN)
max(candleSizeNN)

candleSizeNN <- (candleSizeNN - min(candleSizeNN)) / 
  (max(candleSizeNN) - min(candleSizeNN))

names( candleSizeNN)<-c("candleSizeNN")


#Normalization of the variable "revenueNN" : doing the same as before, for the
#revenueNN predictor:

str(predictorsDataset)
min(predictorsDataset $revenueNN)
max((predictorsDataset $revenueNN))
hist(predictorsDataset $ revenueNN)

predictorsDataset$revenueNN <- (predictorsDataset$revenueNN - min(predictorsDataset$revenueNN)) / 
  (max(predictorsDataset$revenueNN) - min(predictorsDataset$revenueNN))

# Show the results:
hist(predictorsDataset$revenueNN)


#Making all fields numeric: as stated above, Neural Networks only take numeric
#values as input for the computations:


yesterdaysCandle<-data.frame(
  ifelse(predictorsDataset$yesterdaysCandle == "BULL","1","0"))
todaysCandle<-data.frame(
  ifelse(predictorsDataset$todaysCandle == "BULL","1","0"))
tomorrowsCandle <-data.frame(
  ifelse(predictorsDataset$tomorrowsCandle == "BULL","1","0"))

enhancedReversalCandle <- data.frame(
  ifelse(predictorsDataset$enhancedReversalCandle == "YES","1","0"))
priceRelativeToEMA20 <-data.frame(
  ifelse(predictorsDataset$priceRelativeToEMA20 == "ABOVE","1","0"))
priceRelativeToEMA50 <-data.frame(
  ifelse(predictorsDataset$priceRelativeToEMA50 == "ABOVE","1","0"))
EMA20_50Crossings <-data.frame(
  ifelse(predictorsDataset$EMA20_50Crossings == "ABOVE","1","0")) 
revenueNN <-as.numeric(predictorsDataset$revenueNN )


## Create a dataframe with the predictors as columns:
normalizedDataset<-data.frame(todaysCandle,yesterdaysCandle,
                   enhancedReversalCandle,
                 priceRelativeToEMA20,
                 priceRelativeToEMA50,
                 EMA20_50Crossings ,
                   candleSizeNN,revenueNN,
                   tomorrowsCandle)

names(normalizedDataset)<-c("todaysCandle","yesterdaysCandle",
                   "enhancedReversalCandle",
               "priceRelativeToEMA20",
               "priceRelativeToEMA50",
               "EMA20_50Crossings" ,
               "candleSizeNN",
               "revenueNN",
               "tomorrowsCandle")


normalizedDataset$yesterdaysCandle <-
  as.numeric(normalizedDataset$yesterdaysCandle)
normalizedDataset$todaysCandle<- as.numeric(normalizedDataset$todaysCandle)


normalizedDataset$enhancedReversalCandle <- 
  as.numeric(normalizedDataset$enhancedReversalCandle)
normalizedDataset$priceRelativeToEMA20 <-
  as.numeric(normalizedDataset$priceRelativeToEMA20)
normalizedDataset$priceRelativeToEMA50 <-
  as.numeric(normalizedDataset$priceRelativeToEMA50)
normalizedDataset$EMA20_50Crossings <-
  as.numeric(normalizedDataset$EMA20_50Crossings )
normalizedDataset$revenueNN <-as.numeric(normalizedDataset$revenueNN )

# Show the results:
hist(normalizedDataset $ candleSizeNN)


#Factoring of the Output Dependent Variable: now we transform the dependent 
#variable to a factor:


normalizedDataset$tomorrowsCandle <- factor(normalizedDataset$tomorrowsCandle,
                               levels = c(0,1),
                               labels = c(0,1))



#Next, we proceed to Normalizing & Scaling all the Predictors.
# As stated before, we must normalize the variables in order to avoid 
# some predictors to weight more than others. 
# The scaling of data is essential because otherwise a variable may have 
# a bigger impact on the prediction, only because of its scale. 
# Using non-scaled data may produce erroneous results. 

# Actually we want each variable to contribute equally to the analysis.
# To standardize the rest of the variables, we use the "scale" function.
# We do not scale the output variable,
# i.e. the dependent variable "tomorrowsCandle" : so we take only 8 
# independent variables to scale:


i = 1
for (i in 1:(length(normalizedDataset) - 2)) {
    temp <- scale(normalizedDataset[,i])
    apply(temp[,1,drop=F],1, function(x)sum(is.na(x))) -> NAs
    if (NAs[length(NAs)] > 0) {
     print(paste("There were ",NAs[length(NAs)]," NAs detected.") )
  
  } else {
    normalizedDataset[,i] <- temp[,1]
  }
}



# Check for the maximal and minimal values, in order to centering the
# scale of the columns:

subds <- subset(normalizedDataset, select =  -c(tomorrowsCandle))
if (countOfDoji == 0) {
  subds <- subset(normalizedDataset, select = 
                    -c(tomorrowsCandle,enhancedReversalCandle))
}
apply(subds, 2, max) -> maximums
apply(subds, 2, min) -> minimums

data <- as.data.frame(scale(subds,
                            center = minimums, scale = maximums - minimums))

# Check whether the "enhancedReversalCandle" predictor is relevant:
if (countOfDoji == 0) {
  data$enhancedReversalCandle <- normalizedDataset$enhancedReversalCandle
}
data$tomorrowsCandle <- normalizedDataset$tomorrowsCandle
normalizedDataset <- data



#As the final preparations before the Neural Network computations, we check the
#data to verify whether or not there are NAs inside the predictors:


# Checking for the behavior of NAs in the predictors:

aggr(normalizedDataset,
     col = c("green","red"),  # graphic colors
     numbers = TRUE,          # proportions represented by numbers
     sortVars = TRUE, # sort the variables according to their count of NAs
     labels = c("current","previous",
                   "enhancedReversalCandle","ema7","ema20","ema7ema2" ,
                   "candleSizeNN","revenueNN",
                   "next.day"),
     cex.axis = 0.75, # the width of the bars
     gap = 1, # distance between the graphs
     ylab = c("NAs Histogram","Patterns")) +
     neuralTheme



# On the "Patterns" part of the graph, it can be seen that all the predictors
# reach the maximum of "1.0", as expected.
# From the graph we can conclude that all the variables are complete,
# that means, there are not NAs at all.


# Results Data exploration: 

table(normalizedDataset$todaysCandle)
table(normalizedDataset$yesterdaysCandle)
table(normalizedDataset$enhancedReversalCandle)
table(normalizedDataset$priceRelativeToEMA20)
table(normalizedDataset$priceRelativeToEMA50)
table(normalizedDataset$EMA20_50Crossings)
table(normalizedDataset$candleSizeNN)



##########################################################
# 3.2.3.6  Data Partition of the dataset
##########################################################



# In order to divide the dataset in two sets, Train set and Test set, we cannot use
# the "Sample" function, because we need to ensure a continuity 
# of the stream of prices from a day to the next.
# An example of the utilization of the "Sample()" function, would be the following:


sample(1:nrow(dsData), round(0.75 * nrow(dsData))) -> index



# What we'll do instead, is to partition the data in Train and Test, but keeping
# the dates continuity stream, that means, the logic continuation between 
# successive candles. We'll take 75% of the dataset for creating the Train
# data frame, from the initial date until some date approximately at 75% from the
# beginning, and take the rest of the continuous stream of prices, until the end:

trainRange<-1:200
testRange<-201:300

dfNN1 <- normalizedDataset[trainRange,]
testNN1<-normalizedDataset[testRange,]




##########################################################
# 3.3 Neural Networks Modeling Section
##########################################################


# In this section we develop two Models in order to predict Stocks Market prices
# for the Apple data obtained from Yahoo. The first one using the Caret package,
# and the second one using the Neuralnet package. Then, we'll decide which one 
# gives the best results, and choose that one to proceed with our research.


# After the construction of the Models, the RMSE will be computed, on the
# validation dataset. This method of validation, the training-test split method,
# is in fact the simplest form of cross validation, known as holdout method.  
# A limitation of the holdout method is the variance of performance evaluation
# metrics, in our case, the RMSE, which can be highly based on the data
# selected to create the training and the test set.
# This dependence of performance on test-training split, reduces the variance 
# of performance metrics, and can be considered a bias. 
# For that reason, in the following section, a more elaborated way of validation
# will be performed on the Models.



##########################################################
# 3.3.1 Neural Network Caret Model
##########################################################



# Training the Caret Neural Networks function:

set.seed(42)
nncaret <- caret::train(tomorrowsCandle~.,
                   dfNN1,
                   method = "nnet",
                   trace = FALSE)

# Show the analysis of the calculations:
plot(nncaret)


#From the plot we can see that by using one neurons layer the algorithm obtains an
#accuracy of ~51%, while if using 5 layers, the accuracy descends to 49%

str(testNN1)

# Make the predictions:
caretPred <- predict(nncaret,testNN1)

confusionmatrix.CARET <- table(
  prediction= caretPred,   actual=testNN1$tomorrowsCandle)
print(confusionmatrix.CARET)


# Calculating misclassification:
misc <- 1 - sum(diag(confusionmatrix.CARET)) / sum(confusionmatrix.CARET)
misc
print(paste("Caret Misclassification percentage = ",(( misc) * 100),"%") )


#calculating accuracy:
lr.error <- mean(testNN1$tomorrowsCandle != caretPred )

print(paste("Caret Neural Network Accuracy = ",(( 1 - lr.error) * 100),"%") )



##########################################################
# 3.3.2 Neural Network Neuralnet Model
##########################################################



# Now we perform the training of the Neuralnet Neural Networks function.
# The dependency between independent and dependent variables is not linear, 
# therefore we use linear.output with value "false".

# For being relevant, the Accuracy of the Model must be bigger than 50%.

set.seed(42)
 
 
nn <- neuralnet(formula1, data = dfNN1, 
                hidden = 4, 
                threshold = 0.05,
                algorithm = "rprop+", # sag slr backprop rprop-
                act.fct = "logistic",
                lifesign = "none",
                rep = 10,
                stepmax = 100000,
                linear.output = F)



# We won't use the Formulas #2 and #3 because, while tested, the percentage of
# accuracy for the Neural Network Model descended.

#Now we can see the "nn$result.matrix" results, in order to check the results
#obtained from the Neural Network training.   

#After checking the nn$result.matrix, could be noticed that the "rep"
#(repetition) producing the smallest error, was the number 9, therefore
#we choose that "rep" which had the smaller error:

# In our case, it's the model 9 between 10 models, so we use it, sending the
# value "9" to the parameter "rep":


compTRAIN <- neuralnet::compute(nn,  within(dfNN1,rm(tomorrowsCandle)), rep = 9) 

# For more information about the repetitions obtained, uncomment :
#   nn$result.matrix


#To satisfy curiosity, we proceed to checking the model against the Training 
#dataset.
#We also select a threshold that defines which results correspond to which
#kind of candles, bear or bull:

min(compTRAIN$net.result)
max(compTRAIN$net.result)
thresholdTRAIN <- mean(compTRAIN$net.result)
thresholdTRAIN <- 0.5
predTRAIN <- ifelse(compTRAIN$net.result >= thresholdTRAIN,"1","0")

# Computing the Confusion Matrix:

confusionmatrix.TRAIN <- table(
  prediction= predTRAIN[,2] , actual=dfNN1$tomorrowsCandle)
print(confusionmatrix.TRAIN)


# Calculating misclassification:
misc <- 1 - sum(diag(confusionmatrix.TRAIN)) / sum(confusionmatrix.TRAIN)
misc
print(paste("Misclassification percentage = ",(( misc) * 100),"%") )
print(paste("Accuracy percentage = ", (( 1 - misc) * 100),"%") )

#calculating accuracy
lr.error <- mean(dfNN1$tomorrowsCandle != predTRAIN[,2] )

print(paste("TRAIN SET Accuracy = ",(( 1 - lr.error) * 100),"%") )


#Show the Neural Network for the repetition number "9":

plot(nn, rep = 9)



### 3.3.3  Validation by training-test Split Holdout Method


#Now we proceed to checking the model against the Test dataset. In this step,
#we perform Validation against the Test dataset :


comp <- neuralnet::compute(nn,within( testNN1,rm(tomorrowsCandle)), rep = 9) 



# Select a threshold that defines which results correspond to which
# kind of candles, bear or bull:

min(comp$net.result)
max(comp$net.result)
threshold <- mean(comp$net.result)
pred <- ifelse(comp$net.result >= threshold,"1","0")



#To perceive the results of the Neural Network predictions against the Validation
#dataset, we show the confusion matrix:

confusionmatrix.lr <- table(prediction= pred[,2] ,
                            actual=testNN1$tomorrowsCandle)
print(confusionmatrix.lr)



# The confusion matrix means, 24 BEAR predictions were accurate, but 19 weren't.
# And 31 BULL predictions were right, and 26 were not.

        
# Plotting the confusion matrix:

ind <- seq(1:100)
ggplot(testNN1,aes(ind, pred[,2] )) +
  geom_point(aes(color=as.numeric(testNN1$tomorrowsCandle)), alpha=10,
             shape = 1,stroke = 1.5) +
             scale_color_gradient(low = "red", high = "green") +
  theme_gray() +
  ylab("Next Day Predictions") +
  xlab("Candles")




#The red circles represent BEAR candles predictions, and ideally they 
#should be situated below the 0.51 line. 
#On the contrary, green circles are BULL candles predictions, and 
#they should be bigger than 0.51, hence situated above the line.
# We can see from the graph, the quantity of misplaced circles. representing
# miscalculations.


#Calculating the accuracy:

lr.error <- mean(testNN1$tomorrowsCandle != pred[,2] )


testNN1 %>% mutate(Prediction = pred[,2]) %>%
  mutate(Success = Prediction == testNN1$tomorrowsCandle)  %>%
  select(Prediction,tomorrowsCandle, Success, everything()) -> result

head(result,20)
mean(result$Success, na.rm = T) * 100 -> percentSuccess

paste("The percentage of accuracy for the Neural Network Model is:",
      percentSuccess,"%")



# This Model has an Accuracy of  55 %, that means that in 55% of
# the times, the prediction is correct.

# Yet another way of computing accuracy, is by calculating misclassification:


misc <- 1 - sum(diag(confusionmatrix.lr)) / sum(confusionmatrix.lr)
misc
print(paste("Misclassification percentage = ",(( misc) * 100),"%") )
print(paste("Accuracy percentage = ", (( 1 - misc) * 100),"%") )

#calculating accuracy
lr.error <- mean(testNN1$tomorrowsCandle != pred[,2] )

print(paste("Accuracy = ",(( 1 - lr.error) * 100),"%") )



#Next we proceed to the denormalization of the field revenueNN.
# We need de-normalize this column, in order to calculate the real return
# for the model:



min(testNN1$revenueNN)
max((testNN1$revenueNN))
hist(testNN1$ revenueNN)


min(testNN1$revenueNN)
max((testNN1$revenueNN))
hist(testNN1$ revenueNN)

# Create a function to calculate the revenue:
predictedReturn <- function(df,pred){
  
  df$pred <- pred
  df$prediReturn <- ifelse(df$tomorrowsCandle != df$pred, -df$revenueNN,df$revenueNN)
  df$cumReturn <- cumsum(df$prediReturn )
  return (df)
}

# results in usd :
testReturns <- predictedReturn(testNN1, pred[,2])
tail(testReturns$cumReturn)
head(testReturns)

sum(testReturns$prediReturn)

hist(testNN1 $ revenueNN)
hist(testReturns$prediReturn)



#Next we calculate the Quadratic Error, of the difference between the actual
#next.day.return (from Test dataframe) and the predicted next day return:


qe <- sum((testReturns$prediReturn - testNN1 $ revenueNN)^2)/nrow(testNN1)
qe


# Along with the Accuracy of the system, we calculate the RMSE:


str(testReturns$pred)
# This column represents the "BULL" candle:
str(pred[,2])     
str(as.numeric(testReturns$pred))
str(as.numeric(pred[,2]))

# Calculate RMSE:
RMSE.NN = (sum((as.numeric(pred[,2]) -
                  as.numeric(testReturns$tomorrowsCandle))^2) 
                  / nrow(testReturns)) ^ 0.5

# Show the RMSE:
RMSE.NN



# The RMSE of the Model is not impressive, because it is greater than 1. However,
# this model performs better than the previous models researched.



paste("The percentage of accuracy for the Neural Network Model is:",
      percentSuccess,"%")





##########################################################################
# 3.3.4 Cross validation by training-test K-fold Cross Validation Method
##########################################################################



 
# In the previous sections were built two Neural Network Models, the first one using
# the Caret library, and the second one using NeuralNet.
# After the construction of the Models, the RMSE was computed, using the
# validation dataset created with the training-test split Holdout method.
   

#The cross validation technique used in this section, will be the K-fold Cross
#Validation. This method can be viewed as a recurring Holdout method, in which 
#most of the data is partitioned into k equal subsets, and each time a subset is
#assigned as training set, while others are used for validating the model. 


#In order to compare the models performances on cross-validation, we add two
#functions for Model Evaluation Definitions: MAE & RMSE.
#First we design the Mean Absolute Error (MAE), which is the average of the 
#squared error that is used as the loss function, the square of the difference
#between the predicted and actual target variables:



MeanAbsoluteError <- function(data = NULL, predictions = NULL){
  mean(abs((as.numeric(data$tomorrowsCandle)) - (as.numeric(predictions))))
}



#Second, we define the RMSE - Root Mean Squared Error: RMSE is the square root 
#of the Mean Squared Error (MSE) 


RMSE <- function(data = NULL, predictions = NULL){
    (sum((as.numeric(predictions) -
                  as.numeric(data$tomorrowsCandle))^2) 
                  / nrow(data)) ^ 0.5

}


##########################################################
# 3.3.4.1  K-fold Cross Validation Loop
##########################################################



#Creating a dataframe that contains all RMSE-MAE-Accuracy results for the
# two Models (Caret and Neuralnet) iterations:


crossValidationResults <- data.frame(Model="Trading System expectations", 
                      RMSE=  0.8000, 
                      MeanAbsoluteError = 0.500,
                      Accuracy = 51)



# Initializing variables:
set.seed(42)
k = 7 # 7 no. of iterations according to dataset size
sizeTrain <- 200
sizeTest <- 100
dsSize <- nrow(normalizedDataset)
RMSE.NN = NULL
dfRMSECARET <- as.data.frame(1.300)
dfMAECARET <- as.data.frame(0.500)
dfACCCARET <- as.data.frame(50)
dfRMSENN <- as.data.frame(1.300)
dfMAENN <- as.data.frame(0.500)
dfACCNN <- as.data.frame(50)

# Dataframes used for computing averages after the cross-validation:
dfCaret = data.frame(x = numeric(), y = numeric(),z=numeric())
names(dfCaret)<-c("ValidationLoop","Accuracy","RMSE")
dfNeuralnet = data.frame(x = numeric(), y = numeric(),z=numeric())
names(dfNeuralnet)<-c("ValidationLoop","Accuracy","RMSE")

# Fit neural network models within cross-validation "for" loop:
for (i in 0:(k - 1)) {
        
        offset <- ifelse(i == 0,1,0)
        startTrain <- i * 100 + offset
        startTest <- startTrain + sizeTrain + 1
        endTrain <- startTrain + sizeTrain
        endTest <- startTest + sizeTest
        rangeTrain <- startTrain:endTrain
        rangeTest <- startTest:endTest
        
        tempTrain <- normalizedDataset[rangeTrain,]
        
        tempTest <- normalizedDataset[rangeTest,]
       
        

 #1 : using Caret Neural Network function:
set.seed(42)
nncaret <- caret::train(tomorrowsCandle~.,
                   tempTrain,
                   method = "nnet",
                   trace = FALSE)

# Make the predictions:
caretPred <- predict(nncaret,tempTest)

confusionmatrix.CARET <- table(
  prediction= caretPred,   actual=tempTest$tomorrowsCandle)

# Calculating misclassification:
misc <- 1 - sum(diag(confusionmatrix.CARET)) / sum(confusionmatrix.CARET)
#calculating accuracy
lr.error <- mean(tempTest$tomorrowsCandle != caretPred )
accuracy <- (( 1 - lr.error) * 100)

# Calculating the RMSE:
RMSE.NN   <-  RMSE(tempTest, caretPred) 
MAE <- MeanAbsoluteError(tempTest, caretPred) 
     
# Adding the Caret RMSE to the results data frame:
paste("Caret Model #",i + 1," validation: ",startTrain,":",
      startTrain + sizeTrain," // ",
      startTest,":",startTest + sizeTest," "
      ) -> modelCaret

crossValidationResults <- crossValidationResults %>% add_row(Model=modelCaret, 
                               RMSE=RMSE.NN, 
                               MeanAbsoluteError = MAE,
                               Accuracy  = accuracy
                                 )

dfRMSECARET[i + 1] <- RMSE.NN
dfMAECARET[i + 1] <- MAE
dfACCCARET[i + 1] <- accuracy

dfCaret <-rbind(dfCaret, 
                    data.frame(ValidationLoop=i + 1, 
                               Accuracy=accuracy,
                               RMSE=RMSE.NN))
 

 #2 : using NeuralNet Neural Network function:

set.seed(42)
nnCrossValidation <- neuralnet(formula1, data = tempTrain, 
                hidden = 4, 
                threshold = 0.05,
                algorithm = "rprop+", # sag slr backprop rprop-
                act.fct = "logistic",
                lifesign = "none",
                rep = 1,
                stepmax = 100000,
                linear.output = F)


# Make prediction on TEST dataset:
compCV <- neuralnet::compute(nnCrossValidation,
                           within( tempTest,rm(tomorrowsCandle)), 
                           rep = 1) 

threshold <- mean(compCV$net.result)
pred <- ifelse(compCV$net.result >= threshold,"1","0")

#confusion matrix
confusionmatrix.neuralnet <- table(prediction= pred[,2] ,
                            actual=tempTest$tomorrowsCandle)


#calculating accuracy:
# Using pred[,2] because that means the "1" ("BULL") candle:
lr.error <- mean(tempTest$tomorrowsCandle != pred[,2] )
accuracy <- (( 1 - lr.error) * 100)


tempTest %>% mutate(Prediction = pred[,2]) %>%
  mutate(Success = Prediction == tempTest$tomorrowsCandle)  %>%
  select(Prediction,tomorrowsCandle, Success, everything()) -> res

mean(res$Success, na.rm = T) * 100 -> percentSuccess


# Calculate RMSE:
RMSE.NN   <-  RMSE(tempTest, pred[,2]) 
MAE <- MeanAbsoluteError(tempTest, pred[,2]) 
     
# Adding the Caret RMSE to the results data frame:
paste("Neuralnet Model #",i + 1," validation: ",startTrain,":",
      startTrain + sizeTrain," // ",
      startTest,":",startTest + sizeTest," "
      ) -> modelNN

crossValidationResults <- crossValidationResults %>% add_row(Model=modelNN, 
                               RMSE=RMSE.NN, 
                               MeanAbsoluteError = MAE,
                               Accuracy  = accuracy
                                 )

dfRMSENN[i + 1] <- RMSE.NN
dfMAENN[i + 1] <- MAE
dfACCNN[i + 1] <- accuracy


dfNeuralnet <-rbind(dfNeuralnet, 
                    data.frame(ValidationLoop=i + 1, 
                               Accuracy=accuracy,
                               RMSE=RMSE.NN))

} # END of the Cross-Validation For loop


mean(as.numeric( dfACCCARET)) / 100 -> caretMean 
mean(as.numeric( dfACCNN)) / 100 -> neuralnetMean

# Adding the averages to the results data frame:
crossValidationResults <- crossValidationResults %>% add_row(
  Model="  Caret Model averages: RMSE - MAE - Accuracy", 
                               RMSE=mean(as.numeric(dfRMSECARET)), 
                               MeanAbsoluteError = mean(as.numeric(dfMAECARET)),
                               Accuracy  = mean(as.numeric( dfACCCARET)) 
                                 )

# Adding the RMSE to the results data frame:
crossValidationResults <- crossValidationResults %>% add_row(
  Model="  NeuralNet Model averages: RMSE - MAE - Accuracy", 
                               RMSE=mean(as.numeric(dfRMSENN)), 
                               MeanAbsoluteError = mean(as.numeric(dfMAENN)),
                               Accuracy  = mean(as.numeric( dfACCNN))
                                 )





##########################################################
# 3.3.5 Accuracy & Performance Comparison - Apple Inc. Case
##########################################################



#After running the two models on the cross-validation loop, and adding the results
#to a dataframe, we can check the results:



crossValidationResults

  



#Also, we show a visualization of the Neural Network Models validation results :





boxplot((as.numeric(dfRMSECARET)), ylab = "RMSE", main = "RMSE Caret Model BoxPlot")



 #This boxplot, representing the results for the Caret Neural Network Model,
 #shows that the median RMSE across the 7 samples when 
# changing the training set and the validation set, is about 0.6700, and
# that the RMSE varies between approx. 0.6600 and 0.7100. Also, the boxplot
#expresses an approximately normal distribution of the results. The 50% of
# the results, spread between 0.6600 and 0.6900.


boxplot((as.numeric(dfRMSENN)), ylab = "RMSE", main = "RMSE NeuralNet BoxPlot")



# This graph refers to the NeuralNet Neural Network Model.
# The above boxplot shows that the median RMSE across the 7 samples when 
# changing the training set and the validation set, is about  1.13000, and
# that the RMSE varies between approx. 1.0200 and 1.18000. The distribution
# is mostly on the region of the RMSE 1.1000. 


plot( unlist(dfACCCARET))
plot( unlist(dfACCNN))



# The accuracy results for the Caret Models are above 50% in most of the cases.


x <- as.matrix( dfACCNN)
barplot(x,
main = "NeuralNet Neural Network Accuracy",
xlab = "Accuracy",
ylab = "Validation Loop",
names.arg = c("1", "2", "3", "4", "5", "6", "7"),
col = "darkred",
horiz = TRUE)



# Comparing accuracy for the two models:


ggplot(dfCaret, aes(x = ValidationLoop, y = Accuracy      )) +  # bar plot
  geom_col(size = 0.5,fill="cyan",colour="#006000") +
  neuralTheme
ggplot(dfNeuralnet, aes(x = ValidationLoop, y = Accuracy      )) +  # bar plot
  geom_col(size = 0.5,fill="cyan",colour="#006000")  +
  neuralTheme



# Comparing RMSE for the two models:


ggplot(dfCaret, aes(x = ValidationLoop, y = RMSE      )) +    # line plot
  geom_line(size = 2 , color="red", group = 1) +
  neuralTheme
ggplot(dfNeuralnet, aes(x = ValidationLoop, y = RMSE      )) +    # line plot
  geom_line(size = 2 , color="red", group = 1) +
  neuralTheme
  



# Comparing both accuracy and RMSE for the two models:
# Caret:


ggplot(dfCaret) + 
  geom_col(aes(x = ValidationLoop, y = Accuracy), size = 1
           ,fill="cyan",colour="#006000") + # , color = "darkblue", fill = "white"
  geom_line(aes(x = ValidationLoop, y = 50*RMSE), 
            size = 1.5 , color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./50, name = "RMSE")) +
  neuralTheme



#Neuralnet:




ggplot(dfNeuralnet) + 
  geom_col(aes(x = ValidationLoop, y = Accuracy), size = 1
           ,fill="cyan",colour="#006000") + # , color = "darkblue", fill = "white"
  geom_line(aes(x = ValidationLoop, y = 50*RMSE), 
            size = 1.5 , color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./50, name = "RMSE")) +
  neuralTheme



# From the comparison of the two graphs, can be drawn that the RMSEs for the
# NeuralNet Models are more unequally distributed than those of the Caret
# Models. There are also big differences between the accuracy of the models.
 
 
 

########################################################################################
########################################################################################
#   4.                        Results Section
########################################################################################
########################################################################################



# This section presents the modeling results and discusses the models performances,
# that have been tested using the validation sets.



##########################################################
# 4.1 Final Evaluation
##########################################################


 
# When starting the development of the present project, we fixed a minimal 
# threshold in order to evaluate the Models performances. 
# That minimum would be .51, i.e. 51 percent.
# If the performance of a Model surpasses that minimum, we could state that
# the specific model has attained the required goal.
# At this stage, we can compare the results obtained by the different Models, 
# and draw conclusions.
 

##########################################################
# 4.2  Results Conclusions & Model Performance Discussion
##########################################################


# While testing the different models against the corresponding Validation datasets,
# specifically when performing Cross validation by training-test K-fold Cross
# Validation Method, we got plenty of results measured by accuracy, RMSE and MAE
# standards.
                             

# The Neural Network Model rendered several results that were averaged in two
# mean global results, one for the Caret model and another for the Neuralnet
# model. These two averages will now be added to the Results Table, and
# compared with the results obtained from the Naive Bayes model, the Support
# Vector Machine model, and the Random Forest model.
 
 

##########################################################
# 4.2.1 Final Results Table
##########################################################


# The following are the final results for all of the models built, trained 
# and validated against the validation datasets:




caretMean + totalAccuracy -> totalAccuracy
results <- results %>% 
  add_row(Model="   Fifth Model : Neural Network Caret Model Accuracy", 
                               Accuracy = caretMean
                               )


neuralnetMean + totalAccuracy -> totalAccuracy
results <- results %>% 
  add_row(Model="   Sixth Model : Neural Network NeuralNet Model Accuracy", 
                               Accuracy = neuralnetMean
                               )



# Calculating the mean accuracy and adding it to the Results table:
results <- results %>% 
  add_row(Model="   Mean Accuracy", 
                               Accuracy = totalAccuracy / 6
                               )

# Printing the accuracies:
results 



#The above table shows the results obtained from the Models. 
#Starting from the Stocks Trading System expectations, which were the size of
#51 percent, or .51, we tested the models against the validation data, to find
#the performances done.

#The first Model, Support Vector Machine, using the Polynomial kernel, rendered 
#an accuracy of 49 percent, which is smaller than the required goals of 51 
#percent.

#The second Model, Support Vector Machine using the Sigmoid kernel, enhanced the
#performance of SVM to a result of .59, i.e. 8 percent bigger than the
#Stocks Trading System expectations of .51.

#The third Model was the Naive Bayes Model, and attained more that expected, 
#61 percent.

#The fourth Model, the Random Forest Model, performed far better than the previous
#results, and also than the expectations.

#Finally, two Neural Network models were developed and tested using the 
#method called K-fold Cross Validation. These models, which we called Caret Model
#and NeuralNet Model, produced an Accuracy of 0.5417256 and 0.5318246
#respectively, which are also better than the Stocks Trading System 
#expectations.

#The Neural Network Model, using the Caret software package, rendered an 
#accuracy of more than 54%.

#A Mean Accuracy of more than 56% was finally obtained.



########################################################################################
########################################################################################
#  5.                        Conclusions
########################################################################################
########################################################################################



# This section presents a brief summary of the report, its limitations and future work to be done.


##########################################################
# 5.1 Brief Summary of the Report
##########################################################



#  Following the exploratory section, comes the Models building section, which 
#  tries six different models to reach a better Machine Learning System performance.
#  These models started from two Support Vector Machines (SVM) models, which used
# two different kernels, the Polynomial kernel and the Sigmoid Kernel, and, passing
# through a Naive Bayes Model, they progressively became more complicated, with the
# utilization of a Random Forests Model, and finally, a Neural Network Model,
# developed using resilient back-propagation with weight backtracking.
 
 
#  After training the different models, it stand to reason that the two best models
#  were the Random Forest Model, which reached an accuracy of 62%, and the
#  Naive Bayes Model, with an accuracy of 61%.
  
#  However, the performances rendered by the Neural Network Models, which were
#  solidly tested against several validation sets, were fairly good, better than
#  the Stocks Trading System expectations. These models achieved the desired
#  performance, with an RMSE and MAE pretty good, at least in the Caret Model
#  average case, which produced an RMSE of 0.6766180 and a Mean Absolute 
#  Error (MAE) of 0.4582744.
     
#  The Neural Network Model, using the Caret software package, rendered an 
#  accuracy of more than 54%.

#  All Models put together, a Mean Accuracy of more than 56% was attained, which
#  represents a good improvement over the project goals fixed at the beginning.


 # In short, can be stated that on a whole the Predictive Models yield a fairly good
 # accuracy, better than the Stocks Trading System expectations.



##########################################################
# 5.2 Limitations
##########################################################



 # The Models developed and the Data Analysis performed, were somehow limited by
#  the home laptop available to run this Machine Learning techniques, because of the
#  required amount of memory needed.

#  Although the Stocks Trading System succeeded on achieving a good accuracy, 
#  the hardware limitations didn't allow for a very more thorough analysis, such
#  as the one which could be performed on many years of daily stocks prices
#  data.
#  In addition to that, while developing the System, the need of doing Cross
#  validation by training-test K-fold Method, applied over many more years, 
#  appeared many times.
#  The completion of such a project was postponed because of the previously referred
#  hardware limitations.

#  Another issue refers to the need of necessary research concerning the enormously
#  diverse techniques regarding Technical Trading. The vast number of different
#  methods and Technical Indicators, that could be applied to this research,
#  limited definitely its scope, and open the need of new computations in order
 # to render more appropriate new predictions.


##########################################################
# 5.3 Future Work
##########################################################



#  Despite the fairly good results obtained while measuring the Stocks Market
#  Trading System accuracy, better results could be obtained by adding different
#  features to the predictors, or by thorough research of their impact on the
#  system performance. 
  
#  While rendering the prices data graphs, we used some Technical Analysis 
#  Indicators, specifically Bollinger Bands and Moving Average Convergence
#  Divergence (MACD), which for example could been also used as predictors. 
#  There are many more Trading Technical Analysis Indicators, such as the
#  Stochastic Oscillator, which represents oversold markets and overbought 
#  markets, the Relative Strength Index (RSI), Fibonacci Retracements, 
 # the Ichimoku Cloud, which identifies support and resistance levels and 
#  also estimates price momentum, the Average Directional Index (ADX), which
#  illustrates the strength of a price trend, and so on. Every one of these
#  Indicators can eventually be used as predictors for the System, which
#  in turn could produce more robust predictions.

#  By accessing stronger hardware, enhancements like doing Cross-validation by
 # training-test K-fold Method, applied over many years, and of course,
#  the training of the System on many years of daily stocks prices data, could 
#  be certainly added, leading to very more thorough analysis and to a more 
#  powerful Trading System, which eventually could be designed and developed.

         

# (c) 2021 - Carmel Shvartzman









