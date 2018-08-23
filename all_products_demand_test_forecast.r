
# rm(list=ls()) # clear memory

# setwd(paste("C:/Users/elliott.good/Desktop/Projects/",
#             "Inventory Stock Analysis/Forecasting", sep=""))

source("forecastFunctions.r") # user defined helper functions

library("RODBC")
library("fpp2")
library("zoo")
library("dplyr")


gtinsQuery = sqlQuery

# get list of gtin/style_name on auto replen
mapping <- read.csv("auto_replen_gtins.txt")
mapping <- mapping[,1] # convert to factor


print("Getting data...")
totalDf <- getData("Redshift", gtinsQuery)
print("Query successful, creating models...")
# filter totalDf to compare forecasts with actuals
# and create training and testing data
totalDf2 <- totalDf[!(totalDf$capture_year == 2018 & totalDf$capture_month > 6),]
h <- 1
separateTestTrainDf(totalDf2,forecastPeriods=h)
# create models and forecasts from training data
etsModels <- createEtsList(trainDf, styleList=mapping)
print(paste("Models created. Creating forecasts for", h, "periods..."))
etsForecasts <- createForecastList(modelList=etsModels, forecastPeriods=h)
print("Forecasts created. Converting values to dataframe...")


fcastDf <- data.frame()
for (fcast in etsForecasts){
  fcastDf <- rbind(fcastDf, forecastToDataframe(fcast))
}


print(paste("Forecast dataframe created. Creating a dataframe to compare", 
            "actuals with forecasts."))

styles <- unique(fcastDf$style)
compareDf <- data.frame()
for (styleName in styles){
  filteredFcastDf <- filter(fcastDf, style==styleName)
  filteredFcastDf <- addNaiveForecast(filteredFcastDf, h)
  filteredFcastDf <- addTestValues(filter(testDf, style_name==styleName), 
                                   filteredFcastDf, h)

  compareDf <- rbind(compareDf, filteredFcastDf)
}

View(summarizeErrors(compareDf, "observed", "forecast", "naive"))
write.csv(compareDf, "Compare DF.csv", row.names=FALSE)



###########
# split data into train/test data
# then compare accuracy of prediction compared to test
# for ETS-only models, ETS-Naive models, and Naive only models






