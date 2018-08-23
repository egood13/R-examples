
# rm(list=ls())

startTime <- proc.time()

# setwd(paste("C:/Users/elliott.good/Desktop/Projects/",
#             "Inventory Stock Analysis/Forecasting2", sep=""))

library("RODBC")
library("fpp2")
library("zoo")
library("dplyr")


source("forecastFunctions.r") # user defined helper functions

fgtinsQuery = sqlQuery

print("Getting data...")
fallDf <- getData("Redshift", fgtinsQuery)
# get list of gtin/style_name on auto replen
styleList <- read.csv("auto_replen_gtins.txt")
mapping <- styleList[,1] # convert to factor

print("Query successful, creating models...")

fetsModels <- createEtsList(fallDf, styleList=mapping, 
                            modelType = "MAM")
fh <- 6

print(paste("Models created. Creating forecasts for", fh, "periods..."))
fetsForecasts <- createForecastList(modelList=fetsModels, forecastPeriods=fh)


print("Writing model selection by style to Style Model Methods.csv")
# get model methods for each style
mm <- getModelMethod(fetsModels)
write.csv(mm, "Style Model Methods ETS Only.csv", row.names=FALSE)

print("Writing forecasts to Forecast Models.csv")
# get forecasts and export to excel
fcastDf <- data.frame()
for (fcast in fetsForecasts){
  fcastDf <- rbind(fcastDf, forecastToDataframe(fcast))
}
write.csv(fcastDf, "Forecast Models ETS Only.csv", row.names=FALSE)

print("Done. Reviewing models for bad fits and changing to Naive forecasts...")
# review forecasts and change ETS forecasts to naive for those with unusually
# high errors
fetsModels2 <- fetsModels
fetsForecasts2 <- fetsForecasts
fcastDf2 <- data.frame()
i = 0
for (fcast in fetsForecasts){
  i = i + 1
  # print(attr(fcast, "style"))
  if (accuracy(fcast)[1,"MAPE"] > 200){
    # get attributes to copy over
    styleAttr <- attr(fcast, "style")
    # replace model
    fetsModels2[[i]] <- "Naive Forecast"
    fetsModels2[[i]]$method <- "Naive Forecast"
    attr(fetsModels2[[i]], "style") <- styleAttr
    # replace forecast
    fcast <- naive(convertTs(styleName=attr(fcast,"style"), fallDf), h=fh)
    attr(fcast, "style") <- styleAttr
    # add to dataframe
    fcastDf2 <- rbind(fcastDf2, forecastToDataframe(fcast))
  } else {
    fcastDf2 <- rbind(fcastDf2, forecastToDataframe(fcast))
  }
}

# get model methods for each style
mm2 <- getModelMethod(fetsModels2)
write.csv(mm2, "Style Model Methods ETS-Naive.csv", row.names=FALSE)

write.csv(fcastDf2, "Forecast Models ETS-Naive.csv", row.names=FALSE)



totalTime <- proc.time() - startTime

print(paste("Finished. Script ran in", round(totalTime[3]/60,2), "minutes."))



######### Review Forecast Accuracy ##########

# compare full ETS-only models with Naive-only models and ETS-Naive mix models
# ETS-only
fetsAccuracy <- getAccuracy(fetsForecasts)
# Naive-only
fnaiveForecasts <- createNaiveList(fallDf, styleList=mapping)
fnaiveAccuracy <- getAccuracy(fnaiveForecasts)
# Mix
fetsAccuracy2 <- getAccuracy(fetsForecasts2)

# convert columns from factors to numeric 
fetsAccuracy[,1:7] <- sapply(fetsAccuracy[,1:7], as.character)
fetsAccuracy2[,1:7] <- sapply(fetsAccuracy2[,1:7], as.character)
fnaiveAccuracy[,1:7] <- sapply(fnaiveAccuracy[,1:7], as.character)

fetsAccuracy[,1:7] <- sapply(fetsAccuracy[,1:7], as.numeric)
fetsAccuracy2[,1:7] <- sapply(fetsAccuracy2[,1:7], as.numeric)
fnaiveAccuracy[,1:7] <- sapply(fnaiveAccuracy[,1:7], as.numeric)

fetsAccTotal <- sapply(fetsAccuracy[,1:7], sum)
fetsAccTotal2 <- sapply(fetsAccuracy2[,1:7], sum)
fnaiveAccTotal <- sapply(fnaiveAccuracy[,1:7], sum)

# show results
rbind(fetsAccTotal, fetsAccTotal2, fnaiveAccTotal)

