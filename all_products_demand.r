
# rm(list=ls())

startTime <- proc.time()

# setwd(paste("C:/Users/elliott.good/Desktop/Projects/",
#             "Inventory Stock Analysis/Forecasting2", sep=""))

library("RODBC")
library("fpp2")
library("zoo")
library("dplyr")


source("forecastFunctions.r") # user defined helper functions

fgtinsQuery = "WITH captured_orders AS (
	SELECT c.capture_year
,c.capture_month
,c.product_style_num || '-' || c.product_color || '-' || c.SIZE || '-' || c.gtin AS style_name 
,SUM(quantity) AS quantity
FROM (
SELECT DATE_PART(YEAR, so.capture_date) as capture_year
,DATE_PART(MONTH, so.capture_date) as capture_month
,DATE_PART(WEEK, so.capture_date) AS capture_week
,TRIM(sl.gtin) AS gtin
,TRIM(sl.product_name) AS product_name
,TRIM(sl.product_style_num) AS product_style_num
,TRIM(sl.SIZE) AS size
,INITCAP(TRIM(sl.color_name)) AS product_color
,sl.printer_id
,p.name AS printer_name
,sl.product_brand
,sl.product_class
,pg.name AS product_group
,sl.quantity
FROM rollups.super_orders so
JOIN rollups.super_line_items sl ON sl.order_id = so.order_id
JOIN printers p ON sl.printer_id = p.id
JOIN product_groups pg ON sl.product_group_id = pg.id
WHERE so.order_state_id = 3
AND EXTRACT (YEAR FROM so.capture_date) >= EXTRACT(YEAR FROM CURRENT_DATE) - 2
AND so.capture_date <= CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
) c
GROUP BY 1,2,3
)
SELECT d.num_year AS capture_year
,d.num_month AS capture_month
,UPPER(d.style_name) as style_name
,CASE WHEN c.quantity IS NULL THEN 0 ELSE c.quantity END AS quantity 
FROM ( -- generates list of month/year dates for each style_name
SELECT DISTINCT d.num_year
,d.num_month
,prod_list.style_name
FROM d_date d
CROSS JOIN (SELECT DISTINCT style_name FROM captured_orders) prod_list
WHERE EXTRACT(YEAR FROM dateobj) >= EXTRACT(YEAR FROM CURRENT_DATE) - 2
AND dateobj <= CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
ORDER BY 1,2
) d
LEFT JOIN captured_orders c ON d.num_year = c.capture_year
AND d.num_month = c.capture_month
AND d.style_name = c.style_name
ORDER BY 3,1,2"

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

