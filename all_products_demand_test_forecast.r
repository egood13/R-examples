
# rm(list=ls()) # clear memory

# setwd(paste("C:/Users/elliott.good/Desktop/Projects/",
#             "Inventory Stock Analysis/Forecasting", sep=""))

source("forecastFunctions.r") # user defined helper functions

library("RODBC")
library("fpp2")
library("zoo")
library("dplyr")


gtinsQuery = "WITH captured_orders AS (
	SELECT c.capture_year
,c.capture_month
,c.product_style_num || '-' || c.product_color || '-' || c.SIZE || '-' || c.gtin AS style_name 
,SUM(quantity) AS quantity
FROM (
SELECT DATE_PART(YEAR, so.capture_date) as capture_year
,DATE_PART(MONTH, so.capture_date) as capture_month
,DATE_PART(WEEK, so.capture_date) AS capture_week
,sl.gtin
,sl.product_name
,sl.product_style_num
,sl.SIZE
,INITCAP(sl.color_name) AS product_color
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






