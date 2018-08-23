

require("RODBC")
require("zoo")
require("fpp2")
require("dplyr")


############################# Forecast Functions ##############################

# These funcitons can be used to analyze forecast models

### Creating models and forecasts ###

getData <- function(databaseText, qryText){
  # Pull data from sql database
  # Throws error if query runs without error but doesn't return a dataframe
  # (Indicates there was an error on the SQL side which R doesn't catch)
  # Args:
  #   database_text: string of database name in ODBC connection
  #   qry_text: string of query to run in database
  # Returns:
  #   dataframe
  
  connection <- odbcConnect(databaseText)
  qryDf <- sqlQuery(connection, qryText)
  if(class(qryDf) != "data.frame"){
    odbcClose(connection)
    stop("The query did not return a dataframe. 
         Check the query again and re-run")
  }
  odbcClose(connection)
  return(qryDf)
  }

convertTs <- function(df, styleName, first=NULL, last=NULL){
  # Filter dataframe for product and create ts
  # Args:
  #   styleName: string of style name to filter
  #   df: dataframe to filter stlye name from and create ts
  #   first: vector of start date in ts. if NULL, MIN in series is used
  #   last: vector of end date in ts. if NULL, MAX in series is used
  # Returns 
  #   time series
  
  filteredDf <- filter(df, style_name == styleName)

  # if style name not in df, stop script
  if (nrow(filteredDf) == 0){
    stop(paste(styleName, "style name not found in dataframe. 
               Stopping script"))
  }
  
  if (is.null(first)){
    first <- min(filteredDf["capture_year"]) # get first year
    first <- c(first, min(filteredDf["capture_month"])) # get first month
  }
  if (is.null(last)){
    last <- max(filteredDf["capture_year"]) # get last year
    last <- c(last, 
              max(filteredDf[filteredDf["capture_year"]== last,]["capture_month"])) # get last month of last year
  }
  newTs <- ts(filteredDf["quantity"], start=first, end=last, frequency=12)
  return(newTs)
  }

### Fix so that unique style/gtins are filtered ###
createEtsList <- function(df, styleList=NULL, modelType="ZZZ"){
  # Create list of ETS models from a dataframe
  # Note: uses the convertTs function to get a ts object
  # Args
  #   df: dataframe to filtered by style name
  #   styleList: style name list to filter by. if no argument given, gets a list 
  #              of all unique style names in df
  #   modelType: 3-letter string that sets the type of model. if 'ZZZ',
  #              automatically chooses the model
  # Returns
  #   list of ets objects
  
  # get list of style names and gtins 
  if (is.null(styleList)){
    productList <- df %>% select(style_name) %>% unique
  } else {
    productList <- df %>% filter(style_name %in% styleList) %>% 
      select(style_name) %>% unique()
  }
  
  listLength <- nrow(productList)
  
  # monitor progress in for loop
  pbTitle <- "Model Creation Progress"
  pb <- winProgressBar(title=pbTitle, label = "%", 
                       min=0, max = listLength, width=300)
  
  # initiate ets objects list
  etsList <- vector(mode="list", length=listLength)
  for (i in seq_len(listLength)){
    
    # get progress feedback
    info <- sprintf("%d%% done", round((i/listLength)*100, 0))
    setWinProgressBar(pb, i, title=pbTitle, label=info)
    
    style <- toString(productList[i, "style_name"])
    etsTs <- convertTs(df=df, styleName=style)
    # if 0's contained in time series, auto select model since "MAM" is undefined
    if (0 %in% etsTs){
      etsList[[i]] <- ets(etsTs, model="ZZZ")     # NOTE: add lambda=0 here to ensure positive values
    }                                             # use biasadj=TRUE in the forecast function to get avg instead of median forecast point values
    else {
      etsList[[i]] <- ets(etsTs, model=modelType)
    }
    attr(etsList[[i]], "style") <- style # track style
    # print(paste(i, ": ", style))
  }
  close(pb)
  return(etsList)
}

createForecastList <- function(modelList, forecastPeriods){
  # Create forecasts for each model in modelList for a period set by months
  # 
  # Args
  #   modelList: list of model objects
  #   forecastPeriods: number of periods to forecast
  
  listLength <- length(modelList)
  forecastList <- vector(mode="list", length=listLength)
  
  # monitor progress in for loop
  pbTitle <- "FOrecast Creation Progress"
  pb <- winProgressBar(title=pbTitle, label="%",
                       min=0, max = listLength, width=300)
  
  i <- 0 # used to set list elements
  
  for (model in modelList){
    
    # get progress feedback
    info <- sprintf("%d%% done", round((i/listLength)*100, 0))
    setWinProgressBar(pb, i, title=pbTitle, label=info)
    
    i <- i + 1
    forecastList[[i]] <- forecast(model, h=forecastPeriods)
    # copy over attributes
    attr(forecastList[[i]], "style") <- attr(model, "style") 
  }
  close(pb)
  return(forecastList)
}

createNaiveList <- function(df, styleList=NULL){
  
  # get list of style names and gtins 
  if (is.null(styleList)){
    productList <- df %>% select(style_name) %>% unique
  } else {
    productList <- df %>% filter(style_name %in% styleList) %>% 
      select(style_name) %>% unique()
  }
  
  listLength <- nrow(productList)
  
  # monitor progress in for loop
  pbTitle <- "Naive Forecast Creation Progress"
  pb <- winProgressBar(title=pbTitle, label = "%", 
                       min=0, max = listLength, width=300)
  
  # initiate ets objects list
  naiveList <- vector(mode="list", length=listLength)
  for (i in seq_len(listLength)){
    
    # get progress feedback
    info <- sprintf("%d%% done", round((i/listLength)*100, 0))
    setWinProgressBar(pb, i, title=pbTitle, label=info)
    
    style <- toString(productList[i, "style_name"])
    naiveTs <- convertTs(df=df, styleName=style)
    naiveList[[i]] <- naive(naiveTs)
    attr(naiveList[[i]], "style") <- style # track style
    # print(paste(i, ": ", style))
  }
  close(pb)
  return(naiveList)
}


### Get Methods ###

getModel <- function(modelList, styleName){
  foundModel <- NULL
  for (model in modelList){
    if (attr(model, "style") == styleName){
      foundModel = model
    }
  }
  if (is.null(foundModel)){
    foundModel = "Style name not found."
  }
  return(foundModel)
}

getForecast <- function(forecastList, styleName){
  foundFcast <- NULL
  for (fcast in forecastList){
    if (attr(fcast, "style") == styleName){
      foundFcast <- fcast
    }
  }
  if (is.null(foundFcast)){
    foundFcast = "Style name not found."
  }
  return(foundFcast)
}

getModelMethod <- function(modelList){
  # Get the list of gtins the model selected by the ets function
  # Args
  #   modelList: list of forecast objects
  # Returns
  #   dataframe with gtin and corresponding model selected
  
  modelMethodDf <- data.frame("Style"=NA, "Method"=NA)
  for (model in modelList){
    newDf <- data.frame("Style"= attr(model, "style"), "Method"=model$method)
    modelMethodDf <- rbind(modelMethodDf, newDf)
  }
  modelMethodDf <- na.omit(modelMethodDf) # get rid of first row NA's
  return(modelMethodDf)
}

forecastToDataframe <- function(fcast){
  # Convert forecast to dataframe and extract all information including
  # observed values, fitted values, point forecasts, and prediction intervals
  #
  # Args
  #   fcast: forecast object
  # Returns
  #   data.frame
  
  if (class(fcast) != "forecast"){
    stop("Must pass forecast object.You passed an object of class '"
         , paste(class(fcast)),"' Exiting function.")
  }
  
  # get observed values
  df <- as.data.frame(fcast$x)
  names(df) <- 'observed'
  df$date <- as.Date(time(fcast$x))

  # get fitted values
  fittedDf <- as.data.frame(fcast$fitted)
  names(fittedDf) <- 'fitted'
  fittedDf$date <- as.Date(time(fcast$fitted))

  # merge to one dataframe
  df <- merge(df, fittedDf)

  # get forecast values and prediction intervals, then merge to one
  fcastDf <- as.data.frame(fcast)
  fcastDf$date <- as.Date(time(fcast$mean))
  levels <- fcast$level # get prediction interval levels
  names(fcastDf) <- c("forecast", paste("Lo", levels[1]), paste("Hi", levels[1]),
                      paste("Lo", levels[2]), paste("Hi", levels[2]), "date")
  
  # merge all 
  
  df <- merge(df, fcastDf, all.x=TRUE, all.y=TRUE) # outer join

  # add style to df
  df <- cbind(df, data.frame("style"=rep(attr(fcast, "style"), nrow(df))))
  
  # add model method to df
  df <- cbind(df, data.frame("method"=rep(fcast$method, nrow(df))))

  return(df)
}

### Plotting ###

plotForecast <- function(fcast){
  # Plots the forecast using autoplot with title of the style name
  # Args
  #   fcast: forecast object 
  # Returns 
  #   nothing. Plots forecast.
  
  style <- attr(fcast, "style")
  autoplot(fcast) + ggtitle(style)
}



plotForecastComparison <- function(fcast){
  # Plot a comparison of the forecast and naive forecast. Uses fitted values
  # for the observed periods and forecast values for the forecast period
  # Args
  #   fcast: forecast object
  # Returns
  #   Nothing. Plots observed and forecast values
  
  # get prediction interval levels
  lower <- toString(fcast$level[1])
  upper <- toString(fcast$level[2])
  col_levels <- c(paste("Lo", lower), paste("Hi", lower), 
                  paste("Lo", upper), paste("Hi", upper))
  # get style name to use as title
  style <- attr(fcast, "style")
  # create naive forecast to compare
  naiveForecast <- naive(fcast$x, h=length(fcast$mean))
  attr(naiveForecast, "style") = attr(fcast, "style") # copy style attribute
  # convert forecasts to dataframe for plotting
  fcastDf <- forecastToDataframe(fcast)
  naiveDf <- forecastToDataframe(naiveForecast)
  # plot
  ggplot(data=fcastDf) + 
    geom_line(aes(x=date, y=observed, color="actual", fill="actual", 
                  alpha="actual"), na.rm=TRUE) + 
    geom_ribbon(aes(x=date, ymin=fcastDf[col_levels[3]], 
                    ymax=fcastDf[col_levels[4]],color="upper",fill="upper",
                    alpha="upper"), na.rm=TRUE, linetype=0) + 
    geom_ribbon(aes(x=date, ymin=fcastDf[col_levels[1]], 
                    ymax=fcastDf[col_levels[2]],color="lower",fill="lower", 
                    alpha="lower"), na.rm=TRUE, linetype=0) + 
    geom_line(data=naiveDf, aes(x=date, y=forecast, color="rolling avg", 
                                fill="rolling avg", 
                                alpha="rolling avg"), na.rm=TRUE) + 
    geom_line(data=naiveDf, aes(x=date, y=fitted), color='orangered1', linetype="dashed", 
              na.rm=TRUE) +
    geom_line(aes(x=date, y=fitted), color='steelblue', linetype="dashed", 
              na.rm=TRUE) +
    geom_line(aes(x=date, y=forecast, color="forecast", fill="forecast", 
                  alpha="forecast"), na.rm=TRUE) + 
    scale_color_manual(name='series', # set colors for all lines
                       values=c("actual"="black",
                                "upper"=NA,
                                "lower"=NA,
                                "forecast"="steelblue",
                                "rolling avg"="orangered1"),
                       labels=c("actual"="actual", 
                                "upper"=upper, 
                                "lower"=lower, 
                                "forecast"="forecast", 
                                "rolling avg"="rolling avg"),
                       breaks=c("actual", 
                                "forecast", 
                                "upper", 
                                "lower", 
                                "rolling avg")) +
    scale_fill_manual(name='series', # set fill for prediction intervals
                      values=c("actual"=NA,
                               "upper"="steelblue2",
                               "lower"="steelblue2",
                               "forecast"=NA,
                               "rolling avg"=NA),
                      labels=c("actual"="actual", 
                               "upper"=upper, 
                               "lower"=lower, 
                               "forecast"="forecast", 
                               "rolling avg"="rolling avg"),
                      breaks=c("actual", 
                               "forecast", 
                               "upper", 
                               "lower", 
                               "rolling avg")) +
    scale_alpha_manual(name='series', # set alpha for prediction intervals
                       values=c("actual"=1,
                                "upper"=.2,
                                "lower"=.4,
                                "forecast"=1,
                                "rolling avg"=1),
                       labels=c("actual"="actual", 
                                "upper"=upper, 
                                "lower"=lower, 
                                "forecast"="forecast", 
                                "rolling avg"="rolling avg"),
                       breaks=c("actual", 
                                "forecast", 
                                "upper", 
                                "lower", 
                                "rolling avg")) +
    ylab("Captured Units") +
    xlab("Date") +
    ggtitle(style)
}


########## Testing forecasts ##########

getAccuracy <- function(forecastList){
  colNames <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Style")
  accuracyDf <- data.frame("ME"=double(),
                           "RMSE"=double(),
                           "MAE"=double(),
                           "MPE"=double(),
                           "MAPE"=double(),
                           "MASE"=double(),
                           "ACF1"=double(),
                           "Style"=character(),
                           stringsAsFactors = FALSE)
  for (fcast in forecastList){
    accuracyDf <- rbind(accuracyDf, 
                        cbind(accuracy(fcast), "Style"=attr(fcast, "style")), 
                        make.row.names=FALSE)
  }
  return(accuracyDf)
}

# Note: the below functions are to be used by creating forecasts with
# any number of months intentionally left out so that they can be tested
# against actuals

separateTestTrainDf <- function(totalDf, sortCol=NULL, forecastPeriods){
  # Separate training and testing data
  # Args
  #   totalDf: data.frame with all data to be split
  #   sortCol: if sorting needed, string of column to sort by. If
  #     NULL then no sorting is done
  #   forecastPeriods: number of forecast periods. Training data is 
  #     split by taking the number of forecastPeriods from the end
  #     of the data, by style name. Note: any styles that have less
  #     than or equal to the number of forecast periods are removed
  #     from both data sets
  # Returns
  #   Nothing. Sets the training set and testing set to the global
  #   variables 'trainDf' and 'testDf'
  
  # sort column if sortCol specified
  if (!is.null(sortCol)){
    totalDf <- arrange(totalDf, sortCol)
  }
  
  # remove styles with too few rows
  removeStyles <- data.frame(table(totalDf$style_name)) %>% 
    filter(Freq<=forecastPeriods) %>% select(Var1)
  removeStyles <- removeStyles[,]
  totalDf <- filter(totalDf, !(style_name %in% removeStyles))
  
  # separate dataframe into testing and training sets
  testDf <- totalDf[unlist(tapply(row.names(totalDf), totalDf$style_name, tail, 
                                  n=forecastPeriods)),]
  testDf <- arrange(testDf, style_name, capture_year, capture_month)
  trainDf <- anti_join(totalDf, testDf)
  trainDf <- arrange(trainDf, style_name, capture_year, capture_month)
  
  
  # assign to global variables
  assign("testDf", testDf, envir=.GlobalEnv)
  assign("trainDf", trainDf, envir=.GlobalEnv)
}

addNaiveForecast <- function(fcastDf, forecastPeriods){
  # adds a column to the forecast DF that shows the naive forecast for the
  # given time period. This function simply takes the value from the
  # previous month and replicates it down the number of forecastPeriods
  # Args
  #   fcastDf: data.frame of forecasts
  #   forecastPeriods: number of periods to be forecasted
  # Returns
  #   fcastDf dataframe with naive forecast values for the forecast periods
  
  naiveCol <- select(fcastDf, observed)
  naiveCol <- naiveCol[,] # convert to vector
  naiveCol <- c(NA, naiveCol[1:(length(naiveCol)-forecastPeriods)])
  naiveCol <- c(naiveCol, rep(naiveCol[length(naiveCol)], forecastPeriods-1))
  fcastDf <- cbind(fcastDf, naive=naiveCol)
  return(fcastDf)
}

addTestValues <- function(testDf, fcastDf, forecastPeriods){
  # adds the observed values back to the forecast dataframe
  # so one can compare observed to forecasted
  # Args
  #   testDf: test set dataframe. Can be created from separateTestTrainDf()
  #   fcastDf: forecast data.frame in which observed values are added
  #   forecastPeriods: number of periods forecasted
  # Returns
  #   dataframe. The original fcastDf with observed values added in
  
  observedCol <- select(fcastDf, observed)
  observedCol <- observedCol[,]
  observedCol <- observedCol[1:(length(observedCol)-forecastPeriods)]
  
  testCol <- select(testDf, quantity)
  testCol <- testCol[,]
  
  observedCol <- c(observedCol, testCol)
  fcastDf$observed <- observedCol
  
  return(fcastDf)
}

summarizeErrors <- function(df, actualCol="observed", forecastCol="forecast", 
                            naiveCol="naive"){
  # Summarize errors betwen observed vs forecasted values and observed vs
  # naive forecasted values. Currently uses MAE, RMSE, and MAPE error formulas
  # Args
  #   df: dataframe containing observed and forecasted values
  #   actualCol: string name of observed values column
  #   forecastCol: string name of forecasted values column
  #   naiveCol: string name of naive forecast values column
  # Returns
  #   dataframe of error values between forecast and naive forecast
  
  # remove NA's
  df <- df[!is.na(df[forecastCol]),]
  
  ob <- df[actualCol][,]
  fe <- abs(df[actualCol][,] - df[forecastCol][,])
  ne <- abs(df[actualCol][,] - df[naiveCol][,])
  
  fe <- fe[!is.na(fe)]
  ne <- ne[!is.na(ne)]
  
  mae <- c(mean(fe), mean(ne))
  rmse <- c(sqrt(mean(fe^2)), sqrt(mean(ne^2)))
  mape <- c(mean(fe/ob), mean(ne/ob))
  
  errors <- data.frame(mae, rmse, mape)
  row.names(errors) <- c("forecast", "naive")
  
  return(errors)
}


### Note: test residuals








