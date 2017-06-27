
## This program uses crime rate data to calculate the crime rate by zip code in Louisville, KY


dataCache <- function(url = NULL){
    #to use this function you'll need to set dataCache to a variable
    #and call the subfunctions from the list that is returned
    getdata <- function() url
    setdata <- function(nurl) url <<- read.csv(nurl) #cache the data of a .csv
    list(getdata = getdata, setdata = setdata) 
}

# import data and store it with the dataCache function

crime.data <- dataCache$getdata()

crime.summary <- function(zip.code){
    #check that the correct dir is set
    #load cached data (check if it exists first)
    if(!exists("crime.data")){stop("Import the data using dataCache() first and set it to 'crime.data'.")}
    
    if(!is.null(crime.data$getdata())){
        crime.df <- crime.data$getdata()}
        else{stop("Import the data using dataCache() first and set it to 'crime.data'.")}
    
    crime.df <- crime.data$getdata()
    crime.df <- crime.df[,c("ZIP_CODE", "CRIME_TYPE")] #reduce data to crime and zip
    
    #using summary or table we find the zip code with the highest crime rate is 40203
    
    if(is.na(any(zip.code == crime.df[,"ZIP_CODE"]))){
        print(unique(crime.df[,"ZIP_CODE"]))
        stop("Check that your zip code matches a zip code above.")}
    
    #print(summary(crime.df[,"CRIME_TYPE"]))
    tot.crime <- nrow(crime.df)
    crime.df <- subset(crime.df, ZIP_CODE == zip.code) 
    
    crime.rate <- round((nrow(crime.df)/tot.crime)*100,1) #crime rate that occurs in specified zip of total crime
    crime.rate <- paste("Of the total crimes reported in Louisville, ", crime.rate, "% occurred in ", zip.code, sep = "")
    list(summary(crime.df[,"CRIME_TYPE"]), crime.rate)
}
