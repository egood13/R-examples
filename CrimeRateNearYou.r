
## this program uses crime rate data to calculate the crime rate by zip code in Louisville, KY

##########CrimerateNearYou.r##########




###########FUNCTIONS###########

data.cache <- function(url){
    # set data.cache to a variable to save dataframe
    return(read.csv(url))
}

check.exists <- function(data){
    
    if(!exists(as.character(data))){
        return(FALSE)
    }else if(is.null(data)){
        return(FALSE)
    }else {return(TRUE)}
}

selcol <- function(df, col.list){
    # if select multiple columns, use combine function c(...)
    # add error catching
    return(df[,col.list])
    
}

check.ValInCol <- function(df,col,val){
    
    if(is.na(any(val == df[,col]))){
        return(FALSE)
        # print(unique(crime.df[,"ZIP_CODE"]))}
        # "Check that your zip code matches a zip code above."
    }else {TRUE}
    }
    
subset.df <- function(df, col, val){

    return(subset(df, df[col] == val))
    
}

crime.rate <- function(df, pop = 756832){
    
    # Louisville city population is 756832
    # use subset.df isolate rows with a specified value
    rate <- (nrow(df)/pop)
    return(rate)
}


col.summary <- function(df, col){
    
    # returns a % summary of the data in selected column (col)
    summary <- summary(df[, as.character(col)])
    summary <- (summary/nrow(df))*100
    return(sort(summary, decreasing = TRUE))
}

#######SCRIPT/ANALYSIS##############

# Set directory

setwd("~/R/projects")

my.zip <- 40207 #select desired zip code

crime.data <- data.cache("Crime_Data_All.csv")

if(!check.exists("crime.data")){
    
    stop("Check that the working directory is set and that crime.data exists")

    }else if(!check.ValInCol(crime.data, "ZIP_CODE", my.zip)){
        
        print(unique(crime.data[,"ZipCode"]))
        stop("Check that your zip code matches a zip code above.")

    }

col.data <- selcol(crime.data, c("ZIP_CODE","CRIME_TYPE"))

col.data <- subset.df(crime.data, "ZIP_CODE", my.zip)

crime.rate(crime.data)

col.summary(crime.data, "CRIME_TYPE")
