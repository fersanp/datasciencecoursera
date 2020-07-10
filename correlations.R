library("data.table")

## Write a function named 'pollutantmean' that calculates the mean of a 
## pollutant (sulfate or nitrate) across a specified list of monitors. 
## The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
## matter data from the directory specified in the 'directory' argument and returns the mean 
## of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332){
  ## Create an empty vector of pollutants
  pollutants <- c()
  
  for( i in id ){
    # Format number with fixed width and then append .csv to number
    fileName <- paste0(directory, '/', formatC(i, width=3, flag="0"), ".csv" )
    
    ## read in each file and store it in data
    data <- read.csv(fileName, header = TRUE)
    
    ##Concatenate the vectors from each file of the pollutant('sulfate' or 'nitrate') column 
    ##to pollutants vector
    pollutants = c(pollutants, data[,pollutant])
  }
  
  ## Get the mean of the pollutants and remove NA values
  pollutants_mean = mean(pollutants, na.rm=TRUE)
  
  ## Return the mean 'pollutants_mean'
  return(pollutants_mean)
}


# Example usage
pollutantmean(directory = '~/Desktop/specdata', pollutant = 'sulfate', id = 23)
## [1] 1.280833

pollutantmean("~/Desktop/specdata", "nitrate", 70:72)
## [1] 1.706047

pollutantmean("specdata", "sulfate", c(1,3,5))
## [1] 4.196983




## Write a function that reads a directory full of files and reports the number of 
## completely observed cases in each data file. 
## The function should return a data frame where the first column is the name of the file 
## and the second column is the number of complete cases. 
complete <- function(directory, id = 1:332){
  nobss = c()
  ids = c()
  
  for( i in id ){
    # Format number with fixed width and then append .csv to number
    fileName <- paste0(directory, '/', formatC(i, width=3, flag="0"), ".csv" )
  
    ## read in each file and store it in data
    datas <- read.csv(fileName, header = TRUE)
  
    ## Remove Na's
    complete <- datas[complete.cases(datas), ]
    
    ids = c(ids, i)
    nobss = c(nobss, nrow(complete))
  }
  ## Return the data frame
  data.frame(id=ids, nobs=nobss)
}

# Example usage
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)


## Write a function that takes a directory of data files and a threshold for complete cases 
## and calculates the correlation between sulfate and nitrate for monitor locations 
## where the number of completely observed cases (on all variables) is greater than the threshold. 
## The function should return a vector of correlations for the monitors that meet 
## the threshold requirement. If no monitors meet the threshold requirement, 
## then the function should return a numeric vector of length 0.
corr <- function(directory, threshold  = 0){
  ## Initialize empty vector variable
  correlations <- c()
    
  completes = complete("specdata", 1:332)
  completes_threshold = subset(completes, nobs > threshold)
  ## Get a list of filenames
  filenames = list.files(directory)
  
  for( i in completes_threshold$id){
    ## Concatinate the directory and filename
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ## Calculate and store the number of completed cases
    completeCases = data[complete.cases(data),]
    count = nrow(completeCases)
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  correlations
}


# Example usage
cr <- corr("specdata", 150)
head(cr)

