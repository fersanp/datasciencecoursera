rankhospital <- function(state, outcome, num = "best") {
  ## Check if outcome is valid
  if( !outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }

  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
  if(!state %in% data$"State"){
    stop("Invalid State!")
  }
  
  ## Validate the num value by checking if it is "best","worst", or a number.
  if( num != "best" && num != "worst" && !is.numeric(num) ) {
    stop("invalid num")
  }
  
  ## Get the needed columns from 'data' and create a new dataframe 
  ## with new names ("hospital", "state", "heart attack", etc)
  #"Hospital.Name"                                              
  #"State"                                                     
  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
  
  new_data <- data[c(2, 7, 11, 17, 23)]
  names(new_data)[1] <- "hospital"
  names(new_data)[2] <- "state"
  names(new_data)[3] <- "heart attack"
  names(new_data)[4] <- "heart failure"
  names(new_data)[5] <- "pneumonia"
  
  ## Get only the rows with our state value	
  new_data <- new_data[new_data$state==state & new_data[outcome] != 'Not Available', ]
    
  ## Order the data by name and then outcome
  new_data[outcome] <- as.data.frame(sapply(new_data[outcome], as.numeric))
  new_data <- new_data[order(new_data$hospital, decreasing = FALSE), ]
  new_data <- new_data[order(new_data[outcome], decreasing = FALSE), ]
  
  ## Process the num argument to get the row index
  vals <- new_data[, outcome]
  if( num == "best" ) {
    rowNum <- which.min(vals)
  } else if( num == "worst" ) {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  new_data[rowNum, ]$hospital
  } 


## Examples of usage
rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA