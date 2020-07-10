library(tools)

# Histogram of the 30-day death rates from heart attack
histogram <- function(){
  outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  hist(outcome[, 11])
}


# Finding the best hospital in a state according to a particular outcome.
# The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
best <- function(state, outcome) {
  ## Read outcome data## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Retrieve State column  
  colState <- data$"State"
  
  ## Check that state and outcome are valid## Check that state and outcome are valid
  if(!state %in% colState){
    print("Invalid State!")
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    print("Invalid Outcome!")
  }
  
  ## Convert outcome to column title
  outcome <- toTitleCase(outcome)
  out_title <- gsub("\\s+", ".", paste("Hospital 30 Day Death. Mortality. Rates from ",outcome))
  
  ## Filter by state
  out_dt <- data[data$"State" == state, ] 
  
  ## Order data by outcome 
  out_dt[out_title] <- sapply(out_dt[out_title], as.numeric)
  
  # Removing Missing Values for numerical datatype (outcome column)
  out_dt <- out_dt[complete.cases(out_dt),]
  print(tail(out_dt[out_title]))
  
  out_dt <- out_dt[order(out_dt[out_title]),]

  rate <- out_dt[1,]$"Hospital.Name"
  ## Return hospital name in that state with lowest 30-day death
  rate
}

# Usage example
best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
#$[1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome