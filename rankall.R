rankall <- function(outcome, num = "best") {
  ## Check if outcome is valid
  if( !outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Validate the num value by checking if it is "best","worst", or a number.
  if( num != "best" && num != "worst" && !is.numeric(num) ) {
    stop("invalid num")
  }
  
  ## Rename columns names
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "hospital"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Filter valid rows
  data <- data[data[outcome] != 'Not Available', ]
  
  ## Order the data
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$hospital, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Find all states 
  states <- unique(data[, 2])
  newdata <- data.frame("hospital"=character(), "state"=character())
  
  ## For each state, find the hospital of the given rank
  for(st in states) {
    df <- data[data$state==st, ]
    vals <- df[, outcome]
    if( num == "best" ) {
      rowNum <- which.min(vals)
    } else if( num == "worst" ) {
      rowNum <- which.max(vals)
    } else {
      rowNum <- num
    }

    h <- df[rowNum, ]$hospital
    
    newdata <- rbind(newdata, data.frame(hospital=h, state=st))
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  newdata
}


## Examples of usage
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
head(rankall("heart attack", 20), 10)