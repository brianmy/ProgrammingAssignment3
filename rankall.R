rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
  ## Check that outcome is valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Create object to map outcome parameter to column in data file
  colname <- c(11, 17, 23)
  names(colname) <- valid_outcomes
  
  ## Subset the data by removing NAs for the input outcome column
  data <- data[!is.na(data[colname[outcome]]), ]
  
  ## Function to return the name of the hospital for the input state and rank
  hospital_rank <- function(state) {    
    state_data <- data[data$State == state, ]

    ## Handle num input for "best" and "worse" values
    if(num == "best") {
      ## Set num to the index of the first record
      num <- 1
    }
    else if(num == "worst") {
      ## Set num to the index of the size of the dataframe to get the last record
      num <- nrow(state_data)
    }
    
    ## Sort the data frame with primary sort on outcome values and secondary sort on hospital name
    sorted_data <- order(as.numeric(state_data[, colname[outcome]]), state_data[, "Hospital.Name"])
    
    ## Return the name of the hospital for the input rank
    state_data$Hospital.Name[sorted_data[num]]
  }
  
  states <- sort(unique(data$State))
  record <- sapply(states, hospital_rank)
  
  ## Construct the results as a dataframe
  result = data.frame(record, states)
  colnames(result) <- c("hospital", "state")
  result
}

