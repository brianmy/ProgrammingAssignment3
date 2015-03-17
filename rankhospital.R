rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  valid_states <- data$State
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  if(!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
    
  ## Subset the datafile by the state parameter
  state_data <- data[data$State == state, ]

  ## Create object to map outcome parameter to column in data file
  colname <- c(11, 17, 23)
  names(colname) <- valid_outcomes
  
  # for the outcome column, remove corresponding rows that have NA
  state_data <- state_data[complete.cases(state_data[colname[outcome]]), ]
  
  ## Sort the data frame with primary sort on outcome values and secondary sort on hospital name
  ## Compute rank index of each row and then re-sort the data based on the index
  sorted_index <- order(as.numeric(state_data[ , colname[outcome]]), state_data[ , "Hospital.Name"])
  sorted_data <- state_data[sorted_index, ]

  # handle num input for "best" and "worse" values
  if(num == "best") {
    # set num to the index of the first record
    num <- 1
  }
  else if(num == "worst") {
    # set num to the index of the size of the dataframe to get the last record
    num <- nrow(sorted_data)
  }
  
  ## Based on the sorted data, read the corresponding hospital name for the input num
  hospital_name <- sorted_data[num, "Hospital.Name"]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hospital_name  
}