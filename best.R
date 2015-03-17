best <- function(state, outcome) {
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
  
  ## Sort the data frame by input outcome; first step to rank each row and then create local dataframe to sort data by the rank 
  sorted_index <- sort.list(as.numeric(state_data[ , colname[outcome]]))
  sorted_data <- state_data[sorted_index, ]

  ## Based on the sorted data, read the corresponding hospital name
  hospital_name <- sorted_data[1, "Hospital.Name"]

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hospital_name
}