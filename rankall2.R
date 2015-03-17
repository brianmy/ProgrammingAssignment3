rankall2 <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- read.csv("outcome-of-care-measures.csv", na.strings ="Not Available")
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")
  
  index = list(`heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
  outindex = as.numeric(index[outcome])
  
  data = data[!is.na( data[outindex] ), ]
  
  hospstate = function( state ) {
    
    sdata <- data[data$State == state, ]
    
    if(num == "best")
      num = 1
    else if(num == "worst")
      num = sum(!is.na(sdata[,outindex]))
    
    if(num > nrow(sdata))
      return("NA")
    
    res <- order(sdata[,outindex], sdata[,2], na.last = NA)
    as.character( sdata$Hospital.Name[res[num]] )
    
  }
  
  states <- sort(as.character(unique(data$State)))
  res <- sapply(states, hospstate)
  
  ans = data.frame(res, states)
  colnames(ans) <- c("hospital", "state")
  ans
  
  
}