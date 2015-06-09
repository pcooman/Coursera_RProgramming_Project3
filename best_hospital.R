best <- function(x,y) {
  # x = state
  # y = desired outcome
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (y == "heart attack") {
    cond_index <-11
  } else { 
    if (y == "heart failure") {
    cond_index <-17
    } else {
      if (y == "pneumonia") {
    cond_index <- 23
    } else {
      stop("invalid outcome")
      }
    }
  }
   
  if (!any(x == outcome$State)) {
    stop("invalid state")
  }
    
  outcome_by_state <- split(outcome, outcome$State)
  outcome_this_state <- outcome_by_state[[x]]
  
  # max(...,na.rm = TRUE)
    
  index_best <- min(as.numeric(outcome_this_state[,cond_index]),na.rm=TRUE) == as.numeric(outcome_this_state[,cond_index])
  
  best_hospital <- outcome_this_state$Hospital.Name[index_best]
  
  if (length(best_hospital) > 1) {
    best_hospital <- sort(best_hospital)
    best_hospital[1]
  } else {
    best_hospital
  }  
}