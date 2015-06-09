rankhospital <- function(x,y,num="best") {
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
  mortality <- as.numeric(outcome_this_state[,cond_index])
  hospitals <- outcome_this_state$Hospital.Name
  
  ranking<- order(mortality,hospitals,na.last=NA)
  ranked <- cbind(hospitals, mortality)[ranking,]
  
  if (!is.numeric(num)) {
    if (num == "best") {
      num = 1
    } else {
      if (num == "worst") {
        num = dim(ranked)[1]
      } else {
      stop("invalid number")
      }
    }
  } 
  
  if (num > dim(ranked)[1]) {
    NA
  } else {
    as.character(ranked[num,1])
  }
  
}