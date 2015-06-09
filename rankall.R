rankall <- function(y,num="best") {
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
    
  outcome_by_state <- split(outcome[,c(2,cond_index)], outcome$State)
  outcome_by_state <- lapply(outcome_by_state, function(x) data.frame(x[,1],as.numeric(x[,2])))
  ranked_by_state <- lapply(outcome_by_state, function(x) data.frame(x[order(x[,2],x[,1],na.last=NA),1],x[order(x[,2],x[,1],na.last=NA),2]))
  
  if (!is.numeric(num)) {
    if (num == "best") {
      select <- lapply(ranked_by_state, function(x) x[1,1])
    } else {
      if (num == "worst") {
        select <- lapply(ranked_by_state, function(x) x[length(x[,1]),1])
      } else {
        stop("invalid number")
      }
    }
  } else {
    select <- lapply(ranked_by_state, function(x) if (num>length(x[,1])) {NA} else {x[num,1]})
  }
  
  output <- c()
  for (i in 1:length(select)) {
    out<-as.character(select[[i]])
    output <- c(output,out)
  }
  output <- c(output,names(select))
  dim(output) <- c(length(select),2)
  rownames(output) <- names(select)
  colnames(output) <- c("hospital", "state")
  
  output <- data.frame(output)
  output
    
}