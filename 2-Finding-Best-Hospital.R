best <- function(state, outcome) {
  
  ## Read outcome data
  
  setwd("A:/Desktop/R/week4/")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[order(data$Hospital.Name),]
  sdata <- data[data[,7] == state,]
  ## Check that state and outcome are valid
  
  if(!(state %in% state.abb)) {
    return("ERROR: state is not valid")
  }
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    return("ERROR: invalid outcome")
  }

  if(outcome == "heart attack"){
    meanRow <- which(sdata[,11] == min(as.numeric(sdata[,11]), na.rm = TRUE))
  }
  
  else if(outcome == "heart failure"){
    meanRow <- which(sdata[,17] == min(as.numeric(sdata[,17]), na.rm = TRUE))
  }
  
  else if(outcome == "pneumonia"){
    meanRow <- which(sdata[,23] == min(as.numeric(sdata[,23]), na.rm = TRUE))
  }
  
  
  lowhospital <- sdata[meanRow,2][1]
  
  lowhospital

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
