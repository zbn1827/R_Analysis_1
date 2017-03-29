rankhospital <- function(state, outcome, num = "best") {
  
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

  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  
  
  if(outcome == "heart attack"){
    ranking <- sdata[order(as.numeric(sdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
    len <- nrow(sdata) - sum(sdata[,11]=="Not Available")
  }
  
  if(outcome == "heart failure"){
    ranking <- sdata[order(as.numeric(sdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
    len <- nrow(sdata) - sum(sdata[,17]=="Not Available")
  }
  
  if(outcome == "pneumonia"){
    ranking <- sdata[order(as.numeric(sdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
    len <- nrow(sdata) - sum(sdata[,23]=="Not Available")
  }
  
  if(is.numeric(num) && 0 < num && num <= len){
    return(ranking[num,2])
  }
  
  if(num == "best"){
    return(ranking[1,2])
  }
  if(num == "worst"){
    return(ranking[len,2])
  }
  
  else return("NA")
  
  

}
