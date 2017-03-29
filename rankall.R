rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  
  setwd("A:/Desktop/R/week4/")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[order(data$Hospital.Name),]

  rankingdata <- NULL
  data.frame(rankingdata)
  
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    return("ERROR: invalid outcome")
  }
  
  
  for (state in sort(unique(data$State))){
  sdata <- data[data[,7] == state,]
  
  

  
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
    rankingdata <- rbind(rankingdata, (cbind(ranking[num,2],state)))
  }
  
    else if(num == "best"){
      rankingdata <- rbind(rankingdata, (cbind(ranking[1,2],state)))
    }
    else if(num == "worst"){
      rankingdata <- rbind(rankingdata, (cbind(ranking[len,2],state)))
    }
    
    else {rankingdata <- rbind(rankingdata, (cbind("NA",state)))
  }
  }
  rankingdata
}
