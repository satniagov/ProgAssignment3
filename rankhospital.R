rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
    
      df.outcome <- read.csv("outcome-of-care-measures.csv")
      index<- which(state==df.outcome$State)
      names<- df.outcome$Hospital.Name[index]
      if(num=="best") num<- 1
      
      
      ## Check that state and outcome are valid
      if(!(any(state==df.outcome$State))) stop("invalid state")
      if(!(any(outcome==c("heart attack","heart failure","pneumonia"))))stop("invalid outcome")
      ## Return hospital name in that state with the given rank
      ?order
      if(outcome=="heart attack") {
            heart.attack<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[index] 
            ranking<- cbind(heart.attack, names)[order(heart.attack,names),]
            if(num=="worst") num<- nrow(ranking[complete.cases(ranking),])
            
      }
      
      if(outcome=="heart failure") {
            heart.failure<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[index]
            ranking<- cbind(heart.failure, names)[order(heart.failure,names),]
            if(num=="worst") num<- nrow(ranking[complete.cases(ranking),])
            
      }
      
      if(outcome=="pneumonia") {
            pneumonia<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[index]
            ranking<- cbind(pneumonia, names)[order(pneumonia,names),]
            if(num=="worst") num<- nrow(ranking[complete.cases(ranking),])
            
      }
      
      ## 30-day death rate
      if (num>(nrow(ranking))) {print("NA")}
      else {ranking[num, 2]}
      
     
}