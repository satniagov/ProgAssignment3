##2 Finding the best hospital in a state
best <- function(state, outcome) {
      ## Read outcome data
      df.outcome <- read.csv("outcome-of-care-measures.csv")
      
      ## Check that state and outcome are valid
      if(!(any(state==df.outcome$State))) stop("invalid state")
      if(!(any(outcome==c("heart attack","heart failure","pneumonia"))))stop("invalid outcome")
      
      ## Get the names and index of the hospitals on that state
      index<- which(state==df.outcome$State)
      names<- df.outcome$Hospital.Name[index]
      
      ## Print best for heart attack
      if(outcome=="heart attack") {
            heart.attack<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[index] 
            minimo<- min((heart.attack)[complete.cases(heart.attack)])
            print(sort(names[which(minimo==heart.attack)])[1])
      }
      
      ## Print best for heart failure
      if(outcome=="heart failure") {
            heart.failure<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[index]
            minimo<- min((heart.failure)[complete.cases(heart.failure)])
            print(sort(names[which(minimo==heart.failure)])[1])
      }
      
      ## Print best for pneumonia
      if(outcome=="pneumonia") {
            pneumonia<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[index]
            minimo<- min((pneumonia)[complete.cases(pneumonia)])
            print(sort(names[which(minimo==pneumonia)])[1])
      }
      
}