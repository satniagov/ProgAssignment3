##1 Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

##2 Finding the best hospital in a state
best <- function(state, outcome) {
      ## Read outcome data
      df.outcome <- read.csv("outcome-of-care-measures.csv")
      ##str(df.outcome)
      ## Check that state and outcome are valid
      ##Checking for validity of state
      ##outcome$State
      ##?stop
      ##state="AK"
      if(!(any(state==df.outcome$State))) stop("invalid state")
      if(!(any(outcome==c("heart attack","heart failure","pneumonia"))))stop("invalid outcome")
      ##print("Continue")
      ##table(outcome$State)
      index<- which(state==df.outcome$State)
      
      
      
      names<- df.outcome$Hospital.Name[index]
      if(outcome=="heart attack") {
            heart.attack<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[index] 
            minimo<- min((heart.attack)[complete.cases(heart.attack)])
            print(names[which(minimo==heart.attack)])
            }
      if(outcome=="heart failure") {
            heart.failure<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[index]
            minimo<- min((heart.failure)[complete.cases(heart.failure)])
            print(names[which(minimo==heart.failure)])
      }
      if(outcome=="pneumonia") {
            pneumonia<- as.numeric(df.outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[index]
            minimo<- min((pneumonia)[complete.cases(pneumonia)])
            print(names[which(minimo==pneumonia)])
      }
      
}
      
      ##df.outcome$ Hospital.Name 
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
          
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
     
