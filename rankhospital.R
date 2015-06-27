rankhospital <- function(state, outcome, num="best") {
    ## Read the data from the file.
    hospitalData <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
    
    
    ## Define Valid outcomes
    outcomeList <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check if the state is valid.
    if (!state %in% state.abb){
        stop ("invalid state")
    }
    
    ## Check if the outcome is valid.
    if (!outcome %in% outcomeList){
        stop ("invalid outcome")
    }
    
    ##str(hospitalData)
    ## Subset the data to the hospitals for the given state.
    stateHospitalData <- subset(hospitalData, State==state, select = c(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    ## Rename the columns to something shorter, but meaningful
    colnames(stateHospitalData) <- c("Hospital", "HeartAttack", "HeartFailure", "Pneumonia")
    
    ##return (stateHospitalData[complete.cases(stateHospitalData$HeartAttack),])
   ## Find the outcome data
   if (outcome == "heart attack")
   {
       sortedOutput <- stateHospitalData[complete.cases(stateHospitalData$HeartAttack),]
       sortedOutput <- sortedOutput[order(sortedOutput$HeartAttack, sortedOutput$Hospital),]
   }
   
   if (outcome == "heart failure")
   {
       sortedOutput <- stateHospitalData[complete.cases(stateHospitalData$HeartFailure),]
       sortedOutput <- sortedOutput[order(sortedOutput$HeartFailure, sortedOutput$Hospital),]
   }
   
   if (outcome == "pneumonia")
   {
       sortedOutput <- stateHospitalData[complete.cases(stateHospitalData$Pneumonia),]
       sortedOutput <- sortedOutput[order(sortedOutput$Pneumonia, sortedOutput$Hospital),]
   }   
   
   if (num == "best")
   {
       return(head(sortedOutput, n=1)[1,1])
   } else {
        if (num == "worst"){
            return(tail(sortedOutput, n=1)[1,1])
        }
   }
   return(sortedOutput[num,1])
}
