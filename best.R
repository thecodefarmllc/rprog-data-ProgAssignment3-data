best <- function(state, outcome) {
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
    
    
   ## Find the outcome data
   if (outcome == "heart attack")
   {
       sortedOutput <- stateHospitalData[order(stateHospitalData$HeartAttack, stateHospitalData$Hospital),]
   }
   
   if (outcome == "heart failure")
   {
       sortedOutput <- stateHospitalData[order(stateHospitalData$HeartFailure, stateHospitalData$Hospital),]
   }
   
   if (outcome == "pneumonia")
   {
       sortedOutput <- stateHospitalData[order(stateHospitalData$Pneumonia, stateHospitalData$Hospital),]
   }   
   
   return(sortedOutput[1,1])
}
