## **********************************************************************
## The rankall function will accept two parameters and return a dataframe
## of hospitals were state that fall within a specific rank.
##
## Parameters:
##  outcome - String input of either "heart attack", "heart failure", or
##      pneumonia
##  num - defaults to "best", but can be "worst" or a numeric value > 1
##
## **********************************************************************
rankall <- function(outcome, num="best") {
    ## Read the data from the file.
    stateHospitalData <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
    
    ## Define Valid outcomes
    outcomeList <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check if the outcome is valid.
    if (!outcome %in% outcomeList){
        stop ("invalid outcome")
    }
    
    ## Subset the data to get a new data frame object with only the columns we are interested in looking at.
    ## Recycle the variable.
    stateHospitalData <- subset(stateHospitalData, select = c(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    ## Rename the columns to something shorter, but meaningful
    colnames(stateHospitalData) <- c("State","Hospital", "HeartAttack", "HeartFailure", "Pneumonia")
        
    ## Split the data by state, this will create a list
    hospitalByState <- split(stateHospitalData, stateHospitalData$State)
    
    ## Define an internal function that will be used to create the ranking and return the rank for the given
    ## data set.
    ## data - dataset
    ## num - rank that we want
    ## outcome = string for the items will be ranking on
    getHospitalRank <- function(data, num, outcome){
        
        ## Initialize internal variables
        column <- 0
        rank <- 1
        
        ## Identify the columns we will use to get the data from
        if (outcome == "heart attack")
        {
            column <- 3
        }
        
        if (outcome == "heart failure")
        {
            column <- 4
        }
        
        if (outcome == "pneumonia")
        {
            column <- 5
        }
        
        ## Set the rank number we will use when retrieving the data.
        if ((num == "best") | (num == "worst")){
            rank <- 1
        }
        else{
            rank <- num
        }
        
        ## Use the sapply function to:
        ## a. Get the completed cases
        ## b. Sort the data on the outcome and hospital
        ## c. Return the item in the giving ranking. If there are ties, we will retieve the first item in the tie
        ## since we are sorting on hospital name.
        rankedData <- sapply(data, function(x){
                # For the column that we want get the complete cases only. We don't want NA's 
                # [,column] <- means we are only interested in excluding the NA's from the column only.
                sortedOutput <- x[complete.cases(x[,column]),]
                
                # If the rank is "Worst", sort the data in reverse order by the outcome we are looking for. 
                # We still want to sort on the Hospital name in asc order
                if (num == "worst"){
                    # -x[,column] <- means the desc ordering of the column values in column position "column"
                    # x[order(-x[,column], x$Hospital),] <- means we want to sort the rows only based on the provided
                    #   arguments. x[ord...   ,     (nothing here)]
                    sortedOutput <- x[order(-x[,column], x$Hospital),]
                }
                else{
                    # If its anything else, sort in asc order for both outcome and hospital
                    sortedOutput <- x[order(x[,column], x$Hospital),]
                }
                
                # Return a combined vector of State and Hospital
                # rank = row
                # the number in the items below are the column positions of the data we want.
                c(sortedOutput[rank,2],sortedOutput[rank,1])
                ## ^ We could have also donw it this way: c(sortedOutput[rank,"Hospital"],sortedOutput[rank,"State"])
                
            })
        
        # Return the ranked data matrix
        rankedData
    }
    
    # Call the internal function and pass the split list, the desired rank, and outcome.
    rankedResults<-getHospitalRank(hospitalByState, num, outcome)

    # Convert the returned matrix into a data.frame that has columns called hospital and state
    # Matrix if 2 x N, where N is the numnber of state data. 
    # Row 1 [1,] - Contains the hospital name
    # Row 2 [2,] - Contains the state abbreviations
    # Columns will contain the Hospital/State combinations for each state in the file that represents the ranking requested.
    data.frame(hospital=rankedResults[1,],state=rankedResults[2,])
    
    ## TO TEST:
    ## Set working directory to location of csv file.
    ## setwd("~/rprog-data-ProgAssignment3-data")
    ##
    ## head(rankall("heart attack", 20), 10)
    ## tail(rankall("pneumonia", "worst"), 3)
    ## tail(rankall("heart failure"), 10)
}
