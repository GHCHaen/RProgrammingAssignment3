## This file defines a single method named rankhospital. This function uses the
## data located in outcome-of-care-measures.csv and returns the name of the
## hospital matching the rank specified for a specified state and outcome.

## Arguments:
##  state (character, length 2) - The state abbreviation for comparison
##  outcome (character) - The outcome on which to do the comparison
##      allowable values: heart attack, heart failure, pneumonia
##  rank (integer or character) - The ranking result desired
##      allowable values: integer - any, character - best, worst
## Output:
##  The name of the hospital with the specified rank for outcome results
##  for the given state and outcome. Ties will be ranked alphabetically. If the
##  rank specified exceeds the number of hospitals to choose from, the result
##  will be NA.
## Usage Examples:
##  > rankhospital("TX", "heart failure", 4)
##  [1] "DETAR HOSPITAL NAVARRO"
##  rankhospital("MD", "heart attack", "worst")
##  [1] "HARFORD MEMORIAL HOSPITAL"
##  rankhospital("MN, "heart attack", 5000)
##  [1] NA

rankhospital <- function(state, outcome, rank = "best")
{
    ## Read outcome data
    b_Data <- read.csv("data/outcome-of-care-measures.csv",
                       colClasses = "character")
    
    ## Valid list of outcomes with corresponding data column name
    b_Outcomes <- list("heart attack" = 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" =
                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = 
                           "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    ## Check that state, outcome and rank are valid
    if(!state %in% b_Data$State)
    {
        stop("Invalid State")
    }
    if(!outcome %in% names(b_Outcomes))
    {
        stop("Invalid Outcome")
    }
    if(class(rank) == "character" & !rank %in% c("best", "worst"))
    {
        stop("Invalid Rank")
    }
    
    ## Define Hospital Name, State and desired Outcome Columns
    b_HospitalNameColumn <- which(colnames(b_Data) == "Hospital.Name")
    b_StateColumn <- which(colnames(b_Data) == "State")
    b_OutcomeColumn <- which(colnames(b_Data) == b_Outcomes[outcome])
    
    ## Filter Data to Desired State Only
    b_Data <- b_Data[b_Data$State == state,]
    
    ## Temporarily suspend warnings for this call
    ## We are ignoring NA coercion warning in as.numeric call
    b_PriorWarningSetting <- getOption("warn")
    options(warn = -1)
    
    ## Make the outcome column numeric
    b_Data[, b_OutcomeColumn] <- as.numeric(b_Data[, b_OutcomeColumn])
    
    ## Restore warnings
    options(warn = b_PriorWarningSetting)
    
    ## Clear rows where specified Outcome has NA values
    b_Data <- b_Data[!is.na(b_Data[b_OutcomeColumn]),]
    
    ## Check if rank desired exceeds hospitals available, numberfy if character
    ## value (best/worst) was provided.
    if(class(rank) == "numeric" & rank > nrow(b_Data))
    {
        return(NA)
    }
    else if(rank == "best")
    {
        rank <- 1
    }
    else if(rank == "worst")
    {
        rank <- nrow(b_Data)
    }
    
    ## Order the Data by outcome then hospital name
    ## This makes the row number equivalent to rank
    b_Data <- b_Data[order(as.numeric(b_Data[,b_OutcomeColumn]),
                          b_Data[,b_HospitalNameColumn]),]

    ## Return hospital name in that state with the specified rank for 30-day
    ## death rate
    b_Data[rank, b_HospitalNameColumn]
}