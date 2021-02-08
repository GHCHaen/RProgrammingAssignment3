## This file defines a single method named best. This function uses the data
## located in outcome-of-care-measures.csv and returns the name (or names in
## case of a tie) of the hospital with the best results for a specified outcome.

## Arguments:
##  state (character, length 2) - The state abbreviation for comparison
##  outcome (character) - The outcome on which to do the comparison
##      allowable values: heart attack, heart failure, pneumonia
## Output:
##  The name of the hospital with the best outcome results for the given state
##  and outcome. Ties will return all results ordered alphabetically.
## Usage Examples:
##   > best("TX", "heart attack")
##   [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
##   > best("TX", "heart failure")
##   [1] "FORT DUNCAN MEDICAL CENTER"
##   > best("MD", "heart attack")
##   [1] "JOHNS HOPKINS HOSPITAL, THE"
##   > best("MD", "pneumonia")
##   [1] "GREATER BALTIMORE MEDICAL CENTER"
##   > best("BB", "heart attack")
##   Error in best("BB", "heart attack") : Invalid State
##   > best("NY", "hert attack")
##   Error in best("NY", "hert attack") : Invalid Outcome

best <- function(state, outcome)
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
    
    ## Check that state and outcome are valid
    if(!state %in% b_Data$State)
    {
        stop("Invalid State")
    }
    if(!outcome %in% names(b_Outcomes))
    {
        stop("Invalid Outcome")
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
    
    ## Order the Data by outcome
    b_Data <- b_Data[order(as.numeric(b_Data[,b_OutcomeColumn])),]
    
    ## Restore warnings
    options(warn = b_PriorWarningSetting)
    
    ## Filter Data to rows with minimum value (matching first record)
    b_Data <- b_Data[b_Data[,b_OutcomeColumn] == b_Data[1,b_OutcomeColumn],]
    
    ## Return hospital name(s) in that state with lowest 30-day death
    ## rate, ordered alphabetically
    sort(b_Data[,b_HospitalNameColumn])
}