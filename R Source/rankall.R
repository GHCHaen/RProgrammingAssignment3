## This file defines a single method named rankall. This function uses the
## data located in outcome-of-care-measures.csv and returns the name and state
## of the hospitals matching the rank specified for every state for a specified
## outcome.

## Arguments:
##  outcome (character) - The outcome on which to do the comparison
##      allowable values: heart attack, heart failure, pneumonia
##  rank (integer or character) - The ranking result desired
##      allowable values: integer - any, character - best, worst
## Output:
##  The name and state of the hospitals with the specified rank for outcome 
##  results will be given for every state. Ties will be ranked alphabetically.
##  If the rank specified exceeds the number of hospitals to choose from, the
##  result will be NA.
## Usage Examples:
##  > head(rankall("heart attack", 20), 3)
##                          hospital state
##                              <NA>    AK
##    D W MCMILLAN MEMORIAL HOSPITAL    AL
## ARKANSAS METHODIST MEDICAL CENTER    AR
## > tail(rankall("pneumonia", "worst"), 3)
##                                   hospital state
## MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
##                     PLATEAU MEDICAL CENTER    WV
##           NORTH BIG HORN HOSPITAL DISTRICT    WY

rankall <- function(outcome, rank = "best")
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
    
    ## Check that outcome and rank are valid
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
    
    ## Order the Data by state, outcome then hospital name
    ## This makes the row number equivalent to rank
    b_Data <- b_Data[order(b_Data[,b_StateColumn],
                           as.numeric(b_Data[,b_OutcomeColumn]),
                           b_Data[,b_HospitalNameColumn]),]
                           
    ## Split the data by state
    b_Data <- split(b_Data, b_Data$State)
    
    ## Create an output Data Frame with a record for each state
    b_Output <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                         c("hospital", "state"))
    
    ## Populate the output data
    for(b_State in b_Data)
    {
        ## Numberfy if character value (best/worst) was provided.
        if(rank == "best")
        {
            b_InnerRank <- 1
        }
        else if(rank == "worst")
        {
            b_InnerRank <- nrow(b_State)
        }
        else
        {
            b_InnerRank <- rank
        }
        
        b_Output <- rbind(b_Output, 
                          c(b_State[b_InnerRank, b_HospitalNameColumn],
                                    b_State[1, b_StateColumn]))
    }
    
    
    colnames(b_Output) <- c("hospital", "state")
    
    ## Name the rows in the output by state
    rownames(b_Output) <- b_Output$state
    
    ## Return the result
    b_Output
}