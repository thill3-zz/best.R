best <- function(state, outcome) {
        ## Read outcome data
        setwd(
                "G:/My Drive/Coursera Data Science Specialization/rprog_data_ProgAssignment3-data/"
        )
        data <-
                read.csv(
                        "outcome-of-care-measures.csv",
                        stringsAsFactors = FALSE,
                        na.strings = c("Not Available") 
                )
        ## Check that state and outcome are valid
        if (!(state %in% state.abb)) {
                stop("invalid state")
        }
        if (tolower(outcome) == "heart attack") {
                col_index <- 11
        } else if (tolower(outcome) == "heart failure") {
                col_index <- 17
        } else if (tolower(outcome) == "pneumonia") {
                col_index <- 23
        } else {
                stop("invalid outcome")
        }
        row_TF <- data[,7] == state
        values <- data.frame(data[, 2], data[, 7], as.numeric(data[, col_index]), stringsAsFactors = FALSE) #collect the important data
                #Hospital.Name, State, data value
                #cbind doesn't quite work because it coerces the numeric values to character type
        #values[,3] <- as.numeric(values[,3]) Doesn't work
        values <- values[row_TF,] #limit to the relevant state
        values <- na.omit(values) #remove NAs
        order_for_values_index <- order(values[, 3], decreasing = FALSE)
        min_ind <- order_for_values_index[1]
        values[min_ind,1]
}

# source("best.R")
state <- "TX"
outcome <- "heart failure"
best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

## Return hospital name in that state with lowest 30-day death rate
# library(tools)
# outcome <- toTitleCase(outcome)
# print(outcome)
# column_name <-
#         paste("Hospital.30.Day.Death..Mortality..Rates.from.",
#               outcome,
#               sep = "")
# print(column_name)
# col_index <-
#         which(colnames(data) == column_name) #error here - the right
# #side evaluates to numeric(0)
# #it thinks none of the column names match the created string
# #identical returns false
# #all.equal returns "1 string mismatch"
# #str returns that each is a chr (character), and they look the same
# #The error might be that column_name needs the quotation marks
# print(col_index)
# min_value <- min(data[, col_index])
# print(min_value)
# row_index <- which(data[, col_index] == min_value)
# print(row_index)
# hospital_name <- data[row_index, 2]
# print(hospital_name)