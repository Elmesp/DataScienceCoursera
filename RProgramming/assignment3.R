outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
setwd(dir="/home/hindol/Workspace/DataAnalysis/HW3/")
state <- "SC"
outcome <- "heart attack"

best <- function(state, outcome) {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(df$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  df2 <- df[df$State == state, ]
  df2[, c(11, 17, 23)] <- sapply(df2[, c(11, 17, 23)], as.numeric)
  df2 <- df2[order(df2[, 2]), ]
  
  if (outcome == "heart attack") {
    best <- df2[which.min(df2[, 11]), "Hospital.Name"]
  } else if (outcome == "heart failure") {
    best <- df2[which.min(df2[, 17]), "Hospital.Name"]
  } else {
    best <- df2[which.min(df2[, 23]), "Hospital.Name"]
  }
  
  best
}

df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
df <- df[c(2, 7, 11, 17, 23)]
df[, c(3, 4, 5)] <- sapply(df[, c(3, 4, 5)], as.numeric)

state <- "AK"
outcome <- "heart failure"
num <- 10L

rankhospital <- function(state, outcome, num = "best") {
  states <- unique(df$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  df2 <- df[df$State == state, ]
  
  if (outcome == "heart attack") {
    df2 <- df2[order(df2[, 3], df2[, 1]), ]
    df2 <- df2[!is.na(df2[, 3]), ]
  } else if (outcome == "heart failure") {
    df2 <- df2[order(df2[, 4], df2[, 1]), ]
    df2 <- df2[!is.na(df2[, 4]), ]
  } else {
    df2 <- df2[order(df2[, 5], df2[, 1]), ]
    df2 <- df2[!is.na(df2[, 5]), ]
  }
  
  if (num == "best") {
    num <- 1L
  } else if (num == "worst") {
    num <- nrow(df2)
  } else {
    num <- as.numeric(num)
  }
  
  df2[num, 1]
}

df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
df <- df[c(2, 7, 11, 17, 23)]
df[, c(3, 4, 5)] <- sapply(df[, c(3, 4, 5)], as.numeric)

rankall <- function(outcome, num = "best") {
  states <- unique(df$State)
  states <- sort(states)
  
  ranks <- data.frame(hospital=NA, state=NA)
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
  }
  
  ranks
}

df <- df[order(df[, 4]), ]
df <- df[!is.na(df[, 4]), ]