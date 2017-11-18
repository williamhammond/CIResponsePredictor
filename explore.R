# Author: William Hammond
# Date  : 5/2/2017
# File  : 

setwd("/home/whammond/Classwork/Spring-2017/ind/src")
dat <- read.csv("../data/records.csv")

total.count <- dim(dat)[1]
recordtype.counts <- countdistinct(dat, "recordType")

jurisdiction.counts <- countdistinct(dat, "jusrisdiction")

initeventtype.counts <- countdistinct(dat, "initialEventType")
finaleventtype.counts <- countdistinct(dat, "finalEventType")

initpriority.counts <- countdistinct(dat, "initialPriority")
finalpriority.counts <- countdistinct(dat, "finalPriority")

carbeats.counts <- countdistinct(dat, "carBeat")

dat.prio_miss <- dat[dat$initialPriority != dat$finalPriority, ]
# Make sure the factors are the came
dat$initialEventType <- factor(dat$initialEventType, 
                               levels=levels(dat$finalEventType))
dat.event_miss <- dat[dat$initialEventType != dat$finalEventType, ]

# Function that creates a dataframe consisting of labeled counts of distinct
# elements of a certain attribute of a dataframe
# Input:
#       df   = dataframe holding the data
#       attr = attributed you want to count the distinct elements of 
# Output:
#       1 x n where n is the number of distinct attributes found
countdistinct <- function(df, attr) {
  distinct <- unique(df[,attr])
  result <- data.frame(matrix(ncol = length(distinct), nrow = 1))
  colnames(result) <- distinct  
  for (val in distinct) {
    result[,val] <- length(which(df[,attr] == val))  
  }
  return(result)
}