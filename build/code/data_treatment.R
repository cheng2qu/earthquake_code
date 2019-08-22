## Create treatment dummy for earthquake struck

## Setup -----
# Set the working directory to access temp data
setwd("~/R project/Code replication/build/temp")

# Empty workspace
rm(list = ls())
source_data <- "data_merged.RData"
# Load filter data
load(source_data)
# Load function GreatCircleDist() and StrikeTreatment()
source("~/R project/Code replication/build/code/treatment_function.R")

# Run StrikeTreatment for fs
fs[, c("Struck", "Neighbor", "Depth", "Mag")] <- StrikeTreatment(fs[,c("Quarter", "Lat", "Lon")], copy(earthquake),"Quarter")

fs[, c("MagS", "MagN") := lapply(.SD, "*", Mag), .SDcol=c("Struck", "Neighbor")]

# Other earthquake -----
fs$other <- 0
eventSeason <- c("20110331","20150630","20160630","20161231")
fs$other[fs$Season %in% eventSeason] <- 1

## Save treatment variable
# Select data to save
lsList <- ls()
dropList <- c("eventSeason")
saveList <- lsList[!lsList %in% dropList]
# Save data to temp
save(list=saveList, file="data_treatment.RData")