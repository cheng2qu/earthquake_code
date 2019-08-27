## Split fs for quarterly observations, and
## fs2 for yearly observations

## Setup -----
# Set the working directory to access temp data
setwd("~/R project/Code replication/build/temp")

# Empty workspace
rm(list = ls())
# Set link to temp data
source_data <- "data_treatment.RData"
# Load filter data
load(source_data)

## Substract fs2 from year end observations -----
# Obtain fs2
fs2 <- fs[grepl("12/31", fs$Accper),]

# Redo treatment based on yearly earthquake strikes
fs2[, c("Struck", "Neighbor", "Depth", "Mag")] <- StrikeTreatment(fs2[,c("Year", "Lat", "Lon")], earthquake,"Year")

# Merge fs2 with div data
fs2 <- merge(fs2, div, by = c("Stkcd","Year","Accper"))

## Save the data tables
save.image("data_split.RData")