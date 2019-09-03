## Split fs for quarterly observations, and
## fs2 for yearly observations

## Setup -----
# Empty workspace
rm(list = ls())

# Load filter data
load("build/temp/data_treatment.RData")

## Substract fs2 from year end observations -----
# Obtain fs2
fs2 <- fs[grepl("12/31", fs$Accper),]

# Redo treatment based on yearly earthquake strikes
fs2[, c("Struck", "Neighbor", "Depth", "Mag")] <- StrikeTreatment(fs2[,c("Year", "Lat", "Lon")], earthquake,"Year")

# Merge fs2 with div data
fs2 <- merge(fs2, div, by = c("Stkcd","Year","Accper"))

## Save the data tables
save.image("build/temp/data_split.RData")