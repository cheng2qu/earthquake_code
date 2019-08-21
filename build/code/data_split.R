## Split fs for quarterly observations, and
## fs2 for yearly observations

## Setup -----
# Set the working directory to access temp data
temp_dir <- "~/R project/Code replication/build/temp"
setwd(temp_dir)

# Set link to temp data
source_data <- "data_treatment.RData"
# Load filter data
load(source_data)

## Substract fs2 from year end observations -----
# Obtain fs2
fs2 <- fs[grepl("12/31", fs$Accper),]

# Redo treatment based on yearly earthquake strikes
fs2[, c("Struck", "Neighbor", "Depth", "Mag")] <- StrikeTreatment(fs2[,c("Year", "Lat", "Lon")], earthquake,"Year")
