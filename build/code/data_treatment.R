## Create treatment dummy for earthquake struck

## Setup -----
# Set the working directory to access temp data
setwd("~/R project/Code replication/build/temp")

# Empty workspace
rm(list = ls())
source_data <- "data_merged.RData"
# Load filter data
load(source_data)

## Function to calculate great-circle distance -----
# Preparing for function.
GreatCircleDist <- function(LatA, LonA, LatB, LonB, R){
  # Formula for great-circle distance
  # With point A and B's longitude and latitude LatA, LonA, LatB, LonB
  # The central angle: arccos(sin LatA sin LatB + cos LatA cos LatB cos |LonA - lonB|)
  # The GC distance: R * central angle
  
  # Set the radius as mean earth radius
  if (missing(R)) {
    R = 6371
  }
  
  # Check if lon and lat are measured in pi
  if (any(abs(c(LatA, LonA, LatB, LonB)) > pi)){
    warning("Coordinate beyond range 2*Pi")
  }
  
  # Calculate central angle C
  C=sin(LatA)*sin(LatB) + cos(LatA)*cos(LatB)*cos(abs(LonA-LonB))
  
  # Calculate GC dist
  dist = R*acos(C)
  return(dist)
}

## Function to asign treatment -----
StrikeTreatment <- function(sampleData, quake, period){
  
  # Calculate the dist to strikes
  # Asign treatment group by dist comparing with MMI5 and MMI1 dists
  
  # Period can be either quarter or year
  # Refer lon and lat with pi
  quake[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
  sampleData[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
  
  # Initiate the treatment
    sampleData[, c("Struck", "Neighbor", "Depth", "Mag"):=0]
  
  distList <- sampleData[, lapply(1:nrow(quake), 
                                      function(x) GreatCircleDist(Lon, Lat, quake$Lat[x], quake$Lon[x]))]
  sampleData[, dist:=apply(distList, 1, FUN=min)]
  sampleData[, minInd:=apply(distList, 1, FUN=which.min)]
  
  sampleData[, distMMI5:= quake$MMI5[minInd]]
  sampleData[, distMMI1:= quake$MMI1[minInd]]
  
  validInd <- which(sampleData[[period]] %in%  quake[[period]])
  if (length(validInd)>0){
    
    sampleData[validInd, Depth:= quake$Depth[minInd]]
    sampleData[validInd, Mag:= quake$Mag[minInd]]
    
    sampleData[intersect(validInd, which(Lat<0.05)), Struck := 1]
    sampleData[intersect(validInd, which(dist>distMMI5 & dist<=distMMI1)), Neighbor := 1]  
  }
  
  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  

## Call the function -----

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
dropList <- c("eventSeason", "GreatCircleDist", "StrikeTreatment")
saveList <- lsList[!lsList %in% dropList]
# Save data to temp
save(list=saveList, file="data_treatment.RData")