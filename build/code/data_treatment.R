## Create treatment dummy for earthquake struck

## Setup -----
# Set the working directory to access temp data
temp_dir <- "~/R project/Code replication/build/temp"
setwd(temp_dir)

# Set link to temp data
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
StrikeTreatment <- function(sampleData, earthquake, period){
  # Calculate the dist to strikes
  # Asign treatment group by dist comparing with MMI5 and MMI1 dists
  
  # Period can be either quarter or year
  
  # Initiate the treatment
    sampleData[, c("Struck", "Neighbor", "Depth", "Mag"):=0]
  
  distList <- sampleData[, lapply(1:nrow(earthquake), 
                                      function(x) GreatCircleDist(Lon, Lat, earthquake$Lat[x],earthquake$Lon[x]))]
  sampleData[, dist:=apply(distList, 1, FUN=min)]
  sampleData[, minInd:=apply(distList, 1, FUN=which.min)]
  
  sampleData[, distMMI5:=earthquake$MMI5[minInd]]
  sampleData[, distMMI1:=earthquake$MMI1[minInd]]
  
  validInd <- which(sampleData[[period]] %in% earthquake[[period]])
  if (length(validInd)>0){
    
    sampleData[validInd, Depth:=earthquake$Depth[minInd]]
    sampleData[validInd, Mag:=earthquake$Mag[minInd]]
    
    sampleData[intersect(validInd, which(Lat<0.05)), Struck := 1]
    sampleData[intersect(validInd, which(dist>distMMI5 & dist<=distMMI1)), Neighbor := 1]  
  }
  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  

## Call the function -----
# Refer lon and lat with pi
earthquake[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
fs[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]

# Run StrikeTreatment for fs
fs[, c("Struck", "Neighbor", "Depth", "Mag")] <- StrikeTreatment(fs[,c("Quarter", "Lat", "Lon")], earthquake,"Quarter")

fs[, c("MagS", "MagN") := lapply(.SD, "*", Mag), .SDcol=c("Struck", "Neighbor")]
## Save treatment variable
# Save data to temp
save.image("data_treatment.RData")