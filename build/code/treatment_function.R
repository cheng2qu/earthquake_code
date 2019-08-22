## Function GreatCircleDist() and StrikeTreatment()

## Function calculates great-circle distance -----
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

## Function asigns treatment -----
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
                                  function(x) GreatCircleDist(Lat, Lon, quake$Lat[x], quake$Lon[x]))]
  sampleData[, dist:=apply(distList, 1, FUN=min)]
  sampleData[, minInd:=apply(distList, 1, FUN=which.min)]
  
  sampleData[, distMMI5:= quake$MMI5[minInd]]
  sampleData[, distMMI1:= quake$MMI1[minInd]]
  
  validInd <- which(sampleData[[period]] %in%  quake[[period]])
  if (length(validInd)>0){
    
    sampleData[validInd, Depth:= quake$Depth[minInd]]
    sampleData[validInd, Mag:= quake$Mag[minInd]]
    
    sampleData[intersect(validInd, which(dist<=distMMI5)), Struck := 1]
    sampleData[intersect(validInd, which(dist>distMMI5 & dist<=distMMI1)), Neighbor := 1]  
  }
  
  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  