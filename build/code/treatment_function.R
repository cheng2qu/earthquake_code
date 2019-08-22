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
  # Period can be either quarter or year
  if(missing(period)) {
    warning("Quarterly data as default")
    period <- "Quarter"
  }
  # Set a number big enough as Inf
  R <- 6000
  
  # Refer lon and lat with pi
  quake[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
  sampleData[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
  
  # Initiate the treatment
  sampleData[, c("Struck", "Neighbor", "Depth", "Mag"):=0]
  # Calculate the dist to seimic center regardless of time
  distList <- sampleData[, lapply(1:nrow(quake), 
                                  function(x) GreatCircleDist(Lat, Lon, quake$Lat[x], quake$Lon[x]))]
  # If accounting period has no earthquake, add R~6000 to dist
  # so this data point would be given any treatment based on dist
  distAdj <- sampleData[, lapply(1:nrow(quake), 
                                 function(x) (get(period)!= quake[[period]][x])*R)]
  distList <- distList + distAdj
  
  # Asign treatment group by dist comparing with MMI5 and MMI1 dists
  StruckList <- 1*distList[, mapply("<=", .SD, quake$MMI5)]
  # If struck, neighbor=0 regardless of other quake events
  NeighborList <- matrix((rowSums(StruckList)==0),20,16)*
    distList[, mapply("<=", .SD, quake$MMI1)]*
    distList[, mapply(">", .SD, quake$MMI5)]
  
  MagList <- cbind(t(matrix(quake$Mag,16,20))*StruckList,t(matrix(quake$Mag,16,20))*NeighborList)

  sampleData$Struck <- 1*(rowSums(StruckList)>0)
  sampleData$Neighbor <- 1*(rowSums(NeighborList)>0)
  sampleData$Mag <- apply(MagList, 1, FUN=max)
  a <- matrix(quake$Depth, 32,1)
  b <- apply(MagList, 1, FUN=which.max)
  sampleData$Depth <- a[b]*(sampleData$Struck+sampleData$Neighbor)

  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  