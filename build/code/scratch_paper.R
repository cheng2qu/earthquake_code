## Scratch paper
## Only for unit testing when necessary

# Data.table division among columns
# Unit test works
earthquake[, c("Date","Lat","Lon") :=
     lapply(.SD, "/", MMI1), .SDcols = c("Date","Lat","Lon")]

# Group samples into n equal-size chunks
# E.G, n=3, and return group index
x <- 2:18
as.numeric(cut(x,breaks = 3))

# Function passing list to argument
tttest <- function(date, earthquake){
  event <- earthquake[Date == date,]
  return(event)
}

# Unit test works
tttest("20070602", earthquake)

# Great-circle distance
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

# Unit test GreatCircleDist()
GreatCircleDist(earthquake$Lat[1],earthquake$Lon[1],earthquake$Lat[2],earthquake$Lon[2])

sapply(1:nrow(earthquake), function(x){
  GreatCircleDist(0.3, 0.1, earthquake$Lat[x],earthquake$Lon[x])
})

