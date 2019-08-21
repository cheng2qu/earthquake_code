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

# Sapply works the same as metrix calculation
sapply(1:nrow(earthquake), function(x){
  GreatCircleDist(0.3, 0.1, earthquake$Lat[x],earthquake$Lon[x])
})
GreatCircleDist(0.3, 0.1, earthquake$Lat,earthquake$Lon)

## Function to asign treatment -----
StrikeTreatment <- function(sampleData, earthquake, period){
  # Calculate the dist to strikes
  # Asign treatment group by dist comparing with MMI5 and MMI1 dists
  
  # Period can be either quarter or year
  
  # If any quake happens in this quarter 
  if(sampleData[[period]] %in% earthquake[[period]]){
    # If no quake, all output arguments set to 0
    treatment <- list("Struck"=0, "Neighbor"=0, "Depth"=0, "Mag"=0)
  } else {
    # If earthquake(s), calculate a list of GC dist and take the min
    distList <- GreatCircleDist(sampleData$Lat, sampleData$Lon, earthquake$Lat, earthquake$Lon)
    dist <- min(distList)
    minInd <- which.min(distList)
    # Keep the quake data
    distMMI5 <- earthquake$MMI5[minInd]
    distMMI1 <- earthquake$MMI1[minInd]
    Depth <- earthquake$Depth[minInd]
    Mag <- earthquake$Mag[minInd]
    
    # Assign Struck/Neighbor/No harm
    if (dist<=distMMI5) {
      Struck <- 1
      Neighbor <- 0
    } else {
      if (dist>distMMI5 & dist<=distMMI1) {
        Struck <- 0
        Neighbor <- 1
      } else {
        Struck <- 0
        Neighbor <- 0
      }
    }
    treatment <- list(Struck, Neighbor, Depth, Mag)
  }
  
  return(treatment)
}  

# Unit test
sampleData <- fs[410:415, c("Quarter", "Lat", "Lon")]
StrikeTreatment(sampleData, earthquake,"Quarter")
sampleData[, c("Struck", "Neighbor", "Depth", "Mag"):=StrikeTreatment(sampleData, earthquake,"Quarter")]

sample <- fs[20:30, c("Quarter", "Lat", "Lon")]
sample[, sample[.I], by=.I]
sample[, sample[.I][["Quarter"]], by=.I]
sample[, sample[.I][["Quarter"]] %in% earthquake[["Quarter"]], by=.I]

sapply(1:nrow(sample), function(x) StrikeTreatment(sample[x,], earthquake,"Quarter"))

sample[, c("Struck", "Neighbor", "Depth", "Mag"):= sapply(1:nrow(sample), function(x) StrikeTreatment(sample[x,], earthquake,"Quarter"))]

treatment <- sapply(1:nrow(fs), function(x) StrikeTreatment(fs[x,], earthquake,"Quarter"))

# Matric calculation version
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
    
    sampleData[validInd & dist<=distMMI5, Struck := 1]
    sampleData[validInd & dist>distMMI5 & dist<=distMMI1, Neighbor := 1]  
  }
  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  

# Unit test
StrikeTreatment(fs[401:420,c("Quarter", "Lat", "Lon")], earthquake,"Quarter")
sample <- fs[401:420,c("Quarter", "Lat", "Lon")]
validInd <- 5:10
sample[intersect(validInd, which(Lat<0.05)), Lon:=-1]
sample[validInd ,  Lon:=-1]