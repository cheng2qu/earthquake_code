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
StrikeTreatment <- function(sampleData, quake, period){
  # Period can be either quarter or year
  
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
  
  # Substract the closet distance
  sampleData[, dist:=apply(distList, 1, FUN=min)]
  sampleData[, minInd:=apply(distList, 1, FUN=which.min)]
  
  sampleData[, distMMI5:= quake$MMI5[minInd]]
  sampleData[, distMMI1:= quake$MMI1[minInd]]
  
  # Asign treatment group by dist comparing with MMI5 and MMI1 dists
  sampleData[dist<=distMMI5, Struck := 1]
  sampleData[dist>distMMI5 & dist<=distMMI1, Neighbor := 1] 
  sampleData[dist<=distMMI1, Depth:= quake$Depth[minInd]]
  sampleData[dist<=distMMI1, Mag:= quake$Mag[minInd]]
  
  return(sampleData[, c("Struck", "Neighbor", "Depth", "Mag")])
}  

# Unit test
StrikeTreatment(fs[401:420,c("Quarter", "Lat", "Lon")], copy(earthquake),"Quarter")
sample <- fs[401:420,c("Quarter", "Lat", "Lon")]
validInd <- 5:10
sample[intersect(validInd, which(Lat<0.05)), Lon:=-1]
sample[validInd ,  Lon:=-1]

## Shift obs rbind to itself
DT <- data.table(year=rep(2010:2011, each=3), v1=1:6, v2=7:12)
DT[order(year), shift(.SD, 1:2, give.names = TRUE), by=year]

DT[, lapply(.SD, "*", year), .SDcol=2:3]
DT[, c("MagS","MagN") := lapply(.SD, "*", year), .SDcol=2:3]

DT[order(year), diff(v1, lag = 1), by=year]

# Local variable in function
f <- function(X,DT){
  DT[order(year), shift(.SD, 1:2, give.names = TRUE), by=year]
  return(X)
}
f(2,DT)
print(DT)
# At the end use copy()

# Keep/remove objects by type
inList <- ls()
objectList <- sapply(inList,get)
typeList <-sapply(objectList,typeof)
ls(typeof())

# Aggregate by earthquake event
StrikeTreatment(fs[401:420,c("Quarter", "Lat", "Lon")], copy(earthquake),"Quarter")
x <- fs[401:420,c("Quarter", "Lat", "Lon")]
x <- cbind(x,StrikeTreatment(x, earthquake, "Quarter"))

sapply(1:nrow(earthquake), function(x) {
  colSums(StrikeTreatment(fs[,c("Quarter", "Lat", "Lon")], copy(earthquake[x,]),"Quarter"))
})

quake1 <- StrikeTreatment(fs[,c("Quarter", "Lat", "Lon")], copy(earthquake[1,]),"Quarter")

quake1[Struck==1,]
quake1[Neighbor==1,]

earthquake[,list(N=sum(fs$Neighbor[fs$Quarter==Quarter]), S=sum(fs$Struck[fs$Quarter==Quarter])), by=Quarter] 
# Aggregate numbers of neighbor and stricken firms

earthquake_effect <- unique(fs$Quarter[fs$Neighbor==1 | fs$Struck==1]) # Aggregate earthquakes which affect listed firms
unique(earthquake[earthquake$Quarter %in% earthquake_effect,"Date"])


####################

quake <- copy(earthquake)
quake[, c("Lat", "Lon") := lapply(.SD, "/", 180*pi), .SDcol = c("Lat", "Lon")]
earthquake

Data <- fs[11401:11410,c("Quarter", "Lat", "Lon")]
Data$Quarter[1:3] <- c("2008 Q3","2014 Q3", "2018 Q3")
Data$Lon[1:3] <- c(34,30, 23)
Data$Lat[1:3] <- c(108,105, 100)

#####################
fs <- fs[,.(Quarter, Lat, Lon)]
Data <- copy(fs)
quake <- 1

####################
load("D:/Master thesis/RData_matched0.04.RData")

setwd("D:/Master thesis/Data_and_Rcode_40944")
rm(list = ls())

####################
stargazer(fs[, !c("Stkcd", "Struck", "Neighbor")],
          fs2[, !c("Stkcd", "Struck", "Neighbor")],
          header=FALSE,
          summary.stat = statList,
          flip = TRUE, 
          column.labels   = c("Quarterly data", "Annual data"),
          column.separate = c(5, 2),
          type='text')

stargazer(attitude[1:4,], 
          summary=FALSE, 
          rownames=FALSE,
          column.labels   = c("Quarterly data", "Annual data"),
          column.separate = c(5, 3))
####################
path_dir <- "~/R project/Code replication/analysis/output"
changeDir(path_dir)

capture.output(
  stargazer(fs[,!c("Stkcd", "Struck", "Neighbor")],
            fs2[, !c("Stkcd", "Struck", "Neighbor")],
            header=FALSE,
            summary.stat = statList,
            flip = TRUE, 
            type='text'),
  stargazer(fs[which(fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]),
               !c("Stkcd", "Struck", "Neighbor")],
            fs2[which(fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
                !c("Stkcd", "Struck", "Neighbor")],
            header=FALSE,
            summary.stat = statList,
            flip = TRUE, 
            type='text'),
  file = "Table8.txt")

stargazer(fs[which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1] & !fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]), 
             !c("Stkcd", "Struck", "Neighbor")],
          fs2[which(!fs2$Stkcd %in% fs2$Stkcd[fs2$Struck==1] & !fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
              !c("Stkcd", "Struck", "Neighbor")],
          header=FALSE,
          summary.stat = statList,
          flip = TRUE, 
          type='text',
          digits = 2,
          out = "temp.txt",
          title = "Panel c: Unaffected Firms")

######################
