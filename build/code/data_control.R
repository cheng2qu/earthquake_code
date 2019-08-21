## Create control dummy for earthquake struck

## 4. Mark the treatment and controlled groups
fs$Struck <- 0
fs$Neighbor <- 0
fs$Depth <- 0
fs$Mag <- 0

# Mark the struck and neighbor group by the great-circle distance 
# betweeen semisic centers and firms

for (quake in 1:nrow(earthquake)){
  # Calculate the GC distance
  R = 6371.004
  LatA = earthquake$Lat[quake]/180*pi
  LatB = company$Lat[match(fs$Stkcd,company$Stkcd)]/180*pi
  LonA = earthquake$Lon[quake]/180*pi
  LonB = company$Lon[match(fs$Stkcd,company$Stkcd)]/180*pi
  
  C=sin(LatA)*sin(LatB) + 
    cos(LatA)*cos(LatB)*
    cos(abs(LonA-LonB))
  
  dist = R*acos(C)
  
  fs$Struck[which(fs$Season==earthquake$Season[quake] & fs$Struck==0)] <- 
    1*(dist<=earthquake$MMI5[quake])[which(fs$Season==earthquake$Season[quake] & fs$Struck==0)]
  
  fs$Neighbor[which(fs$Season==earthquake$Season[quake] & fs$Neighbor==0 & fs$Struck==0)] <-
    1*(dist>earthquake$MMI5[quake] & dist<=earthquake$MMI1[quake])[which(fs$Season==earthquake$Season[quake] & fs$Neighbor==0 & fs$Struck==0)]
  
  fs$Neighbor[fs$Season==earthquake$Season[quake] & fs$Struck==1] <- 0
  
  fs$Depth[fs$Season==earthquake$Season[quake]] <- earthquake$Depth[quake]
  
  fs$Mag[fs$Season==earthquake$Season[quake]] <- earthquake$Mag[quake]
}

fs$Year <- substr(fs$Accper,1,4)
# Lag of quake event
fs <- fs[, n_1 := shift(Neighbor*Mag, n=1L, type = "lead"), by = Stkcd]
fs <- fs[, s_1 := shift(Struck*Mag, n=1L, type = "lead"), by = Stkcd]

fs <- fs[, n1 := shift(Neighbor*Mag, n=1L, type = "lag"), by = Stkcd]
fs <- fs[, s1 := shift(Struck*Mag, n=1L, type = "lag"), by = Stkcd]
fs <- fs[, n2 := shift(Neighbor*Mag, n=2L, type = "lag"), by = Stkcd]
fs <- fs[, s2 := shift(Struck*Mag, n=2L, type = "lag"), by = Stkcd]
fs <- fs[, n3 := shift(Neighbor*Mag, n=3L, type = "lag"), by = Stkcd]
fs <- fs[, s3 := shift(Struck*Mag, n=3L, type = "lag"), by = Stkcd]
fs <- fs[, n4 := shift(Neighbor*Mag, n=4L, type = "lag"), by = Stkcd]
fs <- fs[, s4 := shift(Struck*Mag, n=4L, type = "lag"), by = Stkcd]

fs <- fs[, n5 := shift(Neighbor*Mag, n=5L, type = "lag"), by = Stkcd]
fs <- fs[, s5 := shift(Struck*Mag, n=5L, type = "lag"), by = Stkcd]
fs <- fs[, n6 := shift(Neighbor*Mag, n=6L, type = "lag"), by = Stkcd]
fs <- fs[, s6 := shift(Struck*Mag, n=6L, type = "lag"), by = Stkcd]
fs <- fs[, n7 := shift(Neighbor*Mag, n=7L, type = "lag"), by = Stkcd]
fs <- fs[, s7 := shift(Struck*Mag, n=7L, type = "lag"), by = Stkcd]
fs <- fs[, n8 := shift(Neighbor*Mag, n=8L, type = "lag"), by = Stkcd]
fs <- fs[, s8 := shift(Struck*Mag, n=8L, type = "lag"), by = Stkcd]