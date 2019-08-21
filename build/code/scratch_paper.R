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
