# Thesis - load all data

require(data.table)
require(zoo)

setwd("Z:/Documents/Thesis/Data_and_Rcode_40944/")
## Basic setting 

## 1. Earthquake locations, by events ---------------------------------
earthquake <- data.table(read.table("earthquake_chn_6.txt", header = TRUE))
names(earthquake) <- c( "Date","Lat","Lon","Depth","Mag","MMI6","MMI5","MMI1","Fatalities","EconLoss")
earthquake$Season <- round(earthquake$Date/10000,0)*10000+
  c(331, 630, 930, 1231)[findInterval(earthquake$Date%%10000, c(0, 331, 630, 930, 1231))] # Time stamp quake event by season

# earthquake$Season <- c(round(earthquake$Date/10000,0)*10000,(round(earthquake$Date/10000,0)-1)*10000)[
#   findInterval(earthquake$Date%%10000, c(0, 229, 1231))]+
#   c(1231, 331, 630, 930, 1231)[findInterval(earthquake$Date%%10000, c(0, 131, 430, 731, 1031, 1231))] # Time stamp quake event by season

## 2. Company loactions, by stock code ---------------------------------
company <- read.csv("city_location.csv", header = TRUE)
company$X <- NULL # Drop the city names

## 3. Companies financial statement data---------------------------------
fs <- data.table(read.csv("Cash_RD.csv")) # Financial statement data
names(fs) <- c("Stkcd","Accper","Reptyp","Cash","RD","TA","DebtL","TL","TE")
fs <- fs[fs$Reptyp=="A",]

fs <- fs[fs$Stkcd %in% company$Stkcd,] # Keep only shares listed before 2007
fs <- fs[fs$Reptyp=="A" & !grepl("/1/1",fs$Accper), # Drop financial situation on 01/01
         c("Stkcd","Accper","Cash","RD","TA","DebtL","TL")]
fs$Season <- with(fs, year(Accper)*10000 +month(Accper)*100+mday(Accper))
fs_midsm <- fs[fs$Stkcd>002000&fs$Stkcd<002999,] # Keep only MSE shares
fs <- fs[fs$Stkcd<002000|fs$Stkcd>002999,] # Keep only main board stock market

# Adjust to inflation
CPI <- read.csv("CPI.csv")
fs$Cash <- fs$Cash/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$RD <- fs$RD/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$TA <- fs$TA/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$TL <- fs$TL/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$DebtL <- fs$DebtL/CPI$CPI[match(fs$Accper,CPI$Season)]

# Load industrial data
ind <- read.csv("Industry.csv")
ind[, c("X","X.1")] <- NULL
ind <- ind[!duplicated(ind$Stkcd),]
fs <-merge(fs,ind,by = "Stkcd", all = FALSE)

fs$City <- company$Regplc[match(fs$Stkcd, company$Stkcd)] # Adding location factor into financial data
fs$City_Season <- paste(fs$City,fs$Season %% 10000,sep = "_") # City-season index
fs$Ind_Season <- paste(fs$Indcd,fs$Season %% 10000,sep = "_") # Industry-season index

# fs <- fs[which(fs$Stkcd>=900000&fs$Stkcd<1000000 | fs$Stkcd>=600000&fs$Stkcd<7000000),] # Keep only main board stocks

# Calculate the ratios
fs$TA[fs$TA==0] <- NA # Mark records where total asset is zero
fs$Cash_p <- fs$Cash/fs$TA # Calculate cash holding ratio
fs$Leverage <- log(fs$TL) - log(fs$TA-fs$TL) # Calculate leverage ratio
fs$Leverage[which(is.infinite(fs$Leverage)&fs$Leverage>0)] <- 9999 # Give value to infinte leverage ratio, i.e. TA=TL
fs$Leverage[which(is.infinite(fs$Leverage)&fs$Leverage<0)] <- -9999

# Subgroup selection
fs <- fs[, c("quanTA","quanOI","quanLev", "quanCash_p") := 
           list(findInterval(TA, quantile(TA,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9))),
                findInterval(OpIncome, quantile(OpIncome,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
                findInterval(Leverage, quantile(Leverage,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
                findInterval(Cash_p, quantile(Cash_p,na.rm = TRUE, probs = c(0,1/3,2/3)))#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
           ),   by = "Accper"]

# remove(fs_midsm)

fs <- fs[which(!is.na(fs$Cash_p)),] # Delete records when total asset is zero
fs <- fs[which(!is.na(fs$Leverage)),] # Delete records when total equity is negative

fs <- fs[, Cash_d := shift(Cash, n=1L, type = "lag"), by = Stkcd] # Lag the cash holding
fs <- fs[, RD_d := shift(RD, n=1L, type = "lag"), by = Stkcd] # Lag the RD
fs <- fs[, TA_d := shift(TA, n=1L, type = "lag"), by = Stkcd] # Lag the RD

fs <- fs[, Cash_p_d := shift(Cash_p, n=1L, type = "lag"), by = Stkcd] # Lag the cash holding ratio
fs <- fs[, Leverage_d := shift(Leverage, n=1L, type = "lag"), by = Stkcd] # Lag the leverage ratio
fs <- fs[, DebtL_d := shift(DebtL, n=1L, type = "lag"), by = Stkcd] # Lag the leverage ratio

fs <- fs[, Cash_c := log(Cash_p)-log(Cash_p_d), by = Stkcd] # Lag the cash holding ratio
fs$Cash_c[which(is.nan(fs$Cash_c))] <- NA
fs$Cash_c[which(is.infinite(fs$Cash_c))] <- NA

# One quarter forward
fs <- fs[, Cash_p_1f := shift(Cash_p, n=1L, type = "lead"), by = Stkcd] # Lead the cash holding ratio

# 4 seasons
fs$Season4 <- substr(fs$Season,5,8)

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

## 4. Operating income---------------------------------
Income <- data.table(read.table("OpIncome.txt", 
                                header = TRUE,
                                stringsAsFactors=FALSE)) # Operating income
Income <- Income[Income$Typrep == "A" & ! grepl("/01/01", Income$Accper),]
# Adjust to inflation
Income$OpIncome <- Income$OpIncome/CPI$CPI[match(Income$Accper,CPI$Season)]

fs <- merge(fs, Income[,.(Stkcd, Accper, OpIncome)], by=c("Stkcd", "Accper"), all.x = TRUE)
fs <- fs[, OpIncome_d := shift(OpIncome, n=1L, type = "lag"), by = Stkcd] # Lag the leverage ratio

## 5. Propertity insurance expenses---------------------------------

Insurance <- data.table(read.table("insurance.txt", 
                                   header = TRUE,
                                   stringsAsFactors=FALSE)) # Property nsurance expenses
# Adjust to inflation
Insurance$Amount <- Insurance$Amount/CPI$CPI[match(Insurance$Accper,CPI$Season)]

# Merge insurance and fs
Insurance <- merge(fs, Insurance, by = c("Stkcd","Accper"))
Insurance$Season <- with(Insurance, year(Accper)*10000 +month(Accper)*100+mday(Accper))
Insurance <- Insurance[,.(Stkcd, Accper, OpIncome,  Amount, Cash_p, Leverage, Cash_p_d, TA,
                          Season, Indcd, City, Cash_c, Cash_p_1f, Struck, Neighbor, Mag)]


# Calculate the coverage ratio
Insurance$Ins_p <- Insurance$Amount / (Insurance$OpIncome+0.01)
# Insurance$Ins_p <- Insurance$Amount / Insurance$TA

# One quarter lag
Insurance <- Insurance[, Ins_p_d := shift(Ins_p, n=1L, type = "lag"), by = Stkcd]
# One quarter forward
Insurance <- Insurance[, Ins_p_1f := shift(Ins_p, n=1L, type = "lead"), by = Stkcd]
# Adjust the lag cash holding ratio as last year, instead of last quater
# One quarter lag
Insurance <- Insurance[, Cash_p_d := shift(Cash_p, n=1L, type = "lag"), by = Stkcd]
# One quarter forward
Insurance <- Insurance[, Cash_p_1f := shift(Cash_p, n=1L, type = "lead"), by = Stkcd]

Insurance$Year <- substr(Insurance$Accper,1,4)
earthquake$Year <- substr(earthquake$Date,1,4)

# Mark the year with struck and neighbor group by the great-circle distance 
# betweeen semisic centers and firms

Insurance$Struck <- 0
Insurance$Neighbor <- 0
Insurance$Depth <- 0
Insurance$Mag <- 0

for (quake in 1:nrow(earthquake)){
  # Calculate the GC distance
  R = 6371.004
  LatA = earthquake$Lat[quake]/180*pi
  LatB = company$Lat[match(Insurance$Stkcd,company$Stkcd)]/180*pi
  LonA = earthquake$Lon[quake]/180*pi
  LonB = company$Lon[match(Insurance$Stkcd,company$Stkcd)]/180*pi
  
  C=sin(LatA)*sin(LatB) + 
    cos(LatA)*cos(LatB)*
    cos(abs(LonA-LonB))
  
  dist = R*acos(C)
  
  Insurance$Struck[which(Insurance$Year==earthquake$Year[quake]
                         & Insurance$Struck==0)] <-
    1*(dist<=earthquake$MMI5[quake])[which(Insurance$Year==earthquake$Year[quake]&
                                             Insurance$Struck==0)]
  
  Insurance$Neighbor[which(Insurance$Year==earthquake$Year[quake]
                           & Insurance$Neighbor==0 & Insurance$Struck==0)] <-
    1*(dist>earthquake$MMI5[quake] & dist<=earthquake$MMI1[quake])[which(Insurance$Year==earthquake$Year[quake]&
                                                         Insurance$Neighbor==0 & Insurance$Struck==0)]
  
  Insurance$Neighbor[which(Insurance$Year==earthquake$Year[quake] & Insurance$Struck==1)] <- 0
  
  Insurance$Depth[which(Insurance$Year==earthquake$Year[quake])] = earthquake$Depth[quake]
  
  Insurance$Mag[which(Insurance$Year==earthquake$Year[quake])] = earthquake$Mag[quake]
}
remove(LatA,LatB,LonA,LonB,dist,C,R,quake)

# Set the key
setkeyv(Insurance, c("Stkcd","Accper"))
setkeyv(fs, c("Stkcd","Accper"))

## 6. fs2, annual event treatment---------------------------------
# Subset of fs, end of the year x quake during the year
fs2 <- fs[grepl("12/31", fs$Accper),]
fs2 <- fs2[, Cash_p_d := shift(Cash_p, n=1L, type = "lag"), by = Stkcd]
# fs2 <- fs2[, Leverage_d := shift(Leverage, n=1L, type = "lag"), by = Stkcd]

fs2$Struck <- 0
fs2$Neighbor <- 0
fs2$Depth <- 0
fs2$Mag <- 0
for (quake in 1:nrow(earthquake)){
  # Calculate the GC distance
  R = 6371.004
  LatA = earthquake$Lat[quake]/180*pi
  LatB = company$Lat[match(fs2$Stkcd,company$Stkcd)]/180*pi
  LonA = earthquake$Lon[quake]/180*pi
  LonB = company$Lon[match(fs2$Stkcd,company$Stkcd)]/180*pi
  
  C=sin(LatA)*sin(LatB) + 
    cos(LatA)*cos(LatB)*
    cos(abs(LonA-LonB))
  
  dist = R*acos(C)
  
  fs2$Struck[fs2$Year==earthquake$Year[quake]
             & fs2$Struck==0] <-
    1*(dist<=earthquake$MMI5[quake])[fs2$Year==earthquake$Year[quake]&
                                       fs2$Struck==0]
  
  fs2$Neighbor[fs2$Year==earthquake$Year[quake]
               & fs2$Neighbor==0 & fs2$Struck==0] <-
    1*(dist>earthquake$MMI5[quake] & dist<=earthquake$MMI1[quake])[fs2$Year==earthquake$Year[quake]&
                                                   fs2$Neighbor==0 & fs2$Struck==0]
  
  fs2$Neighbor[fs2$Year==earthquake$Year[quake] & fs2$Struck==1] <- 0
  
  fs2$Depth[fs2$Year==earthquake$Year[quake]] = earthquake$Depth[quake]
  
  fs2$Mag[fs2$Year==earthquake$Year[quake]] = earthquake$Mag[quake]
}
remove(LatA,LatB,LonA,LonB,dist,C,R,quake)

fs2 <- merge(fs2, Insurance[, .(Stkcd,Accper,Ins_p)], by=c("Stkcd","Accper"), all.x=TRUE)
fs2$Ins_p[is.na(fs2$Ins_p)] <-0
fs2 <- fs2[, Ins_p_d := shift(Ins_p, n=1L, type = "lag"), by = Stkcd]
fs2 <- fs2[, Ins_p_1f := shift(Ins_p, n=1L, type = "lag"), by = Stkcd]

fs2 <- fs2[, n1 := shift(Neighbor*Mag, n=1L, type = "lag"), by = Stkcd]
fs2 <- fs2[, s1 := shift(Struck*Mag, n=1L, type = "lag"), by = Stkcd]
fs2 <- fs2[, n2 := shift(Neighbor*Mag, n=2L, type = "lag"), by = Stkcd]
fs2 <- fs2[, s2 := shift(Struck*Mag, n=2L, type = "lag"), by = Stkcd]
fs2 <- fs2[, n3 := shift(Neighbor*Mag, n=3L, type = "lag"), by = Stkcd]
fs2 <- fs2[, s3 := shift(Struck*Mag, n=3L, type = "lag"), by = Stkcd]
fs2 <- fs2[, n4 := shift(Neighbor*Mag, n=4L, type = "lag"), by = Stkcd]
fs2 <- fs2[, s4 := shift(Struck*Mag, n=4L, type = "lag"), by = Stkcd]

# 7. Dividend data---------------------------------

# Load dividend data
div <- data.table(read.csv("Div.csv"))
div <- div[(div$Typrep=="A" & grepl("/12/31",div$Accper)) |
             (div$Typrep=="A" & grepl("/06/30",div$Accper)) ,]
div$Payoff <- 1-div$Payoff
div$X <- substr(div$Accper,1,4)
div <- div[, DivT:= sum(Div,na.rm = TRUE), by=.(Stkcd,X)]
div <- div[Payoff>0, Payoff:= DivT/Div*Payoff, by=.(Stkcd,X)]
div <- div[grepl("/12/31",div$Accper),.(Stkcd,Accper, DivT, Payoff )]

div <- merge(div, fs2, by = c("Stkcd","Accper"))
div <- div[, Payoff_d := shift(Payoff, n=1L, type = "lag"), by = Stkcd]
div <- div[, Payoff_1f := shift(Payoff, n=1L, type = "lead"), by = Stkcd]

# 8. Load net income data---------------------------------
NI <- data.table(read.table("NI.txt", header = TRUE))
NI <- NI[Typrep=="A",]
NI <- NI[Typrep=="A" & grepl("/12/31",Accper),]
summary(NI[Stkcd %in% Neighbor$Stkcd & Accper %in% Neighbor$Accper,])

# 9. Other earthquake---------------------------------
fs$other <- 0
fs$other[fs$Season %in% c("20110331","20150630","20160630","20161231")] <- 1
fs$Season4 <- substr(fs$Season,5,8)

# # 10. Preannouncement---------------------------------
# forecast <- data.table(read.table("Forecast.txt", header = TRUE))
# forecast$Accper <- NULL
# setnames(forecast, "Announce","Accper")
# forecast <- merge(forecast, fs, by = c("Stkcd","Accper"), all = TRUE)
# forecast$Forecast <- as.character(forecast$Forecast)
# forecast$Forecast[is.na(forecast$Forecast)] <- ""
# forecast$Forecast <- factor(forecast$Forecast, levels = unique(forecast$Forecast))

# # 11. MA event ---------------------------------
# MA <- data.table(read.table("MA.txt", header = TRUE))
