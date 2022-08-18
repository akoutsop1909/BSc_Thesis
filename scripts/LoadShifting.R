# Performs load shifting on the TimeZones structure

# CONTAINS FUNCTIONS AT THE END OF THE FILE

# Load packages
# =======================================================
library(dplyr)
library(glue)
library(lubridate)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Create time zone Vectors for 'Noise' checking
# =======================================================
W1 <- seq(dmy_hm("1-1-2010 7:00"), dmy_hm("1-1-2010 13:50"), by = "10 mins")
W1 <- paste(hour(W1), format(W1,"%M"), sep = ":")

W2 <- seq(dmy_hm("1-1-2010 14:00"), dmy_hm("1-1-2010 19:50"), by = "10 mins")
W2 <- format(W2, "%H:%M")

W3 <- seq(dmy_hm("1-1-2010 20:00"), dmy_hm("1-1-2010 21:50"), by = "10 mins")
W3 <- format(W3, "%H:%M")

W4 <- seq(dmy_hm("1-1-2010 22:00"), dmy_hm("2-1-2010 6:50"), by = "10 mins")
W4 <- paste(hour(W4), format(W4,"%M"), sep = ":")

# Prepare data frame TimeZones
# =======================================================
TZ <- read.csv2("TimeZones.csv", stringsAsFactors = FALSE)
TZ$Noise <- 0
TZ_Sort <- TZ[order(dmy(TZ$Date)),]

kwh_before <- sum(TZ$KWh)

# Create subset data frames
# =======================================================
Peak_L1 <- filter(TZ_Sort, Time_Zone == "Peak" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
Peak_L2 <- filter(TZ_Sort, Time_Zone == "Peak" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

Shoulder1_L1 <- filter(TZ_Sort, Time_Zone == "Shoulder 1" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
Shoulder1_L2 <- filter(TZ_Sort, Time_Zone == "Shoulder 1" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

Shoulder2_L1 <- filter(TZ_Sort, Time_Zone == "Shoulder 2" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
Shoulder2_L2 <- filter(TZ_Sort, Time_Zone == "Shoulder 2" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

# Initialize Variables
# =======================================================
a1 <- 0.1 # Changeable Value
d1 <- 0.5
e1 <- 0.2
b1 <- 0.1 # Changeable Value
g1 <- 0.5
c1 <- 0.1 # Changeable Value

Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Data Sampling - Load shifting
# =======================================================

# Peak_L1 shifting
for(i in 1:dim(Peak_L1)[1]) {
  dailyIndexes[k] <- Peak_L1$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Peak_L1)[1]) TZ <- Peak.shifting(dailyIndexes)
  
  # Previous Values
  else if (Peak_L1$Date[i+1] != Date) {
    TZ <- Peak.shifting(dailyIndexes)
    
    Date <- Peak_L1$Date[i+1]
    k <- 1
  }
}

# !!! Initializing Variables !!!
Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Peak_L2 shifting
for(i in 1:dim(Peak_L2)[1]) {
  dailyIndexes[k] <- Peak_L2$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Peak_L2)[1]) TZ <- Peak.shifting(dailyIndexes)
  
  # Previous Values
  else if (Peak_L2$Date[i+1] != Date) {
    TZ <- Peak.shifting(dailyIndexes)
    
    Date <- Peak_L2$Date[i+1]
    k <- 1
  }
}

# !!! Initializing Variables !!!
Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Shoulder1_L1 shifting
for(i in 1:dim(Shoulder1_L1)[1]) {
  dailyIndexes[k] <- Shoulder1_L1$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Shoulder1_L1)[1]) TZ <- Shoulder1.shifting(dailyIndexes)
  
  # Previous Values
  else if (Shoulder1_L1$Date[i+1] != Date) {
    TZ <- Shoulder1.shifting(dailyIndexes)
    
    Date <- Shoulder1_L1$Date[i+1]
    k <- 1
  }
}

# !!! Initializing Variables !!!
Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Shoulder1_L2 shifting
for(i in 1:dim(Shoulder1_L2)[1]) {
  dailyIndexes[k] <- Shoulder1_L2$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Shoulder1_L2)[1]) TZ <- Shoulder1.shifting(dailyIndexes)
  
  # Previous Values
  else if (Shoulder1_L2$Date[i+1] != Date) {
    TZ <- Shoulder1.shifting(dailyIndexes)
    
    Date <- Shoulder1_L2$Date[i+1]
    k <- 1
  }
}

# !!! Initializing Variables !!!
Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Shoulder2_L1 shifting
for(i in 1:dim(Shoulder2_L1)[1]) {
  dailyIndexes[k] <- Shoulder2_L1$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Shoulder2_L1)[1]) TZ <- Shoulder2.shifting(dailyIndexes)
  
  # Previous Values
  else if (Shoulder2_L1$Date[i+1] != Date) {
    TZ <- Shoulder2.shifting(dailyIndexes)
    
    Date <- Shoulder2_L1$Date[i+1]
    k <- 1
  }
}

# !!! Initializing Variables !!!
Date <- "4/1/2010"
dailyIndexes <- NA
k <- 1

# Shoulder2_L2 shifting
for(i in 1:dim(Shoulder2_L2)[1]) {
  dailyIndexes[k] <- Shoulder2_L2$Index[i]
  k <- k + 1
  
  # Last Value
  if (i == dim(Shoulder2_L2)[1]) TZ <- Shoulder2.shifting(dailyIndexes)
  
  # Previous Values
  else if (Shoulder2_L2$Date[i+1] != Date) {
    TZ <- Shoulder2.shifting(dailyIndexes)
    
    Date <- Shoulder2_L2$Date[i+1]
    k <- 1
  }
}

kwh_after <- sum(TZ$KWh)

TZ$Stop_Time <- NA
TZ <- TZ[order(TZ$Charge_Type, TZ$HV_Code, dmy(TZ$Date), TZ$HV_Code, hm(TZ$Start_Time), TZ$Charge_Duration),]

# Percentage Of 'Noise' (in KWh)
sum(TZ$Noise[TZ$Charge_Type%in%"L1"]) / sum(TZ$KWh[TZ$Charge_Type%in%"L1"]) * 100
sum(TZ$Noise[TZ$Charge_Type%in%"L2"]) / sum(TZ$KWh[TZ$Charge_Type%in%"L2"]) * 100

# Exporting TimeZones Data Frame to csv
# =======================================================
write.csv2(TZ, "Loadshifting01.csv", row.names = FALSE)

# Functions
# =======================================================
Peak.shifting <- function(dailyIndexes) {
  sampleDaily <- NA
  sampleIndexes1 <- NA
  sampleIndexes2 <- NA
  sampleIndexes3 <- NA
  sampleKWH <- 0
  pcKWH <- 0
  z <- 1
  
  tData <- filter(TZ_Sort, Index %in% dailyIndexes)
  sumKWH <- sum(tData$KWh)
  
  while (pcKWH < a1) {
    sampleDaily[z] <- sample(dailyIndexes, size = 1)
    dailyIndexes <- setdiff(dailyIndexes, sampleDaily)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleDaily[z]]
    pcKWH <- sampleKWH/sumKWH

    z <- z + 1
  }
  # ==
  tData <- filter(TZ_Sort, Index %in% sampleDaily)
  sampleKWH <- 0
  sumKWH <- sum(tData$KWh)
  pcKWH <- 0
  z <- 1
  
  while (pcKWH < d1) {
    sampleIndexes1[z] <- sample(sampleDaily, size = 1)
    sampleDaily <- setdiff(sampleDaily, sampleIndexes1)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleIndexes1[z]]
    pcKWH <- sampleKWH/sumKWH
    z <- z + 1
  }
  # ==
  sampleKWH <- 0
  pcKWH <- 0
  z <- 1
  
  while (pcKWH < e1) {
    sampleIndexes2[z] <- sample(sampleDaily, size = 1)
    sampleDaily <- setdiff(sampleDaily, sampleIndexes2)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleIndexes1[z]]
    pcKWH <- sampleKWH/sumKWH
    z <- z + 1
  }
  
  sampleIndexes3 <- setdiff(sampleDaily, sampleIndexes2)
  
  for(i in 1:length(sampleIndexes1)) TZ <- shifting(TZ, sampleIndexes1[i], "Shoulder 1")
  for(i in 1:length(sampleIndexes2)) TZ <- shifting(TZ, sampleIndexes2[i], "Shoulder 2")
  for(i in 1:length(sampleIndexes3)) TZ <- shifting(TZ, sampleIndexes3[i], "Off Peak")

  return(TZ)
}

Shoulder1.shifting <- function(dailyIndexes) {
  sampleDaily <- NA
  sampleIndexes1 <- NA
  sampleIndexes2 <- NA
  sampleKWH <- 0
  pcKWH <- 0
  z <- 1
  
  tData <- filter(TZ_Sort, Index %in% dailyIndexes)
  sumKWH <- sum(tData$KWh)
  
  while (pcKWH < b1) {
    sampleDaily[z] <- sample(dailyIndexes, size = 1)
    dailyIndexes <- setdiff(dailyIndexes, sampleDaily)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleDaily[z]]
    pcKWH <- sampleKWH/sumKWH

    z <- z + 1
  }
  # ==
  tData <- filter(TZ_Sort, Index %in% sampleDaily)
  sampleKWH <- 0
  sumKWH <- sum(tData$KWh)
  pcKWH <- 0
  z <- 1
  
  while (pcKWH < g1) {
    sampleIndexes1[z] <- sample(sampleDaily, size = 1)
    sampleDaily <- setdiff(sampleDaily, sampleIndexes1)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleIndexes1[z]]
    pcKWH <- sampleKWH/sumKWH
    
    z <- z + 1
  }
  
  sampleIndexes2 <- setdiff(sampleDaily, sampleIndexes1)
  
  for(i in 1:length(sampleIndexes1)) TZ <- shifting(TZ, sampleIndexes1[i], "Shoulder 2")
  for(i in 1:length(sampleIndexes2)) TZ <- shifting(TZ, sampleIndexes2[i], "Off Peak")
  
  return(TZ)
}

Shoulder2.shifting <- function(dailyIndexes) {
  sampleDaily <- NA
  sampleIndexes1 <- NA
  sampleKWH <- 0
  pcKWH <- 0
  z <- 1
  
  tData <- filter(TZ_Sort, Index %in% dailyIndexes)
  sumKWH <- sum(tData$KWh)
  
  while (pcKWH < c1) {
    sampleIndexes1[z] <- sample(dailyIndexes, size = 1)
    dailyIndexes <- setdiff(dailyIndexes, sampleIndexes1)
    
    sampleKWH <- sampleKWH + TZ$KWh[sampleIndexes1[z]]
    pcKWH <- sampleKWH/sumKWH
    
    z <- z + 1
  }
  
  for(i in 1:length(sampleIndexes1)) TZ <- shifting(TZ, sampleIndexes1[i], "Off Peak")
  
  return(TZ)
}

shifting <- function(df, ind, nZone) {
  WZ <- NA
  VCharges <- NA
  
  if (nZone == "Shoulder 1") WZ <- W1
  else if (nZone == "Shoulder 2") WZ <- W3
  else WZ <- W4
    
  if (df$Charge_Duration[ind] > length(WZ)) {
    df$Time_Zone[ind] <- nZone
    df$Start_Time[ind] <- WZ[1]
    
    duration <- df$Charge_Duration[ind] - length(WZ)
    df$Charge_Duration[ind] <- length(WZ)
    if (df$Charge_Type[ind] == "L1") {
      df$KWh[ind] <- (df$Charge_Duration[ind]/6) * 1.92
      kwh <- (duration/6) * 1.92
    }
    else {
      df$KWh[ind] <- (df$Charge_Duration[ind]/6) * 6.6
      kwh <- (duration/6) * 6.6
    }
    
    VCharges <- filter(df, Index != ind & Charge_Type == df$Charge_Type[ind] & Date == df$Date[ind] & HV_Code == df$HV_Code[ind] & Time_Zone == df$Time_Zone[ind])
    df$Noise[ind] <- Overlap(VCharges, df$Start_Time[ind], df$Charge_Duration[ind], WZ)
    
    pos <- last(df$Index) + 1
    if (nZone == "Shoulder 1") {
      df <- add_row(df, Index = pos, Charge_Type = df$Charge_Type[ind], Date = df$Date[ind], HV_Code = df$HV_Code[ind], Charge_Duration = duration, Start_Time = W2[1], Time_Zone = "Peak", DayType = "W", KWh = kwh)
      
      VCharges <- filter(df, Index != pos & Charge_Type == df$Charge_Type[pos] & Date == df$Date[pos] & HV_Code == df$HV_Code[pos] & Time_Zone == df$Time_Zone[pos])
      df$Noise[pos] <- Overlap(VCharges, df$Start_Time[pos], df$Charge_Duration[pos], W2)
    }
    else if (nZone == "Shoulder 2") {
      df <- add_row(df, Index = last(df$Index) + 1, Charge_Type = df$Charge_Type[ind], Date = df$Date[ind], HV_Code = df$HV_Code[ind], Charge_Duration = duration, Start_Time = W4[1], Time_Zone = "Off Peak", DayType = "W", KWh = kwh)
      
      VCharges <- filter(df, Index != pos & Charge_Type == df$Charge_Type[pos] & Date == df$Date[pos] & HV_Code == df$HV_Code[pos] & Time_Zone == df$Time_Zone[pos])
      df$Noise[pos] <- Overlap(VCharges, df$Start_Time[pos], df$Charge_Duration[pos], W4)
      
      df$Noise[pos] <- Off_Peak_Case(df, pos, W4)
    }
    else {
      df <- add_row(df, Index = last(df$Index) + 1, Charge_Type = df$Charge_Type[ind], Date = df$Date[ind], HV_Code = df$HV_Code[ind], Charge_Duration = duration, Start_Time = W1[1], Time_Zone = "Shoulder 1", DayType = "W", KWh = kwh)
      
      VCharges <- filter(df, Index != pos & Charge_Type == df$Charge_Type[pos] & Date == df$Date[pos] & HV_Code == df$HV_Code[pos] & Time_Zone == df$Time_Zone[pos])
      df$Noise[pos] <- Overlap(VCharges, df$Start_Time[pos], df$Charge_Duration[pos], W1)
    }
  }
  else {
    try <- 0
    flag <- TRUE
    
    while (try < 100 & flag) {
      rn <- floor(runif(1, 1, length(WZ) - df$Charge_Duration[ind] + 2))
      df$Time_Zone[ind] <- nZone
      df$Start_Time[ind] <- WZ[rn]
      try <- try + 1
      
      VCharges <- filter(df, Index != ind & Charge_Type == df$Charge_Type[ind] & Date == df$Date[ind] & HV_Code == df$HV_Code[ind] & Time_Zone == df$Time_Zone[ind])
      df$Noise[ind] <- Overlap(VCharges, df$Start_Time[ind], df$Charge_Duration[ind], WZ)
      
      if(nZone == "Off Peak") df$Noise[ind] <- Off_Peak_Case(df, ind, WZ)
      
      if (df$Noise[ind] == 0) flag <- FALSE
    }
  }
  return(df)
}

Overlap <- function(df, st, cd, WZ) {
  if (dim(df)[1] == 0) return (0)
  
  noise <- NA

  p1 <- match(st, WZ)
  p2 <- p1 + cd - 1
  prev <- WZ[p1:p2]
  
  for(i in 1:dim(df)[1]) {
    p1 <- match(df$Start_Time[i], WZ)
    p2 <- p1 + df$Charge_Duration[i] - 1
    curr <- WZ[p1:p2]
    
    if (df$Charge_Type[i] == "L1") noise <- (length(intersect(prev, curr))/6) * 1.92
    else noise <- (length(intersect(prev, curr))/6) * 6.6
    
    if (noise > 0) return (noise)
  }
  return(0)
}

Off_Peak_Case <- function(df, ind, WZ) {
  midnight <- match("0:00", WZ)
  p <- match(df$Start_Time[ind], WZ)
  
  if (p >= midnight) {
    dt <- as.Date(df$Date[ind], format = "%d/%m/%Y") - 1
    dt <- glue("{day(dt)}/{month(dt)}/{year(dt)}")
    VCharges <- filter(df, Charge_Type == df$Charge_Type[ind] & Date == dt & HV_Code == df$HV_Code[ind] & Time_Zone == df$Time_Zone[ind] & match(Start_Time, WZ) < midnight & match(Start_Time, WZ) + Charge_Duration - 1 >= midnight)
    noise2 <- Overlap(VCharges, df$Start_Time[ind], df$Charge_Duration[ind], WZ)
    return(max(noise2, df$Noise[ind]))
  }
  else {
    p <- p + df$Charge_Duration[ind] - 1
    if(p >= midnight) {
      if(format(as.Date(df$Date[ind]), "%a") == "Fri") return(df$KWh[ind])
      
      dt <- as.Date(df$Date[ind], format = "%d/%m/%Y") + 1
      dt <- glue("{day(dt)}/{month(dt)}/{year(dt)}")
      VCharges <- filter(df, Charge_Type == df$Charge_Type[ind] & Date == dt & HV_Code == df$HV_Code[ind] & Time_Zone == df$Time_Zone[ind] & match(Start_Time, WZ) >= midnight)
      noise2 <- Overlap(VCharges, df$Start_Time[ind], df$Charge_Duration[ind], WZ)
      return(max(noise2, df$Noise[ind]))
    }
    else return(df$Noise[ind])
  }
}
