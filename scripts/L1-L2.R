# Converts LoadShifting or TimeZones structures back into the L1 and L2 type structures

# CONTAINS FUNCTION AT THE END OF THE FILE

# Load packages
# =======================================================
library(lubridate)
library(dplyr)
library(glue)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Read LoadShifting (or TimeZones) data frame
# =======================================================
LS <- read.csv2("LoadShifting01.csv", stringsAsFactors = FALSE)
LS <- filter(LS, Noise == 0)

# Prepare empty data frame L1
# =======================================================
Time <- seq(dmy_hm("4-1-2010 00:00"), dmy_hm("8-1-2010 23:50"), by = "10 mins")
mins <- format(Time,"%M")
Time <- glue("{day(Time)}/{month(Time)}/{year(Time)} {hour(Time)}:{mins}")
L1 <- data.frame(Time)
namesVector <- as.character(seq(1, 348, by = 1))
L1[namesVector] <- 0
L2 <- data.frame(Time)
L2[namesVector] <- 0

# Create data frames L1 and L2
# =======================================================
L1 <- convert.to.lx(L1, LS, 1920)
L2 <- convert.to.lx(L2, LS, 6600)

# Export the L1 and L2 data frames to .csv
# =======================================================
write.csv2(L1, "L1_01.csv", row.names = FALSE)
write.csv2(L2, "L2_01.csv", row.names = FALSE)

# F U N C T I O N S   S E C T I O N
# =======================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = =
convert.to.lx <- function(x, df, watt) {
  DateTime <- NA
  counter <- 1
  
  if (watt == 6600) k <- match("L2", df$Charge_Type)
  else k <- 1
  
  for(j in 2:dim(x)[2]) {
    for(i in 1:dim(x)[1]) {
      DateTime <- paste(df$Date[k], df$Start_Time[k])
      
      # First Occurrence
      if (x$Time[i] == DateTime) {
        names(x)[j] <- df$HV_Code[k]
        x[i,j] <- watt
        counter <- counter + 1
        
        if (df$Charge_Duration[k] == 1) {
          counter <- 1
          k <- k + 1
        }
      }
      
      # Next Occurrences
      else if (counter > 1) {
        
        # Before The Final Occurrence
        if (counter < df$Charge_Duration[k]) {
          x[i,j] <- watt
          counter <- counter + 1
        }
        
        # Final Occurrence
        else {
          if (counter == df$Charge_Duration[k]) {
            x[i,j] <- watt
            counter <- 1
            k <- k + 1
          }
          else {
            counter <- 1
            k <- k + 1
          }
        }
      }
    }
  }
  return(x)
}
