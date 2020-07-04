# Script that creates the R plots that are presented in Chapter 2

# CONTAINS FUNCTIONS AT THE END OF THE FILE

# Load packages
# =======================================================
library(ggplot2)
library(lubridate)
library(dplyr)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Prepare Year_AVG Data Frame
# =======================================================
House <- demand("House.csv")
Total_L1 <- tot.demand(House, "PEV_L1.csv")
Total_L2 <- tot.demand(House, "PEV_L2.csv")

House_Year_AVG <- avg.demand(House, "House")
Total_L1_Year_AVG <- avg.demand(Total_L1, "House + L1")
Total_L2_Year_AVG <- avg.demand(Total_L2, "House + L2")

Year_AVG <- rbind(Total_L2_Year_AVG, Total_L1_Year_AVG, House_Year_AVG)
Year_AVG$Type <- factor(Year_AVG$Type, levels = c("House + L2", "House + L1", "House"))

# Creating the plots
# =======================================================
p <- plot1(Year_AVG)
p

# F U N C T I O N S   S E C T I O N
# =======================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = =
demand <- function(file) {
  df <- read.csv2(file)
  df$Demand <- rowSums(df[,-1])
  df <- df[, c(1,dim(df)[2])]
  df$Time <- dmy_hm(df$Time)
  df$DayType <- format(df$Time, "%a")
  df$DayNum <- str_replace_all(df$DayType, c("Mon" = "1", "Tue" = "2", "Wed" = "3", "Thu" = "4", "Fri" = "5", "Sat" = "6", "Sun" = "7"))
  df$Hour <- format(df$Time,"%H:%M:%S")
  df$DayTime <- paste(df$DayNum, df$Hour, df$DayType, sep = " ")
  df <- df[, c(1,6,2)]
}

tot.demand <- function(df, file) {
  temp <- demand(file)
  tot <- cbind(df, temp$Demand)
  tot$Demand <- rowSums(tot[,c(3,4)])
  tot <- tot[, -4]
}

avg.demand <- function(df, type) {
  df <- group_by(df, DayTime)
  df <- summarize(df, AVG = mean(Demand))
  df$DayTime <- seq(ymd_hm("2010-1-4 00:00"), ymd_hm("2010-1-10 23:50"), by = "10 mins")
  df$Type <- type
  colnames(df) <- c("Time", "Demand", "Type")
  return(df)
}

# ==

plot1 <- function(df) {
  # Average yearly power demand by day
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_blank(), 
                    legend.position = "top", 
                    legend.justification = "left",
                    legend.direction = "horizontal",
                    legend.title = element_blank(), 
                    legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Time, y = Demand, color = Type)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
  scale_x_datetime(date_labels = "%a", breaks = "1 day") +
  labs(y = "Average demand (W)") +
  myfill + mycolor + mytheme
}
