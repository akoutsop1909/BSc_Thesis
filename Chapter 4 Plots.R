# Script that creates the R plots that are presented in Chapter 4

# CONTAINS FUNCTIONS AT THE END OF THE FILE

# Load packages
# =======================================================
library(ggplot2)
library(lubridate)
library(dplyr)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Prepare Month_SUM Data Frame
# =======================================================
House <- demand("House.csv")
Total_L1 <- tot.demand(House, "PEV_L1.csv")
Total_L2 <- tot.demand(House, "PEV_L2.csv")

House_Month_SUM <- sum.month.demand(House, "House")
Total_L1_Month_SUM <- sum.month.demand(Total_L1, "House + L1")
Total_L2_Month_SUM <- sum.month.demand(Total_L2, "House + L2")

Month_SUM <- rbind(Total_L2_Month_SUM, Total_L1_Month_SUM, House_Month_SUM)
Month_SUM$Type <- factor(Month_SUM$Type, levels = c("House + L2", "House + L1", "House"))
Month_SUM$Month <- factor(Month_SUM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Prepare WorkingDay / WeekendDay Data Frames
# =======================================================
House <- avg.demand("House.csv", "Household only")
PEV_L1 <- avg.demand("PEV_L1.csv", "L1 charging only")
PEV_L2 <- avg.demand("PEV_L2.csv", "L2 charging only")

Working_Day <- type.combine(House, PEV_L1, PEV_L2, "Working day")
Working_Day$Day <- factor(Working_Day$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

Weekend_Day <- type.combine(House, PEV_L1, PEV_L2, "Weekend day")
Weekend_Day$Day <- factor(Weekend_Day$Day, levels = c("Sat", "Sun"))

# Creating the plots
# =======================================================
p <- plot1(Month_SUM)
p

# ==

p <- plot2(Working_Day)
p
p <- plot3(Working_Day)
p
p <- plot4(Working_Day)
p

# ==

p <- plot2(Weekend_Day)
p
p <- plot3(Weekend_Day)
p
p <- plot4(Weekend_Day)
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

sum.month.demand <- function(df, type) {
  df$Month <- format(House$Time, "%b")
  df <- group_by(df, Month)
  df <- summarize(df, Demand = sum(Demand))
  df$Type <- type
  return(df)
}

avg.demand <- function(file, type) {
  df <- demand(file)
  df <- group_by(df, DayTime)
  df <- summarize(df, AVG = mean(Demand))
  df$DayTime <- seq(ymd_hm("2010-1-4 00:00"), ymd_hm("2010-1-10 23:50"), by = "10 mins")
  df$Type <- type
  colnames(df) <- c("Time", "Demand", "Type")
  return(df)
}

day.type <- function(df) {
  df$Day <- format(df$Time, "%a")
  df$DayType <- sub("Mon|Tue|Wed|Thu|Fri", "Working day", df$Day)
  df$DayType <- sub("Sat|Sun", "Weekend day", df$DayType)
  return(df)
}

type.combine <- function(df1, df2, df3, type) {
  df1 <- day.type(df1)
  df2 <- day.type(df2)
  df3 <- day.type(df3)
  
  df <- rbind(df1, df2, df3)
  df <- filter(df, DayType == type)
  df <- df[, -5]
}

# ==

plot1 <- function(df) {
  # Total power demand per month
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Month, y = Demand, fill = Type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Total demand (W)") +
  myfill + mytheme
}

plot2 <- function(df) {
  # Average yearly power demand on working/weekend days
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Time, y = Demand)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
  facet_grid(. ~ Type) +
  scale_x_datetime(date_labels = "%a") +
  labs(y = "Average demand (W)") +
  myfill + mycolor + mytheme
}

plot3 <- function(df) {
  # Average yearly power demand on working/weekend days (bar chart)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Type) +
  labs(y = "Average demand (W)") +
  myfill + mytheme
}

plot4 <- function(df) {
  # Average yearly power demand on working/weekend days (boxplot)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
  geom_boxplot() +
  facet_grid(. ~ Type) + 
  labs(y = "Average demand (W)") +
  myfill + mytheme
}
