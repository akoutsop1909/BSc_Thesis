# Script that creates the R plots that are presented in Chapter 5

# CONTAINS FUNCTIONS AT THE END OF THE FILE

# Load packages
# =======================================================
library(ggplot2)
library(lubridate)
library(dplyr)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Prepare data frame TZ
# =======================================================
TZ <- read.csv2("TimeZones_old.csv", stringsAsFactors = FALSE)
TZ <- TZ[,-1] 
TZ$Date <- dmy(TZ$Date)
TZ$Day <- format(TZ$Date, "%a")
TZ$Day <- factor(TZ$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
TZ$Month <- format(TZ$Date, "%B")
TZ$Month <- factor(TZ$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
TZ$Week <- ceiling(day(TZ$Date) / 7)
TZ$Week <- paste("Week", TZ$Week, sep = " ")

# Create the plots
# =======================================================
p <- plot1(TZ, "L1")
p
p <- plot1(TZ, "L2")
p
# == 
p <- plot2(TZ, "L1")
p
p <- plot2(TZ, "L2")
p
# == 
p <- plot3(TZ, "L1")
p
p <- plot3(TZ, "L2")
p


# F U N C T I O N S   S E C T I O N
# =======================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = =
plot1 <- function(df, type) {
  # Number of charges per day with L1/L2 charging grouped by time zone 
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_blank(), 
                    legend.position = "top", 
                    legend.justification = "left",
                    legend.direction = "horizontal",
                    legend.title = element_text(face = "bold"),
                    legend.background = element_rect(colour = "black"))
  
  df %>% filter(df$Charge_Type == type) %>% ggplot(aes(x = Day, fill = Time_Zone)) + geom_bar() +
  labs(fill = "Time Zones:") +
  myfill + mytheme
}

plot2 <- function(df, type) {
  # Number of charges per week with L1/L2 charging grouped by time zone
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(df$Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar() +
  facet_grid(. ~ Month) +
  labs(fill = "Time Zones:") +
  myfill + mytheme
}

plot3 <- function(df, type) {
  # Number of charges per week with L1/L2 charging grouped by time zone (proportions)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(df$Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar(position = "fill") + 
  ylab("proportions") +
  facet_grid(. ~ Month) +
  labs(fill = "Time Zones:") +
  myfill + mytheme
}
