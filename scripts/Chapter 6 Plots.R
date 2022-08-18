# Script that creates the R plots that are presented in Chapter 6
# This script studies Case 1 and Case 2 
# Case 1: load shifting using a1, b1, c1
# Case 2: load shifting using a1 only

case <- 1                  # Assign "1" for Case 1 or "2" for Case 2
percentage <- "a1, b1, c1" # Assign "a1, b1, c1" for Case 1 or "a1" for Case 2

# CONTAINS FUNCTIONS AT THE END OF THE FILE

# Load packages
# =======================================================
library(lubridate)
library(ggplot2)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Prepare data frame LS
# =======================================================
TZ <- kwh.by.zone(paste("TimeZones.csv", sep =""), 0)
LS01 <- kwh.by.zone(paste("Case", case, "/LoadShifting01.csv", sep =""), 0.1)
LS02 <- kwh.by.zone(paste("Case", case, "/LoadShifting02.csv", sep =""), 0.2)
LS03 <- kwh.by.zone(paste("Case", case, "/LoadShifting03.csv", sep =""), 0.3)
LS04 <- kwh.by.zone(paste("Case", case, "/LoadShifting04.csv", sep =""), 0.4)
LS05 <- kwh.by.zone(paste("Case", case, "/LoadShifting05.csv", sep =""), 0.5)

LS <- rbind(TZ, LS01, LS02, LS03, LS04, LS05)

# Preparing House+L1 And House+L2 Data Frames
# =======================================================
L1_00 <- demand("PEV_L1.csv", "Before load shifting")
L2_00 <- demand("PEV_L2.csv", "Before load shifting")

L1_01 <- demand(paste("Case", case, "/L1_01.csv", sep =""), "After load shifting")
L1_01 <- rbind(L1_00, L1_01)
L1_01$Type <- factor(L1_01$Type, levels = c("Before load shifting", "After load shifting"))
L2_01 <- demand(paste("Case", case, "/L2_01.csv", sep =""), "After load shifting")
L2_01 <- rbind(L2_00, L2_01)
L2_01$Type <- factor(L2_01$Type, levels = c("Before load shifting", "After load shifting"))

L1_02 <- demand(paste("Case", case, "/L1_02.csv", sep =""), "After load shifting")
L1_02 <- rbind(L1_00, L1_02)
L1_02$Type <- factor(L1_02$Type, levels = c("Before load shifting", "After load shifting"))
L2_02 <- demand(paste("Case", case, "/L2_02.csv", sep =""), "After load shifting")
L2_02 <- rbind(L2_00, L2_02)
L2_02$Type <- factor(L2_02$Type, levels = c("Before load shifting", "After load shifting"))

L1_03 <- demand(paste("Case", case, "/L1_03.csv", sep =""), "After load shifting")
L1_03 <- rbind(L1_00, L1_03)
L1_03$Type <- factor(L1_03$Type, levels = c("Before load shifting", "After load shifting"))
L2_03 <- demand(paste("Case", case, "/L2_03.csv", sep =""), "After load shifting")
L2_03 <- rbind(L2_00, L2_03)
L2_03$Type <- factor(L2_03$Type, levels = c("Before load shifting", "After load shifting"))

L1_04 <- demand(paste("Case", case, "/L1_04.csv", sep =""), "After load shifting")
L1_04 <- rbind(L1_00, L1_04)
L1_04$Type <- factor(L1_04$Type, levels = c("Before load shifting", "After load shifting"))
L2_04 <- demand(paste("Case", case, "/L2_04.csv", sep =""), "After load shifting")
L2_04 <- rbind(L2_00, L2_04)
L2_04$Type <- factor(L2_04$Type, levels = c("Before load shifting", "After load shifting"))

L1_05 <- demand(paste("Case", case, "/L1_05.csv", sep =""), "After load shifting")
L1_05 <- rbind(L1_00, L1_05)
L1_05$Type <- factor(L1_05$Type, levels = c("Before load shifting", "After load shifting"))
L2_05 <- demand(paste("Case", case, "/L2_05.csv", sep =""), "After load shifting")
L2_05 <- rbind(L2_00, L2_05)
L2_05$Type <- factor(L2_05$Type, levels = c("Before load shifting", "After load shifting"))

# Prepare Tuesday Data Frames
# =======================================================
tueL1_00 <- tue.demand("PEV_L1.csv", "Before load shifting")
tueL2_00 <- tue.demand("PEV_L2.csv", "Before load shifting")

tueL1_01 <- tue.demand(paste("Case", case, "/L1_01.csv", sep =""), "After load shifing")
tueL1_01 <- rbind(tueL1_00, tueL1_01)
tueL2_01 <- tue.demand(paste("Case", case, "/L2_01.csv", sep =""), "After load shifting")
tueL2_01 <- rbind(tueL2_00, tueL2_01)

tueL1_03 <- tue.demand(paste("Case", case, "/L1_03.csv", sep =""), "After load shifting")
tueL1_03 <- rbind(tueL1_00, tueL1_03)
tueL2_03 <- tue.demand(paste("Case", case, "/L2_03.csv", sep =""), "After load shifting")
tueL2_03 <- rbind(tueL2_00, tueL2_03)

tueL1_05 <- tue.demand(paste("Case", case, "/L1_05.csv", sep =""), "After load shifting")
tueL1_05 <- rbind(tueL1_00, tueL1_05)
tueL2_05 <- tue.demand(paste("Case", case, "/L2_05.csv", sep =""), "After load shifting")
tueL2_05 <- rbind(tueL2_00, tueL2_05)

# Import Enegry Saving Data Frame
# =======================================================
es <- read.csv2(paste("Case", case,"/EnergySaving.csv", dec = ".", sep =""))

# Create the plots
# =======================================================
p <- plot1(LS, percentage)
p

# ==

p <- plot2(L1_01)
p
p <- plot2(L1_02)
p
p <- plot2(L1_03)
p
p <- plot2(L1_04)
p
p <- plot2(L1_05)
p

p <- plot2(L2_01)
p
p <- plot2(L2_02)
p
p <- plot2(L2_03)
p
p <- plot2(L2_04)
p
p <- plot2(L2_05)
p

# ==

p <- plot3(tueL1_01)
p
p <- plot3(tueL1_03)
p
p <- plot3(tueL1_05)
p

p <- plot3(tueL2_01)
p
p <- plot3(tueL2_03)
p
p <- plot3(tueL2_05)
p

# ==

p <- plot4(es, percentage)
p

# F U N C T I O N S   S E C T I O N
# =======================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = =
kwh.by.zone <- function(file, percentage) {
  df <- read.csv2(file, stringsAsFactors = FALSE)
  df$Date <- dmy(df$Date)
  df$Day <- format(df$Date, "%a")
  df <- filter(df, Charge_Type == "L1" & Day == "Tue")
  df <- group_by(df, Time_Zone)
  df <- summarize(df, KWh = sum(KWh))
  df$a1b1c1 <- percentage
  return (df)
}

demand <- function(file, type) {
  df <- read.csv2(file)
  lower <- match(("4/1/2010 0:00"), df$Time)
  upper <- match(("8/1/2010 23:50"), df$Time)
  df <- df[lower:upper,]
  df$Demand <- rowSums(df[,-1])
  df <- df[, c(1,dim(df)[2])]
  df$Time <- dmy_hm(df$Time)
  df$Type <- type
  return(df)
}

tue.demand <- function(file, type) {
  df <- read.csv2(file)
  lower <- match(("4/1/2010 22:00"), df$Time)
  upper <- match(("6/1/2010 6:50"), df$Time)
  df <- df[lower:upper,]
  df$Demand <- rowSums(df[,-1])
  df <- df[, c(1,dim(df)[2])]
  df$Type <- type
  df$Time <- dmy_hm(df$Time)
  return(df)
}

# ==

plot1 <- function(df, type) {
  # Case 1/Case 2: kWh per time zone for the L1 charges on Tuesday"
  mycolor <- scale_color_manual(values = c("slateblue4", "orangered2", "orchid3", "darkgoldenrod3"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = a1b1c1, y = KWh, color = Time_Zone, size = 4)) + geom_point() +
    labs(color = "Time Zones:", x = type) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    scale_size(guide = "none") +
    mycolor + mytheme
}

plot2 <- function(df) {
  # Case X - Subcase Y: Weekly power demand with L1/L2 charging
  mytimes <- as.numeric(c(dmy_hm("4/1/2010 7:00"), dmy_hm("4/1/2010 14:00"), dmy_hm("4/1/2010 20:00"), dmy_hm("4/1/2010 22:00"), dmy_hm("5/1/2010 7:00"), dmy_hm("5/1/2010 14:00"), dmy_hm("5/1/2010 20:00"), dmy_hm("5/1/2010 22:00"), dmy_hm("6/1/2010 7:00"), dmy_hm("6/1/2010 14:00"), dmy_hm("6/1/2010 20:00"), dmy_hm("6/1/2010 22:00"), dmy_hm("7/1/2010 7:00"), dmy_hm("7/1/2010 14:00"), dmy_hm("7/1/2010 20:00"), dmy_hm("7/1/2010 22:00"), dmy_hm("8/1/2010 7:00"), dmy_hm("8/1/2010 14:00"), dmy_hm("8/1/2010 20:00"), dmy_hm("8/1/2010 22:00")))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mytheme <- theme(plot.title = element_text(hjust = 0.3),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  p <- ggplot(df, aes(x = Time, y = Demand, color = Type)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
    geom_vline(xintercept = mytimes, linetype = "dashed", size = 1) +
    scale_x_datetime(date_labels = "%a", breaks = "1 day") +
    labs(y = "Demand (W)") +
    myfill + mycolor + mytheme
}

plot3 <- function(df) {
  # Case X - Subcase Y: Daily power demand with L1/L2 charging
  mytimes <- as.numeric(c(dmy_hm("4/1/2010 22:00"), dmy_hm("5/1/2010 7:00"), dmy_hm("5/1/2010 14:00"), dmy_hm("5/1/2010 20:00"), dmy_hm("5/1/2010 22:00"), dmy_hm("6/1/2010 7:00")))
  mycolor <- scale_color_manual(values = c("slateblue4", "orangered2", "orchid3", "darkgoldenrod3"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  p <- ggplot(df, aes(x = Time, y = Demand, color = Type)) +
    geom_line(size = 1) +
    geom_vline(xintercept = mytimes, linetype = "dashed", size = 1) +
    scale_x_datetime(date_labels = "%H:%M", breaks = "4 hours") +
    labs(y = "Demand (W)") +
    mycolor + mytheme
}

plot4 <- function(df, type) {
  # Energy saving for Case 1/Case 2
  mycolor <- scale_color_manual(values = c("orangered2", "slateblue4"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  p <- ggplot(df, aes(x = a1b1c1, y = Percentage, group = Type)) +
    geom_line(aes(color = Type), size = 1) +
    facet_grid(. ~ Class) +
    labs(x = type) +
    mycolor + mytheme
}
