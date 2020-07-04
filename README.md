# BSc_Thesis_Scripts
In order for the scripts to work, three .csv files are required;\
House.csv: a file containing the household power demand (in watt)\
PEV_L1.csv: a file containing the power demand of PEV charging using Level 1 charging (in watt)\
PEV_L2.csv: a file containing the power demand of PEV charging using Level 2 charging (in watt)\
The first collumn of these files has to be named “Time” and contain dates (dd/mm/yyyy) and times (HH:MM) divided by 10-mins. The rest of the columns need to have a unique household-vehicle id in alphabetical order\

"Chapter 2 Plots.R" creates the plots found in Chapter 2 of the thesis\
"Chapter 4 Plots.R" creates the plots found in Chapter 4 of the thesis\
"Chapter 5 Plots.R" creates the plots found in Chpater 5 of the thesis
