library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
data <- read.csv("fifa_ranking.csv")
#sorting out data into reasonable forms
data_sorting <- data[c("country_full","confederation",
                       "previous_points","rank_date"),
                     data[date %between% c("1993-08-08", "2018-06-07")]]
data_sorting

#filtering out confederations
#filtering for UEFA confederation 
UEFA <- subset(data_sorting,confederation == "UEFA")
UEFA

#selecting data to plot
uefa1<-select(UEFA,country_full,previous_points)
uefa1

#filtering for AFC confederation 
AFC <- subset(data_sorting,confederation == "AFC")
AFC

#selecting data to plot
afc1<-select(AFC,country_full,previous_points)
afc1

#filtering for CAF confederation 
CAF <- subset(data_sorting,confederation == "CAF")
CAF

#selecting data to plot
caf1<-select(CAF,country_full,previous_points)
caf1
#filtering for CONMEBOL confederation 
CONMEBOL <- subset(data_sorting,confederation == "CONMEBOL")
CONMEBOL

#selecting data to plot
con1<-select(CONMEBOL,country_full,previous_points)
con1
#filtering for CONCACAF confederation 
CONCACAF <- subset(data_sorting,confederation == "CONMEBOL")
CONCACAF

#selecting data to plot
conc1<-select(CONCACAF,country_full,previous_points)
conc1

#filtering for OFC  confederation 
OFC  <- subset(data_sorting,confederation == "CONMEBOL")
OFC 

#selecting data to plot
ofc<-select(OFC ,country_full,previous_points)
ofc



