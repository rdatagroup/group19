library(ggplot2)
library(DT)
library(png)
library(dplyr)
library(formattable)
library(markdown)
library(lubridate)
library(plyr)
library(tidyr)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
firt_position <- fifa%>%filter(rank<=3)%>%select(country_abrv)
table_count<-count(firt_position)
#barplot(,names.arg = table_count$country_abrv)

#gg<-ggplot(table_count,aes(table_count$country_abrv,table_count$freq))+geom_bar(stat="identity", width = 0.5, fill="tomato2")+labs(title="BAR CHART",subtitle="COUNTRIES THAT OCCUPPIED THE TOP 3")+theme(axis.text.x = element_text(angle=65, vjust=0.6))
#treemapify install
#counts chart
#g<-ggplot(table_count,aes(table_count$country_abrv,table_count$freq))+geom_count(col="tomato3", show.legend=F)
