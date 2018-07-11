library(dplyr)
library(ggplot2)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#filtering the data for country performance
data1<-fifa%>%filter(country_abrv=="GER") %>%select(rank,rank_change,rank_date)
plot_curve<-ggplot(data1,aes(x=rank_date,y=rank_change,group=1))+geom_line(color="#aa0022",size=1.0)+geom_point(color="#aa0022",size=3.5)+scale_x_discrete(breaks=c("1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
plot_curve