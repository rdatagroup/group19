ibrary(dplyr)
library(ggplot2)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#selecting country's data from 1993
country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date)
#graph for Germany team
plot_bar<-ggplot(country_data,aes(x=rank_date,y=previous_points))+geom_col()+coord_flip()
plot_bar