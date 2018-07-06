library(dplyr)
library(ggplot2)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#condeferations best
africa_best<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>% select(rank,country_full,total_points)%>%slice(1:10)
#plot the bar graph for africas best 
plot_bar<-ggplot(africa_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()