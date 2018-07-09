library(dplyr)
library(ggplot2)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#drawing the bar graph of the best 10 countries in the world
world_best<-fifa%>%filter(rank<=10,rank_date=="6/7/2018")%>%select(rank,country_full,total_points)
#draw a bar graph for the world best
<<<<<<< HEAD
plot_bar<-ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
plot_bar
=======
plot_dR<-ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
>>>>>>> 4c36b3668a4cc1c6425349ffd14028b4c82f2799
