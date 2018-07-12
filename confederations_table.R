library(dplyr)
library(ggplot2)
library(formattable)
library(kableExtra)
library(knitr)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
tabl_select<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
#selecting the columns to display
new_data<-within(tabl_select,rank1<-ave(total_points,FUN = function(x)rev(order(x))))
choosen_data<-new_data[,c(1,2,3,4)]
#The display the table

#change column names 
colnames(choosen_data)<-c("WORLD-RANK","COUNTRY","POINTS","RANK")
#display the data in table format using kable
data3<-choosen_data%>%kable()%>%kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 15,position = "center")%>%row_spec(0,color = "white",background = "green")
#widget_formattable<-formattable(choosen_data)