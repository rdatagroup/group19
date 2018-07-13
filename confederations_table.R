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
choosen_data1<-new_data[,c(4,2,3,1)]
#The display the table

#change column names 
colnames(choosen_data1)<-c("WORLD-RANK","COUNTRY","POINTS","RANK")
#display the data in table format using kable
x_html <- knitr::kable(head(choosen_data1), "html")
data3<-kable_styling(x_html,bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 15,position = "center")%>%row_spec(0,color = "white",background = "green")
#widget_formattable<-formattable(choosen_data)