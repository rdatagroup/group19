
library(shiny)
library(plyr)
library(DT)
library(dplyr)
library(ggplot2)

#get file
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)

ui <- fluidPage(
  column(12,
                  box(title = "Counting",
                    tags$b("number of times they were ranked as one"),
                    tableOutput("counter")
                   
                      )
                  ))

server <-function(input,output){
  output$counter <- renderTable({
    
    count_best<-fifa%>%filter(rank=="1")%>%select(rank,country_full)
   count(count_best,country_full,rank)
  })
 
}
shinyApp(ui=ui,server = server)