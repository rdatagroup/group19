
library(shiny)
library(DT)
library(plyr)

#get file
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)

ui <- fluidPage(
  fluidRow(column(12,
                  box(title = "Counting",
                    tableOutput("counter")
                      )
                  ))
)
server <-function(input,output){
  output$counter <- renderTable({
    
    count_best<-fifa%>%filter(rank=="1")%>%select(rank,country_full,total_points)
   count(count_best,c("country_full","rank"))
  })
}
shinyApp(ui=ui,server = server)