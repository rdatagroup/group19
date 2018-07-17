library(shiny)
library(DT)
library(plyr)
library(dplyr)
library(ggplot2)

#get file
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)


ui <- fluidPage(
  fluidRow(column(12,
                  box(title="appearance",
                      tags$b("number of times country has appeared on rank sheet "),
                      tableOutput("times")
                      )
                  ))
)
server <- function(input,output){
  output$times <-renderTable({
    count_times<-fifa%>%filter(country_abrv=="TCA")%>%select(country_abrv,rank,country_full,total_points,confederation)
    count(count_times,country_full)
  })
}

shinyApp(ui=ui,server=server)