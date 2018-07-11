library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)

data<-read.csv(file.choose(),header = TRUE)

#total perfomnce in Year 2018

data<-ddply(data,.(country_full),
                        summarize,Total_Points_Earned=sum(previous_points))
data
#sort in descending order
desc <- data[order(-data$Total_Points_Earned), ]
desc
#output data in table format
library(DT)

ui <- basicPage(
  h2("The World Rankings Since 1993 to 2018 "),
  DT::dataTableOutput("desc")
)

server <- function(input, output) {
  output$desc = DT::renderDataTable({
    desc
  })
}

shinyApp(ui, server)
