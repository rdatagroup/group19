?install.packages()
#get file
data<-read.csv(file.choose(),header = TRUE)

fifa<-as.data.frame(data)

ui <- fluidPage(title = "Visual",
               fluidRow(
                 column(12,
                        plotOutput("visual")
                        )
               ) 
                )
server <- function(input,output){
  output$visual <- renderPlot({
    visuali<-fifa%>%filter(rank_date=="8/8/1993")%>%select(rank,country_full,total_points)%>%slice(1:6)
    ggplot(data=visuali,mapping =  aes(x=country_full,y=rank))+geom_boxplot()
  })
  
}
shinyApp(ui=ui,server=server)