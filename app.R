library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)
library(dplyr)



ui <- dashboardPage(
  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
    
    #the logo for the site
    imageOutput("image1",height = 30),
    
    #get file
    data<-read.csv(file.choose(),header = TRUE),
    fifa<-as.data.frame(data),
    
    
    tags$hr(),
    
    #the sidebar menu
    sidebarMenu(
    menuItem( tags$h4("Conferderation"),
    
      menuSubItem("UEFA",tabName = "UEFA"),
      menuSubItem("CAF", tabName = "CAF"),
      menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
      menuSubItem("AFC", tabName = "AFC"),
      menuSubItem("OFC", tabName = "OFC"),
      menuSubItem("CONCACAF",tabName = "CONCACAF")
    ),
    menuItem( tags$h4("Bet Team"),
              
              menuSubItem("Year",tabName = "Year"),
              menuSubItem("Top10", tabName = "Top10"),
              menuSubItem("Rating",tabName = "Rating")
             
    ),
    menuItem( tags$h4("Prediction"),
              
              menuSubItem("Winning Team",tabName = "Winning Team"),
              menuSubItem("CAF", tabName = "Slider"),
              menuSubItem("CONMEBOL",tabName = "off")
              
    )
  )
  ),
  #the skin color of the dashboard
  skin = "green",
  dashboardBody(
    
    fluidRow(
      column(6,
      box(
        title = tags$b("controls"),width = 50, background = "blue",solidHeader = TRUE,collapsible = TRUE,status = 
          "primary",
        tabItems(
          tabItem(tabName = "Slider",
        sliderInput("slider","Observations:",
                    min = 1,max=100, value = 50)
        ),
        tabItem(tabName = "off",
                box("plot1", h2("off control h2 font"))
                )
      )
      )
    ),
    column(6,
         box(title = tags$b("YEAR"),width = 50,background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary")  
    )
    ),
    fluidRow(
  column(12,
         box(title =tags$b("CHART"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
                tabItems(
      tabItem(tabName = "CAF",
              plotOutput("africaBest", click = "plot ME")
      ),
      tabItem(tabName = "UEFA",
              plotOutput("europebest")),
      tabItem(tabName = "CONMEBOL",
              plotOutput("conmebolbest")),
      tabItem(tabName = "AFC",
              plotOutput("afcbest")),
      tabItem(tabName = "OFC",
              plotOutput("ofcbest")),
      tabItem(tabName = "CONCACAF",
              plotOutput("concacafbest")),
      
      tabItem(tabName = "Year",
              #country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date),
             plotOutput("countrydata", click = "click Me") ),
      tabItem(tabName = "Top10",
              #country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date),
              plotOutput("worldbest", click = "click Me") )
      )
      )
    )
  )
    )
   
  )


server <- function(input,output){
  
 
  #plot_bar country data
 # output$worldbest <- renderPlot(ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip())
 # output the country data
  output$countrydata <- renderPlot(ggplot(country_data,aes(x=rank_date,y=previous_points))+geom_col()+coord_flip())
  #output africa best
  output$africaBest <- renderPlot({
    africa_best<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(africa_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output europe's best
  output$europebest <- renderPlot({
    europe_best<-fifa%>%filter(confederation=="UEFA",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(europe_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output asia's best
  output$afcbest <- renderPlot({
    asian_best<-fifa%>%filter(confederation=="AFC",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(asian_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output oceanian best
  output$ofcbest <- renderPlot({
    oceanian_best<-fifa%>%filter(confederation=="OFC",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(oceanian_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output southamerica's best
  output$conmebolbest <- renderPlot({
    southamerica_best<-fifa%>%filter(confederation=="CONMEBOL",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(southamerica_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output for concaf
  output$concacafbest <- renderPlot({
    Northamerica_best<-fifa%>%filter(confederation=="CONCACAF",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot( Northamerica_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #output worldbest
  
  output$worldbest <- renderPlot({
    world_best_data<-fifa%>%filter(rank<=10,rank_date=="6/7/2018")%>%select(rank,country_full,total_points)
    ggplot( world_best_data,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #choice made of the select option
  output$result <- renderText(paste(input$conferderation))
  
  #render image
  output$image1 <- renderImage({
    if ("True") {
      return(list(
        src = "www/ball1.png",
        contentType = "image/png",
        alt = "foot"
        
      ))
    } 
      
  }, deleteFile = FALSE)
  
}
shinyApp(ui=ui,server=server)