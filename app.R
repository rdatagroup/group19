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
    #data<-read.csv(file.choose(),header = TRUE),
    #fifa<-as.data.frame(data),
    
    
    tags$hr(),
    
    #the sidebar menu
    sidebarMenu(
    menuItem( tags$h4("Conferderation"),
    
      menuSubItem("UEFA",tabName = "UEFA"),
      menuSubItem("CAF", tabName = "CAF"),
      menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
      menuSubItem("AFC", tabName = "AFC"),
      menuSubItem("OFC", tabName = "OFC"),
      menuSubItem("CONCACsAF",tabName = "CONCACAF")
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
            #Select input value country, conferderation, year
    selectInput("conferderation","choose conferderation:",
                choices=c("CAF","COMMEBOL","UEFA","OFC","CONCACAF","AFC")
                ),
    
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
         box(title = tags$b("World Best"),width = 50,background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary",
         tabItems(
            tabItem(tabName = "UEFA",
                    h1("football live")
                    ),
            tabItem(tabName = "OFC",
                    world_best<-fifa%>%filter(rank<=10,rank_date=="6/7/2018")%>% select(rank,country_full,total_points),
                    plotOutput("worldbest",click = "plot Me")
            )
          )
    )  
    )
    ),
    fluidRow(
  column(12,
         box(title =textOutput("result"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
                tabItems(
      tabItem(tabName = "CAF",

             # africa_best<-fifa%>%filter(confederation==textOutput("result"),rank_date=="6/7/2018")%>% select(rank,country_full,total_points)%>%slice(1:10),
              plotOutput("africaBest", click = "plot ME"),

      csvdata <-read.csv(file.choose(),header = TRUE),
           DT::dataTableOutput("csvdata")

      ),
      tabItem(tabName = "Year",
              country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date),
             plotOutput("countrydata", click = "click Me")
                  
                  )
      )
      )
    )
  )
    )
   
  )


server <- function(input,output){
  
 
  #plot_bar country data
  output$worldbest <- renderPlot(ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip())
 # output the country data
  output$countrydata <- renderPlot(ggplot(country_data,aes(x=rank_date,y=previous_points))+geom_col()+coord_flip())
  #output africa best
  output$africaBest <- renderPlot({
    africa_best<-fifa%>%filter(confederation==input$conferderation,rank_date=="6/7/2018")%>% select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  }
    )
  
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