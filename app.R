library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)



ui <- dashboardPage(
  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
    #the logo for the site
    imageOutput("image1",height = 30),
    
    tags$hr(),
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
  skin = "green",
  dashboardBody(
    navbarPage("Applications",
               tabPanel("charts"),
               tabPanel("histogram",tableOutput("mtcars")),
               tabPanel("tables")
               ),
    fluidRow(
      column(6,
      box(
        title = tags$b("controls"),width = 50, background = "blue",solidHeader = TRUE,
        tabItems(
          tabItem(tabName = "Slider",
        sliderInput("slider","Observations:",
                    min = 1,max=100, value = 50)
        ),
        tabItem(tabName = "off",
                h2("off control h2 font")
                )
      )
      )
    ),
    column(6,
         box(title = tags$b("Histogram buddie"),width = 50,background = "blue",
             tabItems(
            tabItem(tabName = "UEFA",
                    h1("football live")
                    ),
            tabItem(tabName = "OFC",
                    h1("The OFC cup")
            )
          )
    )  
    )
    ),
    fluidRow(
  column(12,
         box(title = tags$b("Tables for Conferderation"),width = 100,background = "blue",
                tabItems(
      tabItem(tabName = "CAF",
      csvdata <-read.csv("/home/joshard/mode/fifa_ranking.csv",header = TRUE),
           DT::dataTableOutput("csvdata")
      ),
      tabItem(tabName = "Year",
              h1("hitler my uncle")
      )
      )
    )
  )
    )
   
  )
)

server <- function(input,output){
  
  #render the table of fifa ranking
  output$csvdata <- DT::renderDataTable({
    csvdata
  })
  
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