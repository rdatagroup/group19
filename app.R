library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)



ui <- dashboardPage(
  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
   #imageOutput(src("img/Logo.png"),width = 50),
    tags$img(src='/www/Logo.png'),
    
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
              h1("hitler we love you So!!")
      )
      )
    )
  )
    )
   
  )
)

server <- function(input,output){
  output$csvdata <- DT::renderDataTable({
    csvdata
  })
  output$img <-renderImage({ tags$img(src="/img/Logo.png",width = 50)})
}
shinyApp(ui=ui,server=server)