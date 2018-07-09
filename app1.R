library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library()

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
      menuSubItem("CONMEBOL",tabName = "CONMEBOL")))),

fluidRow(
      column(12,
      box(
        title = tags$b("controls"),width = 50, background = "blue",solidHeader = TRUE,
        tabItems(
          tabItem(tabName = "Slider",
        sliderInput("slider","Observations:",
                    min = 1,max=100, value = 50)
       )))) ),
dashboardBody(
  navbarPage("Applications",
             tabPanel("charts"),
             tabPanel("histogram",tableOutput("mtcars")),
             tabPanel("tables")
  )
))
server <- function(input,output){
  output$csvdata <- DT::renderDataTable({
    csvdata
  })
  output$img <-renderImage({ tags$img(src="/img/Logo.png",width = 50)})
}
shinyApp(ui=ui,server=server)
