library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
<<<<<<< HEAD
library(formattable)
library(png)
=======
<<<<<<< HEAD
library(png)
library(dplyr)


=======
library()
>>>>>>> 453a1ca387e4530336306ab22cc00c85031fe180
>>>>>>> 9b92ef3b72415a19abda4f96f0352918e865e013

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
                
                menuSubItem("UEFA", tabName = "UEFA"),
                menuSubItem("CAF", tabName = "CAF"),
                menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
                menuSubItem("AFC", tabName = "AFC"),
                menuSubItem("OFC", tabName = "OFC"),
                menuSubItem("CONCACAF",tabName = "CONCACAF")
      ),
      menuItem( tags$h4("Best Team"),
                
               menuSubItem("Africabest",tabName = "Africabest" ),
                menuSubItem("Rates of Change", tabName = "rate"),
                menuSubItem("confed best",tabName = "conbest")
                
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
    #Select input value conferderation
    selectInput("conferderation","choose conferderation:",
                list('conferderation'=c("CAF","COMMEBOL","UEFA","OFC","CONCACAF","AFC"))
    ),
    #Select input value country
    
    #fluidRow 1
    
             
             tabItems(
               tabItem(tabName = "Year",
          fluidRow(
              column(6,    
                     
                      # box(title = tags$b("controls"),width = 50, background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                         #  sliderInput("slider","Observations:",
                           #            min = 1,max=100, value = 50)
                      # )
                     ),
              column(6, 
                      # box(title = tags$b("controls"),width = 50, background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                       #    h2("off control h2 font"))
               
             )
      )
      )
      ),
             
             tabItems(
               tabItem(tabName = "wbest",
         fluidRow(
              column(6,             
                       
                       box(title = tags$b("football live"),width = 50,background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                           h1("football live"))
              ),
              column(6, 
               
                       box(title = tags$b("World Best"),width = 50,background = "blue",solidHeader = TRUE,collapsible = TRUE,status = "primary",
                           world_best<-fifa%>%filter(rank<=10,rank_date=="6/7/2018")%>% select(rank,country_full,total_points),
                           plotOutput("worldbest",click = "plot Me"))
         )
             )
             
      )
    ),
    #fluidRow 2
   
             
             tabItems(
               tabItem(tabName = "Africabest",
          fluidRow(
              column(12,
                       box(title =tags$b("Africa Best"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
                           africa_best<-fifa%>%filter(confederation==("CAF"),rank_date=="6/7/2018")%>% select(rank,country_full,total_points)%>%slice(1:10),
                           plotOutput("africaBest", click = "plot ME")
                       )
               ) 
               ),
               tabItem(tabName = "CAF",
                       fluidRow(
                         column(12,
                       box(title =tags$b("CAF"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
                           country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date),
                           plotOutput("countrydata", click = "click Me")
                       )
                         )   
               )
             )
             
      )
    
  )
  
)
)

server <- function(input,output){
  
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
  
  #plot_bar country data
  output$worldbest <- renderPlot(ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip())
  # output the country data
  output$countrydata <- renderPlot(ggplot(country_data,aes(x=rank_date,y=previous_points))+geom_col()+coord_flip())
  #output africa best
  output$africaBest <- renderPlot(ggplot(africa_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip())
  
  #choice made of the select option
  output$result <- renderText(paste(input$conferderation))
  
  
  
}
shinyApp(ui=ui,server=server)