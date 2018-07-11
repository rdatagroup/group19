library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)
library(dplyr)
library(formattable)



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
  ),
  #Row with column 12 siZe
  fluidRow(
    column(12,
           box(title =tags$b("TABLE"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
               #select input to display confederation, country data 
               selectInput("Data","Choose Data:",
                           choices = c("World Best","UEFA","CAF","OFC","AFC","CONMEBOL","CONCACAF","Country Data")),
                           conditionalPanel(condition = "input.Data == 'World Best'",
                              column(6,textInput("date","insert date","6/7/2018")),
                              column(6,textInput("range","insert range",10))),
               
                              
               conditionalPanel(condition = "input.Data == 'UEFA'",
                               column(6,textInput("date","insert date","6/7/2018")),
                               column(6,textInput("range","insert range",10)),
              tableOutput("plotTab")),
           
               conditionalPanel(condition = "input.Data == 'CAF'",
                              column(6,textInput("date","insert date","6/7/2018")),
                              column(6,textInput("range","insert range",10))),
               
               conditionalPanel(condition = "input.Data == 'OFC'",
                              column(6,textInput("date","insert date","6/7/2018")),
                              column(6,textInput("range","insert range",10))),
               
               conditionalPanel(condition = "input.Data == 'CONMEBOL'",
                                column(6,textInput("date","insert date","6/7/2018")),
                                column(6,textInput("range","insert range",10))),
               
               conditionalPanel(condition = "input.Data == 'CONCACAF'",
                                column(6,textInput("date","insert date","6/7/2018")),
                                column(6,textInput("range","insert range",10))),
               
               conditionalPanel(condition = "input.Data == 'AFC'",
                                column(6,textInput("date","insert date","6/7/2018")),
                                column(6,textInput("range","insert range",10))),
               
               conditionalPanel(condition = "input.Data == 'Country Data'",
                                textInput("date","insert date","6/7/2018")),
               
              tableOutput("plotTable")
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
  #display table of world best
  output$plotTable <- renderTable({
    choosen_data <- input$Data
    Date_input <- input$date
    Range <- input$range
    if(choosen_data == "World Best"){
    world_data<-fifa%>%filter(rank<=Range,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))
    best_data<-world_data[,c(1,2,3)]
    colnames(best_data)<-c("RANK","COUNTRY","POINTS")
    widget_formattable<-formattable(best_data)
   
    
    }else if(choosen_data == "UEFA"){
      tabl_select<-fifa%>%filter(confederation==choosen_data,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:Range)
      #selecting the columns to display
      cho_data<-tabl_select[,c(1,2,3)]
      #The display the table
      
      #change column names 
      colnames(cho_data)<-c("RANK","COUNTRY","POINTS")
      widget_formattable<-formattable(cho_data)      
    }

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