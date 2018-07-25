library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)
library(dplyr)
library(data.table)
library(formattable)
library(markdown)
library(lubridate)
library(reshape2)
library(plyr)
library(plotly)
library(tidyr)

#get file
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
dates <- as.Date(data$rank_date,
                 format = '%Y-%m-%d')
data2<-ddply(fifa,.(country_full,rank,rank_change,confederation),
             summarize,Total_Points_Earned=previous_points)
desc <- data2[order(-data2$Total_Points_Earned), ]
agg<-split(desc,desc$country_full)
trial<-do.call(rbind,lapply(agg,function(chunk) chunk[which.max(chunk$Total_Points_Earned),]))
agg3 <- ddply(trial, .(rank),
              function(x)x[x$Total_Points_Earned==max(x$Total_Points_Earned), ])

ui <- dashboardPage(

  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
    
    #the logo for the site
    imageOutput("image1",height = 30),
    
    
   
    
    tags$hr(),
    
    #the sidebar menu
    sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard",icon = icon("tachometer")),
    menuItem( tags$h4("Conferderation"),
    
      menuSubItem("UEFA",tabName = "UEFA"),
      menuSubItem("CAF", tabName = "CAF"),
      menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
      menuSubItem("AFC", tabName = "AFC"),
      menuSubItem("OFC", tabName = "OFC"),
      menuSubItem("CONCACAF",tabName = "CONCACAF")
    ),
    menuItem( tags$h4("Best Team"),
              
              menuSubItem("Year",tabName = "Year"),
              menuSubItem("Top10", tabName = "Top10"),
              menuSubItem("Rating",tabName = "Rating")
             
    ),
    menuItem( tags$h4("Prediction"),
              
              menuSubItem("Winning Team",tabName = "Winning Team"),
              menuSubItem("CAF", tabName = "Slider"),
              menuSubItem("CONMEBOL",tabName = "off")
              
    ),
    menuItem("About",tabName="about_tab",icon=icon("info"))
  )
  ),
  #the skin color of the dashboard
  skin = "green",
  dashboardBody(
    
    fluidRow(
      column(12,
             navbarPage("YEAR",
                        tabPanel("WORLD-BEST", 
                                 navbarMenu(title = tags$b("CURRENT WORLD RANKINGS"),width = 100,
                                            collapsible = TRUE,status = "primary",solidHeader = TRUE,icon=icon("futbol"),
                                            tabPanel("dashboard",formattableOutput("world_rankings")) ,
                                            tabPanel( "about_tab",uiOutput("abouts"))
                                            )
                        ),
                        tabPanel("Summary",
                                 verbatimTextOutput("summary")
                        ),
                        textOutput("result"),
                        
                        tabPanel("Data",
                                 # Select type of trend to plot
                                 selectInput("country", label = strong("Trend index"),
                                             choices = unique(data2$country_full),selected = "Brazil"),
                                 
                                 mainPanel(
                                   textOutput("Department"),
                                   plotOutput("country")
                                 )
                        )
                        
                        
             )
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
              textInput("date_year","Enter Date:","6/7/2018"),
              actionButton("value","PLOT"),
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

                              column(6,textInput("range","insert range",50))),
                              
               conditionalPanel(condition = "input.Data == 'UEFA'",
                               column(6,textInput("date","insert date","6/7/2018")),
                               column(6,textInput("range","insert range",10))),

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
                                column(6,textInput("country","Choose Country","GER")),
                                column(6,textInput("yr","Choose Year","2018"))),
                                
               
               tableOutput("plotTable")
              
                           )
          
           
           
               
                           )
               )
           )
    )
  
   
  

server <- function(input,output,session){
  output$Department <-{(
    renderText(input$country)
  )
  }
 output$country<-renderPlot({
  text_data<-input$country
   dta<-data2%>%filter(country_full==text_data)%>%
      select(rank,country_full,rank_change,Total_Points_Earned)%>%
     arrange(desc(Total_Points_Earned))
   
   ggplot(data=dta, aes_string(x=dta$Total_Points_Earned,y=dta$rank_change))  +
     stat_summary( geom = "bar",colour="#56B4E9",fill="#56B4E9") +
     geom_bar(stat="identity") +
     labs(title=input$country, y ="Rank_change",x="Total Points Earned") +
     theme_classic() +
     theme(plot.title = element_text(hjust = 0.5))
 })
  
 output$world_rankings<-renderFormattable({
   world_data1<-fifa%>%filter(rank<=15,rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,cur_year_avg_weighted,last_year_avg,last_year_avg_weighted,two_year_ago_avg,two_year_ago_weighted,three_year_ago_avg,three_year_ago_weighted,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
   #selecting the columns to display
   best_data1<-world_data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
   #The display the table
   
   #change column names 
   colnames(best_data1)<-c("RANK","COUNTRY","CONFEDERATION","PREVIOUS POINTS","RANKCHANGE","CUR YEAR AVG","CUR YEAR AVG WEIGHTED","LAST YEAR AVG","LAST YEAR AVG WEIGHTED","TWO YEAR AVG","TWO YEAR WEIGHTED","THREE YEAR AVG","THREE YEAR WEIGHTED","POINTS")
   #widget_formattable<-formattable(best_data)
   widget_formattable<-formattable(best_data1, list(
     RANK=formatter("span",
                    style=~style(color=ifelse(RANK>=5,"green","red"))
     ),
     COUNTRY=color_tile("transparent","lightpink"),
     RANKCHANGE=formatter("span",
                          style=~style(color=ifelse(RANKCHANGE<0,"red","green")),
                          RANKCHANGE~icontext(ifelse(RANKCHANGE<0,"arrow-down","arrow-up"),RANKCHANGE)),
     POINTS=color_text("green","red"))
   )
   
 })
 
 output$desc = DT::renderDataTable({
   datatable(agg3)
 })
 
 output$summary <- renderPrint({
   summary(desc)
 })
 
  #plot_bar country data
 # output$worldbest <- renderPlot(ggplot(world_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip())
 # output the country data
  output$countrydata <- renderPlot({
    country_data<-fifa%>%filter(country_abrv=="GER")%>%select(rank,previous_points,rank_date)
    ggplot(country_data,aes(x=rank_date,y=previous_points))+geom_col()+coord_flip()})
  
  #output africa best
  
  output$africaBest <- renderPlot({
    africa_best<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(africa_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
  })
  #display table of world best
  output$plotTable <- renderTable({
    choosen_data <- input$Data
    Date_input <-  input$date
    Range <-  input$range
    yearly_data<-input$yr
    country_spec<-input$country
    if(choosen_data == "World Best"){
    world_data<-fifa%>%filter(rank<=Range,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:Range)
    best_data<-world_data[,c(1,2,3)]
    colnames(best_data)<-c("RANK","COUNTRY","POINTS")
    widget_formattable<-formattable(best_data)
   
    
    }else if(choosen_data%in%c("UEFA","CAF","CONCACAF","OFC","AFC","CONMEBOL")){#choosen_data == "UEFA"){
      tabl_select<-fifa%>%filter(confederation==choosen_data,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:Range)
      #selecting the columns to display
      cho_data<-tabl_select[,c(1,2,3)]
      #The display the table
      
      #change column names 
      colnames(cho_data)<-c("WORLD-RANK","COUNTRY","POINTS")
      widget_formattable<-formattable(cho_data)
    }else if(choosen_data=="Country Data"){
      country_data<-fifa%>%filter(filtered_date<-substring(rank_date,6)==yearly_data,country_abrv==country_spec)%>%select(rank,total_points,rank_change)
      cont_data<-country_data[,c(1,2,3)]
      #The display the table
      
      #change column names 
      colnames(cont_data)<-c("RANK","POINTS","CHANGE")
      widget_formattable<-formattable(cont_data)
    }

  },striped = T,width = "600",align ="c")
  
  #output europe's best
  bar_plot<-eventReactive(
    input$value,{
      date_of_ranking=input$date_year
      europe_best<-fifa%>%filter(confederation=="UEFA",rank_date==date_of_ranking)%>%select(country_full,total_points)%>%slice(1:10)
      #ggplot(europe_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
      ggplot(europe_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer()
      #barplot(europe_best,x=country_full,y=total_points)
    }
  )
  output$europebest <- renderPlot({bar_plot()})
  #output asia's best
  output$afcbest <- renderPlot({
    asian_best<-fifa%>%filter(confederation=="AFC",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(asian_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()+theme_minimal()+ggtitle("AFRICA's BEST COUNTRIES")
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
  #displaying html data
  output$abouts<-reactiveUI(function(){
    file_to_show='aboutus.Rhtml'
    HTML(readLines(file_to_show))
  })
  #choice made of the select option
 # output$result <- renderText(paste(input$conferderation))
  
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

