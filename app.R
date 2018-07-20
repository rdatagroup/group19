library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)
library(dplyr)
library(formattable)
library(markdown)
library(lubridate)
library(plyr)
library(Matrix)
library(tidyr)

#get file
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#creating new column of dates
data4<-data%>%
  select(rank,country_full,rank_change,previous_points,rank_date)
subset <- data4%>%select(rank_date)
subset_date <- format(as.Date(subset$rank_date, format="%Y-%m-%d"),"%Y")
subset_data <-cbind(data4,subset_date)

world_data1<-fifa%>%filter(rank<=211,rank_date=="6/7/2018")%>%
  select(rank,country_full,confederation,previous_points,
         rank_change,cur_year_avg,cur_year_avg_weighted,last_year_avg,last_year_avg_weighted,two_year_ago_avg,two_year_ago_weighted,three_year_ago_avg,three_year_ago_weighted,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
ui <- dashboardPage(

  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
    
    #the logo for the site
    imageOutput("image1",height = 30),
    
    
   
    
    tags$hr(),
    
    #the sidebar menu
    sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard",icon = icon("tachometer"),
             menuSubItem("World Table",tabName = "worldtable"),
             menuSubItem("Europe Table",tabName = "europetable"),
             menuSubItem("South-America Table",tabName = "southtable"),
             menuSubItem("North-America Table",tabName = "northtable"),
             menuSubItem("Africa Table",tabName = "africatable"),
             menuSubItem("OceanianTable",tabName = "oceantable"),
             menuSubItem("Asia Table",tabName = "asiatable")),
    
    menuItem( tags$h4("Conferderation"),
    
      menuSubItem("UEFA",tabName = "UEFA"),
      menuSubItem("CAF", tabName = "CAF"),
      menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
      menuSubItem("AFC", tabName = "AFC"),
      menuSubItem("OFC", tabName = "OFC"),
      menuSubItem("CONCACAF",tabName = "CONCACAF")
    ),
    menuItem("Summaries",tabName = "summary"),
    menuItem("table",tabName = "table"),
    menuItem("Graphs",tabName = "graph"),
  
    menuItem("About",tabName="about_tab",icon=icon("info"))
  )
  ),
  #the skin color of the dashboard
  skin = "green",
  dashboardBody(
    fluidRow(
  column(12,
         box(title =tags$b("DATA DISPLAY AND VISUALIZATION"),width = 100,collapsible = TRUE,status = "primary",solidHeader = TRUE,
                tabItems(
                  
                  tabItem(tabName="worldtable", 
                          DT::dataTableOutput("mytable")),
                  #tabPanel("worldtable",DT::dataTableOutput("world_ranking")),
                  tabItem(tabName = "europetable",DT::dataTableOutput("mytable6")),
                  tabItem(tabName = "southtable",DT::dataTableOutput("mytable1")),
                  tabItem(tabName = "northtable",DT::dataTableOutput("mytable2")),
                  tabItem(tabName = "africatable",DT::dataTableOutput("mytable3")),
                  tabItem(tabName = "oceantable",DT::dataTableOutput("mytable4")),
                  tabItem(tabName = "asiatable",DT::dataTableOutput("mytable5")),
                  
                  
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
      tabItem(tabName = "table",
              
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
                  
              
              ),
      tabItem(tabName = "about_tab",
              uiOutput("abouts")),
      tabItem(tabName = "summary",
              verbatimTextOutput("summary")
              ),
      tabItem(tabName = "search",
              verbatimTextOutput("search")
      ),
      tabItem(tabName = "graph",
              selectInput("country", label = strong("Trend index"),
                          choices = unique(fifa$country_full),selected = "Brazil"),
              
              selectInput('date', label = strong("Select Date"),
                          choices = unique(subset_data$subset_date),selected = "1999"),
              textOutput("Department"),
              plotOutput("country")
      ),
      
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
  #rendering formatable tables 
#  output$world_ranking<-renderFormattable({
 #   world_data1<-fifa%>%filter(rank<=15,rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,cur_year_avg_weighted,last_year_avg,last_year_avg_weighted,two_year_ago_avg,two_year_ago_weighted,three_year_ago_avg,three_year_ago_weighted,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
  #  widget_formattable<-formattable(world_data1, list(
   #   country_full=color_tile("transparent","lightpink"),
    #  rank_change=formatter("span",
     #                      style=~style(color=ifelse(rank_change<0,"red","green")),
      #                     rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change)),
      #POINTS=color_text("green","red"))
  #  )
    output$mytable=DT::renderDataTable({
     
           world_data1
       })
    
    output$mytable6=DT::renderDataTable({
    
      tabl_select<-fifa%>%filter(confederation=="UEFA",rank_date=="6/7/2018")
})
    output$mytable1=DT::renderDataTable({
      
      tabl_select<-fifa%>%filter(confederation=="CONMEBOL",rank_date=="6/7/2018")
    })
    output$mytable2=DT::renderDataTable({
      
      tabl_select<-fifa%>%filter(confederation=="CONCACAF",rank_date=="6/7/2018")
    })
    output$mytable3=DT::renderDataTable({
      
      tabl_select<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")
    })
    output$mytable4=DT::renderDataTable({
      
      tabl_select<-fifa%>%filter(confederation=="OFC",rank_date=="6/7/2018")
    })
    output$mytable5=DT::renderDataTable({
      
      tabl_select<-fifa%>%filter(confederation=="AFC",rank_date=="6/7/2018")
    })
  ####################################################
  output$Department <-{(
    renderText(input$country)
  )
  }
    output$date <- renderText({
      paste("input$date is", as.character(input$date))
    })
  output$country<-renderPlot({
    text_data<-input$country
    datex <-input$date
       
    dtax<-subset_data%>%filter(country_full==text_data)%>%
      select(rank,country_full,rank_change,previous_points,rank_date,subset_date)%>%
      arrange(desc(previous_points))
   
    dta <- dtax %>% filter(subset_date ==datex)%>%
     select(rank,country_full,rank_change,previous_points,rank_date,subset_date)
     
   
    ggplot(data=dta, aes_string(y=dta$previous_points,x=dta$rank_date))  +
      stat_summary( geom = "bar") +
      geom_bar(stat="identity") +
      labs(title=input$country, y ="Total Points Earned",x="Year") +
      scale_fill_manual(values=c("red", "blue","orange", "green","yellow","darkblue",
                                 "darkred","Awesome","Bitter lime","Chartreuse (web)","Dark orchid"
                                 ,"orest green (traditional)","Folly","Fluorescent yellow","Ferrari red"
                                 ,"English green"))+
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
 
 
 output$desc = DT::renderDataTable({
   datatable(agg3)
 })
 
 output$summary <- renderPrint({
   summary(fifa)
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

