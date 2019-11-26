library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(png)
library(plyr)
library(dplyr)
library(formattable)
library(markdown)
library(stringr)
library(lubridate)
library(rsconnect)
library(tidyr)
#get file
#data<-read.csv(file.choose(),header = TRUE)
#data<-read.csv(file="data/fifa_ranking.csv")
fifa<-as.data.frame(data)
data4<-data%>%
  select(rank,country_full,rank_change,previous_points,rank_date)
subset <- data4%>%select(rank_date)
subset_date <- format(as.Date(subset$rank_date, format="%m/%d/%Y"),"%Y")
subset_data <-cbind(data4,subset_date)

#get file
#data<-read.csv(file.choose(),header = TRUE)
#fifa<-as.data.frame(data)

ui <- dashboardPage(
  ###applying the styles to shiny
  
  ####end of style

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
    
    menuItem( tags$h4(icon("globe"),"Conferderation"),
    
      menuSubItem("UEFA",tabName = "UEFA"),
      menuSubItem("CAF", tabName = "CAF"),
      menuSubItem("CONMEBOL",tabName = "CONMEBOL"),
      menuSubItem("AFC", tabName = "AFC"),
      menuSubItem("OFC", tabName = "OFC"),
      menuSubItem("CONCACAF",tabName = "CONCACAF")
    ),
    menuItem("Summaries",tabName = "summary",icon=icon("list-alt")),
    menuItem("Tables",tabName = "table",icon=icon("table")),
    menuItem("Graphs",tabName = "graph",icon=icon("chart-area")),
  menuItem("Leaders",tabName="table2",icon=icon("chess-knight")),
    menuItem("About",tabName="about_tab",icon=icon("info")),
  fileInput('datafile','Choose file',
            accept = c('text/csv','text/comma-separated-values,text/plain','.csv')
            )
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
                          formattableOutput("mytable")),
                  #tabPanel("worldtable",DT::dataTableOutput("world_ranking")),
                  tabItem(tabName = "europetable",formattableOutput("mytable6")),
                  tabItem(tabName = "southtable",formattableOutput("mytable1")),
                  tabItem(tabName = "northtable",formattableOutput("mytable2")),
                  tabItem(tabName = "africatable",formattableOutput("mytable3")),
                  tabItem(tabName = "oceantable",formattableOutput("mytable4")),
                  tabItem(tabName = "asiatable",formattableOutput("mytable5")),
                  
                  
      tabItem(tabName = "CAF",
              selectInput("date_year",label = strong("Enter Date:"),
                        choices = unique(fifa$rank_date),selected ="6/7/2018"
                        ),
              actionButton("value1","PLOT"),
              plotOutput("africaBest")
      ),
      tabItem(tabName = "UEFA",
              selectInput("date_year1",label = strong("Enter Date:"),
                          choices = unique(fifa$rank_date),selected ="6/7/2018"
              ),
              actionButton("value","PLOT"),
              plotOutput("europebest")),
      tabItem(tabName = "CONMEBOL",
              selectInput("date_year2",label = strong("Enter Date:"),
                          choices = unique(fifa$rank_date),selected ="6/7/2018"
              ),
              actionButton("value2","PLOT"),
              plotOutput("conmebolbest")),
      tabItem(tabName = "AFC",
              selectInput("date_year3",label = strong("Enter Date:"),
                          choices = unique(fifa$rank_date),selected ="6/7/2018"
              ),
              actionButton("value3","PLOT"),
              plotOutput("afcbest")),
      tabItem(tabName = "OFC",
              selectInput("date_year4",label = strong("Enter Date:"),
                          choices = unique(fifa$rank_date),selected ="6/7/2018"
              ),
              actionButton("value4","PLOT"),
              plotOutput("ofcbest")),
      tabItem(tabName = "CONCACAF",
              selectInput("date_year5",label = strong("Enter Date:"),
                          choices = unique(fifa$rank_date),selected ="6/7/2018"
              ),
              actionButton("value5","PLOT"),
              plotOutput("concacafbest")),
      tabItem(tabName = "table",
                   
                  #select input to display confederation, country data 
                  selectInput("Data","Choose Data:",
                              choices = c("World Best","UEFA","CAF","OFC","AFC","CONMEBOL","CONCACAF")),
                  conditionalPanel(condition = "input.Data == 'World Best'"),
                  
                  conditionalPanel(condition = "input.Data == 'UEFA'"),
                                
                  conditionalPanel(condition = "input.Data == 'CAF'"),
                                   
                  
                  conditionalPanel(condition = "input.Data == 'OFC'"),
                                 
                  
                  conditionalPanel(condition = "input.Data == 'CONMEBOL'"),
                                  
                  
                  conditionalPanel(condition = "input.Data == 'CONCACAF'"),
                                 
                  
                  conditionalPanel(condition = "input.Data == 'AFC'"),
              DT::dataTableOutput("plotTable")
                  
                  
                  #tableOutput("plotTable")
                  
              
              ),
      tabItem(tabName = "about_tab",
              uiOutput("abouts")),
      tabItem(tabName = "summary",
              #verbatimTextOutput("summary"),
              plotOutput("top3"),
              plotOutput("top32")
              ),
      tabItem(tabName = "table2", uiOutput("table_html")),
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
  #reading file from the pc 
  filedata<-reactive({
    infile<-input$datafile
    if(is.null(infile)){
      return(NULL)
    }
    data<-read.csv(infile$datapath)
  })
  #rendering formatable tables ##############################################
    output$mytable=renderFormattable({
      world_data1<-fifa%>%filter(rank<=50,rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
           formattable(world_data1,list(total_points=color_text("red","green"),
                                        country_full=color_tile("transparent", "pink"),
                                        rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                              rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
                                        )) 
       })
    
    output$mytable6=renderFormattable({
    
      tabl_select<-fifa%>%filter(confederation=="UEFA",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
})
    output$mytable1=renderFormattable({
      
      tabl_select<-fifa%>%filter(confederation=="CONMEBOL",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
    })
    output$mytable2=renderFormattable({
      
      tabl_select<-fifa%>%filter(confederation=="CONCACAF",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
    })
    output$mytable3=renderFormattable({
      
      tabl_select<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
    })
    output$mytable4=renderFormattable({
      
      tabl_select<-fifa%>%filter(confederation=="OFC",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
    })
    output$mytable5=renderFormattable({
      
      tabl_select<-fifa%>%filter(confederation=="AFC",rank_date=="6/7/2018")%>%select(rank,country_full,confederation,previous_points,rank_change,cur_year_avg,last_year_avg,two_year_ago_avg,three_year_ago_avg,total_points)%>%arrange(desc(total_points))%>%slice(1:216)
      formattable(tabl_select,list(total_points=color_text("red","green"),
                                   country_full=color_tile("transparent", "pink"),
                                   rank_change=formatter("span",style=~style(color=ifelse(rank_change<0,"red","green")),
                                                         rank_change~icontext(ifelse(rank_change<0,"arrow-down","arrow-up"),rank_change))
      )) 
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
    
    
    ggplot(data=dta, aes_string(y=dta$previous_points,x=dta$rank_date,fill=dta$rank_date))  +
      stat_summary( geom = "bar") +
      geom_bar(stat="identity") +
      labs(title=input$country, y ="Total Points Earned",x="Year") +
      scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                     breaks=c("OJ", "VC"),
                     labels=c("Orange juice", "Ascorbic acid")) +
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
  
  
  #display table of world best
  output$plotTable =DT::renderDataTable({
    choosen_data <- input$Data
    Date_input <-  input$date
    Range <-  input$range
    yearly_data<-input$yr
    country_spec<-input$country
    if(choosen_data == "World Best"){
    world_data<-fifa%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))
     ### slice(1:Range)
    agg2<-split(world_data,world_data$country_full)
    trial1 <-do.call(rbind,lapply(agg2, function(chunk) chunk[which.max(chunk$total_points),]))
    world_data<-ddply(trial1,
                       .(rank),function(x)x[x$total_points==max(x$total_points),]
    )
    #best_data<-world_data[,c(1,2,3)]
    #colnames(best_data)<-c("RANK","COUNTRY","POINTS")
    #widget_formattable<-fobest_data)
    datatable(world_data)
   
    
    }else if(choosen_data%in%c("UEFA","CAF","CONCACAF","OFC","AFC","CONMEBOL")){#choosen_data == "UEFA"){
      tabl_select1<-fifa%>%filter(confederation==choosen_data)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))
      agg<-split(tabl_select1,tabl_select1$country_full)
      trial <-do.call(rbind,lapply(agg, function(chunk) chunk[which.max(chunk$total_points),]))
      tabl_select<-ddply(trial,
                         .(rank),function(x)x[x$total_points==max(x$total_points),]
                         )
      #%>%slice(1:Range)
      #selecting the columns to display
     # cho_data<-tabl_select[,c(1,2,3)]
      #The display the table
      datatable(tabl_select)
      #change column names 
      #colnames(cho_data)<-c("WORLD-RANK","COUNTRY","POINTS")
      #widget_formattable<-formattable(cho_data)
    }else if(choosen_data=="Country Data"){
      country_data<-fifa%>%filter(filtered_date<-substring(rank_date,6)==yearly_data,country_abrv==country_spec)%>%select(rank,total_points,rank_change)
      #cont_data<-country_data[,c(1,2,3)]
      
      #The display the table
      datatable(country_data)
      #change column names 
      #colnames(cont_data)<-c("RANK","POINTS","CHANGE")
      #widget_formattable<-formattable(cont_data)
    }

  },striped = T,width = "600",align ="c")
#interactive plots of the bar graphs################################################################################
  #output europe's best
  bar_plot<-eventReactive(
    input$value,{
      date_of_ranking=input$date_year1
      europe_best<-fifa%>%filter(confederation=="UEFA",rank_date==date_of_ranking)%>%select(country_full,total_points)%>%slice(1:10)
      #ggplot(europe_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()
      ggplot(europe_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer(palette="Spectral")+ggtitle("EUROPE'S BEST COUNTRIES")#barplot(europe_best,x=country_full,y=total_points)
    }
  )
  output$europebest <- renderPlot({bar_plot()})
  #output africa best
  caf_plot<-eventReactive(
    input$value1,{date_of_ranking=input$date_year
      africa_best<-fifa%>%filter(confederation=="CAF",rank_date==date_of_ranking)%>%select(rank,country_full,total_points)%>%slice(1:10)
      ggplot(africa_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+ggtitle("AFRICA'S BEST COUNTRIES")+scale_fill_brewer(palette="Spectral")
    })
    
  output$africaBest <- renderPlot({caf_plot()})
  #output asia's best
  Asia_plot<-eventReactive(
    input$value3,{
      date_of_ranking=input$date_year3
      asian_best<-fifa%>%filter(confederation=="AFC",rank_date==date_of_ranking)%>%select(rank,country_full,total_points)%>%slice(1:10)
      ggplot(asian_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+ggtitle("ASIAN'S BEST COUNTRIES")+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer(palette="Spectral")
    }
  )
  output$afcbest <- renderPlot({Asia_plot()})
  #output oceanian best
  Ocean_plot<-eventReactive(
    input$value4,{date_of_ranking=input$date_year4
      oceanian_best<-fifa%>%filter(confederation=="OFC",rank_date==date_of_ranking)%>%select(rank,country_full,total_points)%>%slice(1:10)
      ggplot(oceanian_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer(palette="Spectral")+ggtitle("OCEANIAN'S BEST COUNTRIES")
    }
  )
  output$ofcbest <- renderPlot({Ocean_plot()})
  #output southamerica's best
  conmebol_plot<-eventReactive(
    input$value2,{date_of_ranking=input$date_year2
    southamerica_best<-fifa%>%filter(confederation=="CONMEBOL",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(southamerica_best,aes(x=country_full,y=total_points))+geom_col()+coord_flip()+theme_minimal()+geom_bar(stat = "identity",fill="steelblue")+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer()+ggtitle("SOUTH AMERICAN'S BEST COUNTRIES")
    southamerica_best<-fifa%>%filter(confederation=="CONMEBOL",rank_date==date_of_ranking)%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot(southamerica_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer(palette="Spectral")+ggtitle("SOUTH AMERICAN'S BEST COUNTRIES")

    }
  )
  output$conmebolbest <- renderPlot({conmebol_plot()})
  #output for concaf
  concacaf_plot<-eventReactive(
    input$value5,{date_of_ranking=input$date_year5
    Northamerica_best<-fifa%>%filter(confederation=="CONCACAF",rank_date==date_of_ranking)%>%select(rank,country_full,total_points)%>%slice(1:10)
    ggplot( Northamerica_best,aes(x=country_full,y=total_points,fill=country_full))+geom_col()+coord_flip()+geom_bar(stat = "identity",alpha=.4)+geom_text(aes(label=total_points),nudge_y = 4)+scale_fill_brewer(palette="Spectral")+ggtitle("NORTH AMERICAN'S BEST COUNTRIES")
    }
  )
  output$concacafbest <- renderPlot({concacaf_plot()})
  
  ######################################################
  output$top3<-renderPlot({
    firt_position <- fifa%>%filter(rank<=3)%>%select(country_abrv)
    table_count<-count(firt_position)
    ggplot(table_count,aes(table_count$country_abrv,table_count$freq))+geom_bar(stat="identity", width = 0.5, fill="tomato2")+labs(title="BAR CHART",subtitle="COUNTRIES THAT OCCUPPIED THE TOP 3",y="APPEARENCES", x="COUNTRY")+theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  output$top32<-renderPlot({
    firt_position <- fifa%>%filter(rank<=3)%>%select(country_abrv)
    table_count<-count(firt_position)
    ggplot(table_count,aes(table_count$country_abrv,table_count$freq))+geom_count(col="tomato3", show.legend=F)+labs(subtitle="Appearence Vs Country (TOP 3 COUNT)",y="APPEARENCES", x="COUNTRY",title="Counts Plot")
  })
  #displaying html data
  output$abouts<-reactiveUI(function(){
    file_to_show='aboutus.Rhtml'
    HTML(readLines(file_to_show))
  })
  output$table_html<-reactiveUI(function(){
    file_to_show2="leaders.Rhtml"
    HTML(readLines(file_to_show2))
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

