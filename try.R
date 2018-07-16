library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(readr)
library(png)
library(tidyr)
library(shinythemes)
library(dplyr)
library(formattable)
library(markdown)
library(lubridate)
library(plyr)
dates <- as.Date(data$rank_date, 
                 format = '%Y-%m-%d')
data<-read.csv(file.choose(),header = TRUE)
data2<-ddply(data,.(country_full,confederation),
            summarize,Total_Points_Earned=sum(previous_points))
desc <- data2[order(-data2$Total_Points_Earned), ]
#analysis for 2018 

data3 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2018-01-18"), 
                 as.Date("2018-06-07")))

data4<-ddply(data3,.(country_full,confederation),
             summarize,Total_Points_Earned=sum(previous_points))
desc1 <- data4[order(-data4$Total_Points_Earned), ]
#analysis for 2017
data5 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2017-01-12"), 
                 as.Date("2017-12-21")))
data_sorted_2017<-ddply(data5,.(country_full,confederation),
                        summarize,Points_2017=sum(previous_points))

desc2 <- data_sorted_2017[order(-data_sorted_2017$Points_2017), ]
#analysis for 2016

data6 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2016-01-07"), 
                 as.Date("2016-12-22")))
data_sorted_2016<-ddply(data6,.(country_full,confederation),
                        summarize,Points_2016=sum(previous_points))
desc3 <- data_sorted_2016[order(-data_sorted_2016$Points_2016), ]

#analysis for 2015

data7 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2015-01-08"), 
                 as.Date("2015-12-03")))
data_sorted_2015<-ddply(data7,.(country_full,confederation),
                        summarize,Point_2015=sum(previous_points))
desc4 <- data_sorted_2015[order(-data_sorted_2015$Point_2015), ]

#analysis for 2014
data8 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2014-01-16"), 
                 as.Date("2014-12-18")))
#total perfomnce in Year 2014
data_sorted_2014<-ddply(data8,.(country_full,confederation),
                        summarize,Points_2014=sum(previous_points))
#sort in descending order
desc5 <- data_sorted_2014[order(-data_sorted_2014$Points_2014), ]
#analysis of 2013
data9 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2013-01-17"), 
                 as.Date("2013-12-19")))
#total perfomnce in Year 2013

data_sorted_2013 <-ddply(data9,.(country_full,confederation),
                         summarize,Point_2013=sum(previous_points))
#sort in descending order
desc6 <- data_sorted_2013[order(-data_sorted_2013$Point_2013), ]
#analysis for 2012
data10<-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2012-01-18"), 
                 as.Date("2012-12-19")))
#total perfomnce in Year 2012

data_sorted_2012 <-ddply(data10,.(country_full,confederation),
                         summarize,Points_2012=sum(previous_points))
#sort in descending order
desc7 <- data_sorted_2012[order(-data_sorted_2012$Points_2012), ]
#analysis for 2011
data11 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2011-01-12"), 
                 as.Date("2011-12-21")))
#total perfomnce in Year 2011

data_sorted_2011 <-ddply(data11,.(country_full,confederation),
                         summarize,Point_2011=sum(previous_points))
#sort in descending order
desc8<- data_sorted_2011[order(-data_sorted_2011$Point_2011), ]
#analysis for 2010
data12 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2010-02-03"), 
                 as.Date("2010-12-15")))
#total perfomnce in Year 2010

data_sorted_2010 <-ddply(data12,.(country_full),
                         summarize,Points_2010=sum(previous_points))

#sort in descending order
desc9 <- data_sorted_2010[order(-data_sorted_2010$Points_2010), ]
#analysis for 2009
data13 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2009-01-14"), 
                 as.Date("2009-12-16")))
#total perfomnce in Year 2010

data_sorted_2009 <-ddply(data13,.(country_full),
                         summarize,Point_2009=sum(previous_points))

#sort in descending order
desc10 <- data_sorted_2009[order(-data_sorted_2009$Point_2009), ]
#analysis for 2008
data14 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2008-01-16"), 
                 as.Date("2008-12-17")))
#total perfomnce in Year 2008

data_sorted_2008 <-ddply(data14,.(country_full),
                         summarize,Point_2008=sum(previous_points))
#sort in descending order
desc11<- data_sorted_2008[order(-data_sorted_2008$Point_2008), ]
#analysis for 2007
data15 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2007-01-17"), 
                 as.Date("2007-12-17")))
#total perfomnce in Year 2007

data_sorted_2007 <-ddply(data15,.(country_full),
                         summarize,Points_2007=sum(previous_points))
data_sorted_2007
#sort in descending order
desc12 <- data_sorted_2007[order(-data_sorted_2007$Points_2007), ]
#analysis for 2006

data16 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2006-01-18"), 
                 as.Date("2006-12-18")))
#total perfomnce in Year 2006
data_sorted_2006 <-ddply(data16,.(country_full),
                         summarize,Points_2006=sum(previous_points))
#sort in descending order
desc13 <- data_sorted_2006[order(-data_sorted_2006$Points_2006), ]
#analysis for 2005
data17 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2005-01-19"), 
                 as.Date("2005-12-16")))
#total perfomnce in Year 2005
data_sorted_2005 <-ddply(data17,.(country_full),
                         summarize,Points_2005=sum(previous_points))
data_sorted_2005
#sort in descending order
desc14 <- data_sorted_2005[order(-data_sorted_2005$Points_2005), ]

#analysis for 2004
data18 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2004-01-14"), 
                 as.Date("2004-12-20")))
#total perfomnce in Year 2004
data_sorted_2004 <-ddply(data18,.(country_full),
                         summarize,Points_2004=sum(previous_points))
#sort in descending order
desc15 <- data_sorted_2004[order(-data_sorted_2004$Points_2004), ]
#analysis for 2003

data19 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2003-01-15"), 
                 as.Date("2003-12-15")))
#total perfomnce in Year 2003
data_sorted_2003 <-ddply(data19,.(country_full),
                         summarize,Points_2003=sum(previous_points))

#sort in descending order
desc16 <- data_sorted_2003[order(-data_sorted_2003$Points_2003), ]

#analysis for 2002
data20 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2002-01-16"), 
                 as.Date("2002-12-18")))
#total perfomnce in Year 2002

data_sorted_2002 <-ddply(data20,.(country_full),
                         summarize,Points_2002=sum(previous_points))
#sort in descending order
desc17 <- data_sorted_2002[order(-data_sorted_2002$Points_2002), ]
#analysis for 2001
data21 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2001-01-17"), 
                 as.Date("2001-12-19")))
#total perfomnce in Year 2001
data_sorted_2001 <-ddply(data21,.(country_full),
                         summarize,Points_2001=sum(previous_points))
#sort in descending order
desc18 <- data_sorted_2001[order(-data_sorted_2001$Points_2001), ]
#analysis for 2000
data22 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2000-01-19"), 
                 as.Date("2000-12-20")))
#total perfomnce in Year 2000

data_sorted_2000 <-ddply(data22,.(country_full),
                         summarize,Points_2000=sum(previous_points))
#sort in descending order
desc19 <- data_sorted_2000[order(-data_sorted_2000$Points_2000), ]
#analysis for 1999
data23 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1999-01-27"), 
                 as.Date("1999-12-22")))
#total perfomnce in Year 1999

data_sorted_1999 <-ddply(data23,.(country_full),
                         summarize,Points_1999=sum(previous_points))
#sort in descending order
desc20 <- data_sorted_1999[order(-data_sorted_1999$Points_1999), ]
#analysis for 1998
data24 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1998-02-18"), 
                 as.Date("1998-12-23")))
#total perfomnce in Year 1998

data_sorted_1998 <-ddply(data24,.(country_full),
                         summarize,Points_1998=sum(previous_points))

#sort in descending order
desc21 <- data_sorted_1998[order(-data_sorted_1998$Points_1998), ]
#analysis for 1997

data25 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1997-02-27"), 
                 as.Date("1997-12-23")))
#total perfomnce in Year 1997
data_sorted_1997 <-ddply(data25,.(country_full),
                         summarize,Points_1997=sum(previous_points))

#sort in descending order
desc22 <- data_sorted_1997[order(-data_sorted_1997$Points_1997), ]
#analsis for 1996

data26 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1996-01-24"), 
                 as.Date("1996-12-18")))
#total perfomnce in Year 1996

data_sorted_1996 <-ddply(data26,.(country_full),
                         summarize,Points_1996=sum(previous_points))
#sort in descending order
desc23 <- data_sorted_1996[order(-data_sorted_1996$Points_1996), ]

#analysis for 1995
data27 <-data  %>%
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1995-02-20"), 
                 as.Date("1995-12-19")))
#total perfomnce in Year 1995
data_sorted_1995 <-ddply(data27,.(country_full),
                         summarize,Points_1995=sum(previous_points))
#sort in descending order
desc24 <- data_sorted_1995[order(-data_sorted_1995$Points_1995), ]
#analysis for 1994
data28 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1994-02-15"), 
                 as.Date("1994-12-20")))
#total perfomnce in Year 1994

data_sorted_1994 <-ddply(data28,.(country_full),
                         summarize,Points_1994=sum(previous_points))
#sort in descending order
desc25 <- data_sorted_1994[order(-data_sorted_1994$Points_1994), ]
#analysis for 1993
data29 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1993-08-08"), 
                 as.Date("1993-12-23")))
#total perfomnce in Year 1993
data_sorted_1993 <-ddply(data29,.(country_full),
                         summarize,Points_1993=sum(previous_points))
#sort in descending order
desc26 <- data_sorted_1993[order(-data_sorted_1993$Points_1993), ]

ui <- dashboardPage(
  dashboardHeader(title = "FiFA World Ranking System"),
  dashboardSidebar(
    
    #the logo for the site
    imageOutput("image1",height = 30),
    
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
      menuItem( tags$h4("Best Team"),
                
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
      navbarPage("YEAR",
                 tabPanel("WORLD-BEST", 
                          navbarMenu("World-best-teams",tabPanel("desc", DT::dataTableOutput("desc")) )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 ),
                 navbarMenu("More",
          
                            tabPanel("2018",DT::dataTableOutput("table1")),
                            tabPanel("2017",DT::dataTableOutput("table2")),
                            tabPanel("2016",DT::dataTableOutput("table3")),
                            tabPanel("2015",DT::dataTableOutput("table4")),
                            tabPanel("2014",DT::dataTableOutput("table5")),
                            tabPanel("2013",DT::dataTableOutput("table6")),
                            tabPanel("2012",DT::dataTableOutput("table7")),
                            tabPanel("2011",DT::dataTableOutput("table8")),
                            tabPanel("2010",DT::dataTableOutput("table9")),
                            tabPanel("2009",DT::dataTableOutput("table10")),
                            tabPanel("2008",DT::dataTableOutput("table11")),
                            tabPanel("2007",DT::dataTableOutput("table12")),
                            tabPanel("2006",DT::dataTableOutput("table13")),
                            tabPanel("2005",DT::dataTableOutput("table14")),
                            tabPanel("2004",DT::dataTableOutput("table15")),
                            tabPanel("2003",DT::dataTableOutput("table16")),
                            tabPanel("2002",DT::dataTableOutput("table17")),
                            tabPanel("2001",DT::dataTableOutput("table18")),
                            tabPanel("2000",DT::dataTableOutput("table19")),
                            tabPanel("1999",DT::dataTableOutput("table20")),
                            tabPanel("1998",DT::dataTableOutput("table21")),
                            tabPanel("1997",DT::dataTableOutput("table22")),
                            tabPanel("1996",DT::dataTableOutput("table23")),
                            tabPanel("1995",DT::dataTableOutput("table24")),
                            tabPanel("1994",DT::dataTableOutput("table25")),
                            tabPanel("1993",DT::dataTableOutput("table26"))
                 ),
                 textOutput("result"),
                 tabPanel("Data",
                          # Select type of trend to plot
                          selectInput(inputId = "country_full", label = strong("Trend index"),
                                      choices = unique(data$country_full),
                                      selected = "Brazil")
                          ,
                          mainPanel(
                            plotOutput(outputId = "lineplot", height = "300px",width="800px")
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
                                  column(6,dateInput("date","insert date",value = "6/7/2018",format = "mm/dd/yyyy")),
                                  
                                  column(6,numericInput("range","insert range",50))),
                 
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
                                  textInput("date","insert date","6/7/2018")),
                 
                 
                 tableOutput("plotTable")
             )
             
             
             
             
      )
    )
  )
)



server <- function(input,output){
  output$result <- renderText({
    paste("You chose", input$More)
  })
  # Subset data
  selected_trends <- reactive({
    req(input$country_full)
    count <- input$country_full
    
    subset(data,country_full=="count")
     
  })
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
  #par(mar = c(4, 4, 1, 1))
 # plot(selected_trends()$previous_points)
   # plot(x = selected_trends()$previous_points, y=selected_trends()$rank_change, type = "l",
    #     xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    ggplot(selected_trends(),aes(x=previous_points,
                                y=rank_change))+geom_col()+coord_flip()
  })
  output$desc = DT::renderDataTable({
    desc
  })
  
  output$summary <- renderPrint({
    summary(desc)
  })
  
  output$table1 = DT::renderDataTable({
    desc1
  })
  output$table2 = DT::renderDataTable({
    desc2
  })
  output$table3 = DT::renderDataTable({
    desc3
  })
  output$table4 = DT::renderDataTable({
    desc4
  })
  output$table5= DT::renderDataTable({
    desc5
  })
  output$table6= DT::renderDataTable({
    desc6
  })
  output$table7= DT::renderDataTable({
    desc7
  })
  output$table8= DT::renderDataTable({
    desc8
  })
  output$table9= DT::renderDataTable({
    desc9
  })
  output$table10= DT::renderDataTable({
    desc10
  })
  output$table11= DT::renderDataTable({
    desc11
  })
  output$table12= DT::renderDataTable({
    desc12
  })
  output$table13= DT::renderDataTable({
    desc13
  })
  output$table14= DT::renderDataTable({
    desc14
  })
  output$table15= DT::renderDataTable({
    desc15
  })
  output$table16= DT::renderDataTable({
    desc16
  })
  output$table17= DT::renderDataTable({
    desc17
  })
  output$table18= DT::renderDataTable({
    desc18
  })
  output$table19= DT::renderDataTable({
    desc19
  })
  output$table20= DT::renderDataTable({
    desc20
  })
  output$table21= DT::renderDataTable({
    desc21
  })
  output$table22= DT::renderDataTable({
    desc22
  })
  output$table23= DT::renderDataTable({
    desc23
  })
  output$table24= DT::renderDataTable({
    desc24
  })
  output$table25= DT::renderDataTable({
    desc25
  })
  output$table26= DT::renderDataTable({
    desc26
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
    if(choosen_data == "World Best"){
      world_data<-fifa%>%filter(rank<=Range,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:Range)
      best_data<-world_data[,c(1,2,3)]
      colnames(best_data)<-c("RANK","COUNTRY","POINTS")
      widget_formattable<-formattable(best_data)
      
      
    }else if(choosen_data == "UEFA"){
      tabl_select<-fifa%>%filter(confederation==choosen_data,rank_date==Date_input)%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:Range)
      #selecting the columns to display
      #new_data<-within(tabl_select,rank1<-ave(total_points,FUN = function(x)rev(order(x))))
      cho_data<-tabl_select[,c(1,2,3)]
      #The display the table
      
      #change column names 
      colnames(cho_data)<-c("WORLD-RANK","COUNTRY","POINTS")
      widget_formattable<-formattable(cho_data)
      # data3<-cho_data%>%kable()%>%kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),font_size = 15,position = "center")%>%row_spec(0,color = "white",background = "green")%>%cat(.,file = "data.html")
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

