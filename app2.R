library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)
library(colorspace)
library(gridExtra)
library(memisc)
library(sqldf)
library(lubridate)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(quanteda)
library(syuzhet)
library(maps)
library(mapdata)
library(forecast)
library(plotrix)

#desiging a user interface
shinyUI <- 
  
  dashboardPage(
    dashboardHeader(title = strong(h2("CRYPTOCURRENCY ANALYSIS PROJECT")),titleWidth = "500"),
    
    dashboardSidebar(
      
      #cryptocurrency image
      tags$img(src='image2.jpg', width="100"),
      ##########search input##################
      sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
      
      #######end of search#####################################
      sidebarMenu(
        
        
        
        ###############uploadfile########
        menuItem("RawData Upload",tabName = "upload", icon = icon("upload"),
                 menuSubItem(icon = icon("upload"),
                             fileInput(inputId ="file","choose File",
                                       accept =c('text/csv','text/comma-separated-values,text/plain','.csv')) )),
        ##########end of uploading file AND start of data#################
        
        
        
        menuItem("Data", tabName = "data", icon = icon("table")),
        ####33end of data and begin closeprice analysis with all graphs######
        menuItem(strong("ANALYSIS OF PRICE"), tabName = "price"),
        selectInput("var","select price type", c("ClosePrice"=7, "OpenPrice"=4, "HighPrice"=5, "LowPrice"=6)),
        
        
        #######end of analysis in closeprice and start correlation analysis################
        
        menuItem(strong("CORRELATION ANALYSIS"), tabName="corr"),
        
        
        #####end of correlation analysis and begin in analysis in market cap
        menuItem(strong("MARKET CAP ANALYSIS"), tabName = "market"),
        
        ######end in mkt cap and begin top 10 analysis###########
        
        
        menuItem(strong("TOP 10 ANALYSIS"),tabName = "top"),
        ######end of top 10 and begin predict ######
        
        
        menuItem(strong("PREDICT"),tabName = "predict"),
        #######end predict and then project description
        menuItem(strong("Project Description"),tabName = "des")
      )),
    
    ###########begin of the body#############
    dashboardBody(
      
      tabItems(
        #defining data output system
        tabItem(tabName = "data",
                ###start############
                tabsetPanel(id="tabs",
                            tabPanel("uploaded_data",icon = icon("table"), dataTableOutput("mytable1"),dataTableOutput("mytable2")))
                
                #end tab and start of analysis in close price
                
        ),
        #tab content
        tabItem(tabName = "price",
                tabsetPanel(
                  tabPanel("Analysed in var price",icon = icon("table"),
                           width=12,tags$b("var price analysis"),
                           p("The table shows the analysed var prices of all of cryptocurrencies")
                           
                  ),
                  ######tab for the barcharts
                  tabPanel("BarPlot",icon = icon("bar-chart-o"),
                           plotOutput("var price", width = 100,height = 100)
                  ),
                  
                  tabPanel("LineGraph",icon = icon("line-chart"),
                           plotOutput("var price", width = 100,height = 100)
                  ),
                  
                  tabPanel("ScatterPlot",icon = icon("line-chart"),
                           plotOutput("var price", width = 100,height = 100)
                  )
                  ####end
                  
                )
                
                
        ),
        tabItem(tabName = "corr",
                
                tabsetPanel( 
                  tabPanel("btn all cryptocurrencies",icon = icon("bar-chart-o"),
                           plotOutput("var price",width = 100,height = 100)
                  ),
                  
                  tabPanel("+bitcoins",icon = icon("table"),
                           plotOutput("var price",width = 100,height = 100)
                  ),
                  
                  tabPanel("-bitcoins",icon = icon("table"),
                           plotOutput("var price",width = 100,height = 100)
                  )
                  ####end
                )
                
        ),
        #  tab content
        tabItem(tabName = "market",
                
                tabsetPanel(
                  tabPanel("average market cap", icon = icon("bar-chart-o"))
                )
        ),
        #  tab content
        tabItem(tabName = "predict",
                fluidRow(
                  box(plotOutput("plot1", height = 250))
                  
                )
        ),
        
        #  tab content
        tabItem(tabName = "top",
                
                tabsetPanel( 
                  tabPanel("Mean",icon = icon("table"),
                           plotOutput("var price",width = 100,height = 100)
                  ),
                  
                  tabPanel("Variance",icon = icon("table"),
                           plotOutput("var price",width = 100,height = 100)
                  ),
                  
                  tabPanel("Log-return",icon = icon("table"),
                           plotOutput("var price",width = 100,height = 100)
                  )
                  ####end
                )
                
                
        )
        ############tab for the charts here in the close prices#############
        
        ############end of the chartsin closeprice and##################
      )
      ############end of the body#################
    ))


options(shiny.maxRequestSize=50*1024^2)
server <- function(input,output,session){
  #implementing file input##############
  myData <- reactive({
    file1<-input$file
    if(is.null(file))return(NULL)
    
    data<-read.csv(file1$datapath, header = TRUE)
    data
  })
  ###########end and start of output uploaded data#######
  output$contents <- renderTable(
    myData()
  )
  ####eerror ########
  read<-function(){
    if(is.null(input$file))
      return(NULL)
    library(ggplot2)
    
    infile<-input$file
    data<-read.csv(infile$datapath)
    
    #######variables for the data titles in the  set#########
    df<-data.frame(
      Date=data$Date,
      Symbol=data$Symbol,
      Open=data$Open,
      Close=data$Close,
      Volume=data$Volume,
      MarketCap=data$MarketCap
      
    )
    df ####reading the output#############################
  }
  # reading the two tables in the data frame###############
  output$mytable1<-renderDataTable({read()[,1:6]})
  
  ##############end of output and start of analysis in close price barplot####################################################
  
  output$closeprice <- renderPlot({
    d <- barplot(data$Close,data$Date, ylab = "close price", xlab = "date")
  })
  ###############end in closeprice analysis and start in correlation analsis#############
}

shinyApp(ui = shinyUI , server = server)