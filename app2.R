

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
ui <- 
  
  dashboardPage(
    dashboardHeader(title = strong(h2("CRYPTOCURRENCY DATA ANALYSIS PROJECT")),titleWidth = 500),
    
    dashboardSidebar(
      #cryptocurrency image
      tags$img(src='image2.jpg', width="100"),
      ##########search input##################
      sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
      
      #######end of search#####################################
      
      sidebarMenu(
        menuItem(strong("Product Description"),tabName = "des"),
        
        ###############uploadfile########
        menuItem("RawData Upload",icon = icon("upload"),
                 
                 fileInput(inputId ="file","choose File",
                           accept =c('text/csv','text/comma-separated-values,text/plain','.csv')), 
                 
                 helpText(strong("select read.table parameters")),
                 checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                 checkboxInput(inputId = "stringAsFactors",label = "stringAsFactors",value = FALSE)
                 
                 
                 
        ),
        
        ##########end of uploading file AND start of data#################
        menuItem(strong("Data"), tabName = "data", icon = icon("table")),
        
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
        
        
        menuItem(strong("PREDICT"),tabName = "predict")
        #######end predict 
        
        
      )),
    
    dashboardBody(
      
      
      
      tabItems(
        
        #defining data output system
        #  tab content
        tabItem(tabName = "des",
                tabsetPanel(
                  tabPanel("Detailed Analysis of Cryptocurrencies"))
                
                
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
                
                uiOutput("tops")
                
        ),
        
        #  tab content
        tabItem(tabName = "corr",
                
                uiOutput("corrs")
                
        ),
        
        #  tab content
        tabItem(tabName = "price",
                
                uiOutput("prices")
                
        ),
        
        #  tab content
        tabItem(tabName = "data",
                ###start############
                tabsetPanel(id="tabs",
                            tabPanel("uploaded_data",icon = icon("table"), 
                                     uiOutput("tb")
                            ))
                
        )
        
        
        
      )
    ))

options(shiny.maxRequestSize=50*1024^2)
server <- function(input,output,session){
  #reactive function takes inputs from ui.r and uses read.table to read data
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file$datapath, header= input$header, stringsAsFactors = input$stringAsFactors)
  })
  #this output contains summary of dataset in table format
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  #this output contains summary of dataset in table format
  output$sum <- renderTable({
    if(is.null(data())){return()}
    summary(data())
  })
  #this output contains the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  #this output dynamically generates tabsets when file is loaded
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Sorry Excuse Us")
    else
      tabsetPanel(tabPanel("About file",tableOutput("filedf")),
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")))
    
  })
  
  output$tops <- renderUI({
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
    
  })
  
  output$corrs <- renderUI({
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
  })
  
  output$prices <- renderUI({
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
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)