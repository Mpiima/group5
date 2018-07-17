


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
      
      
      sidebarMenu(
        menuItem(strong("Product Description"),tabName = "des"),
        
        ###############uploadfile########
        menuItem("RawData Upload",icon = icon("upload"),
                 
                 fileInput(inputId ="file","choose File",
                           accept =c('text/csv','text/comma-separated-values,text/plain','.csv')) 
                 
                 #helpText(strong("select read.table parameters")),
                 #checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                 #checkboxInput(inputId = "stringAsFactors",label = "stringAsFactors",value = FALSE)
                 
                 
                 
        ),
        
        ##########end of uploading file AND start of data#################
        menuItem(strong("Data"), tabName = "data", icon = icon("table")),
        
        ####33end of data and begin closeprice analysis with all graphs######
        menuItem(strong("ANALYSIS OF PRICE"), tabName = "price"),
        
        
        
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
                
                uiOutput("prices"),
                uiOutput("prices1"),
                uiOutput("prices2")
                
        ),
        
        #  tab content
        tabItem(tabName = "data",
                
                fluidPage(
                  tabBox(width = 200, height = 800,
                         tabPanel(strong("About file"),
                                  box(background ="teal",width = 100, height = 400,tableOutput("filedf"))),
                         tabPanel(strong("Data"),
                                  box(background ="teal",width = 100, height = 400, tableOutput("table"))),
                         tabPanel(strong("Summary"),
                                  box(background ="teal",width = 100, height = 500, verbatimTextOutput("sum"))),
                         tabPanel(strong("Structure"),
                                  box(background ="teal",width = 100, height = 400, verbatimTextOutput("str"))))                     
                ))
        
      )
      
      
      
      
    ))

options(shiny.maxRequestSize=50*1024^2)
server <- function(input,output,session){
  #reactive function takes inputs from ui.r and uses read.table to read data
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    data1<-read.csv(file1$datapath, header = TRUE)
    data1
    # read.table(file=file$datapath, header= input$header, stringsAsFactors = input$stringAsFactors)
  })
  #this output contains summary of dataset in table format
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  #this output contains structure of dataset in table format
  output$str <- renderPrint({
    if(is.null(data())){return()}
    str(data())
    
  })
  
  #this output contains summary of dataset in table format
  output$sum <- renderPrint({
    if(is.null(data())){return()}
    summary(data())
    
  })
  #this output contains the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    
    head(data())
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
  
  output$myhist <- renderPlot({     
    first <- input$var
    if(is.null(data()))
      h5("Sorry Excuse Us")
    else 
    {
      if(first==4){
        data() %>% select(Date,Open) %>% group_by(Date) %>%ggplot(
          aes(Date,Open,fill=Open ))+geom_col()+ggtitle("The Change in Open Price")+
          labs(x ='Date', y ='OpenPrice')
      } 
      else if(first==5){
        dat1 %>% select(Date,High) %>% group_by(Date) %>%ggplot(
          aes(Date,High,fill=High))+geom_col()+ggtitle("The Change in High Price")+
          labs(x ='Date', y ='HighPrice')
        
      } 
      else if(first==6){
        dat1 %>% select(Date, Low) %>% group_by(Date) %>%ggplot(
          aes(Date,Low, fill=Low ))+geom_col()+ggtitle("The Change in Low Price")+
          labs(x ='Date', y ='LowPrice')
      }
      else if(first==7){
        dat1 %>% select(Date,Close) %>% group_by(Date) %>%ggplot(
          aes(Date,Close,fill= Close))+ggtitle("The Change in Close Price")+
          labs(x ='Date', y ='ClosePrice')
      }}})
  
  output$prices <- renderUI({
    fluidRow(
      box( side="center",background="red",
           sliderInput("size", "Point size:", min=0, max=4, value=2, step = 0.2, animate = T ),
           
           selectInput(inputId = "bar","select price type", c("OpenPrice"=4,"HighPrice"=5,"LowPrice"=6,"ClosePrice"=7), multiple = FALSE)
      )
    )})
  
  output$prices1 <- renderUI({
    fluidRow(
      
      
      tabBox( width = 500, height = 500, 
              tabPanel("Analysed in var price",icon = icon("table"),
                       box(background ="teal",width = 400, height = 400, 
                           p("The table shows the analysed var prices of all of cryptocurrencies"))),
              
              tabPanel("BarGraph",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 400, height = 400, plotOutput("myhist"))),
              tabPanel("LineGraph",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 400, height = 400, plotOutput("myhist"))),
              tabPanel("Scatter",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 400, height = 400, plotOutput("myhist")))
              
      )
      
    )})
  
  output$prices2 <- renderUI({
    fluidRow(
      box(downloadButton(outputId="downloadData", label = "Download the plot"),background = "olive",width = 3)
    )
    ####end
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)