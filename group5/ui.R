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

#This section begins the user interfacez
ui <- fluidPage(theme="style.css",
                tags$img(src="crypto7.png", width=100,height=100),
  dashboardHeader(title = "CRYPTOCURRENCY DATA ANALYSIS PROJECT",titleWidth = "500"),

  dashboardSidebar(
    #tags$style("body{background:black;color:brown}"),
  #tags$img(src="one.jpg", width=100,height=100),
    #menuItem("PROJECT MEMBERS",tabName = "MEMBERS", icon =icon("data")),
    
    
  sidebarMenu( 
    
    
    menuItem("RawData Upload",tabName ="upload",icon =icon("upload"),
                                         menuSubItem(icon = icon("none"),
                                                     
                    fileInput(inputId ="file","choose File",accept =c('text/csv','text/comma-separated-values,text/plain','.csv')))),
   
                                menuItem("Data",tabName ="data",icon =icon("table")),
                                menuItem("ANALYSIS IN CLOSE PRICE",tabName = "closeprice", icon =icon("pie-chart")),
                                menuItem("CORRELATION ANALYSIS",tabName = "cor",icon = icon("bar-chart")),
                                menuItem("ANALYIS IN MARKET CAP",tabName = "cap",icon = icon("bar-chart")),
                                menuItem("TOP 10 ANALYSIS",tabName = "cap",icon = icon("bar-chart")),
                                menuItem("DESTRIBUTION ANALYSIS",tabName = "cap",icon = icon("bar-chart")),
                                menuItem("PREDICT",tabName = "predict")
                               #menuItem("",tabName = "cap",icon = icon("bar-chart"))
    
                                

  )
  ),
  #here is start the body
  dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "data",
                tabsetPanel(id="tabs",
                            tabPanel(value="panel1", "Raw-Data",icon = icon("table"), dataTableOutput("data5"))
                ))),#tabitems end for data processing
    #,,,,,,,,,,,,,,,,,,,,,,,closeprice analysis,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
    tabItems(
      tabItem(tabName = "closeprice",
              tabsetPanel(id="tabs",
                          tabPanel(value="panel", "Statement",icon = icon("table"),
                                   tags$b("The newest cryptocurrency"),
                                       p("The table shows how long the cryptocurrency has been on market and sorts out the newest cryptocurrency"),
                                   
                                   textOutput("statement"),
                                   textOutput("crypto")),
                                 tabPanel("SummaryData",p("Summary table about how long the cryptocurrencies when unit is a day"), tableOutput("dayunit"),
                                  p("Summary table about how long the cryptocurrencies in percentage"),tableOutput("per")),
                                 tabPanel("Plot", plotOutput("pie1")))
                                   
                          
              )),#tabitems for close price end
    #,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,correlation analysis,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
    tabItems(
      tabItem(tabName = "cor",
              tabsetPanel(id="tabs",
                          #data for ploting cor plots
                          tabPanel("summarised data", tableOutput("data2"),downloadButton("downloadc1","download")),
                          #corr for all
                          tabPanel(value="panel3", "Correlation between all",icon = icon("bar-chart"),sliderInput("cor.bit[1]", "Input", 1444, 2888, c(1444, 2888)),
                                   plotOutput("his1"),downloadButton("downloadc2","downloadPlot")),
                                  tabPanel("bitcoins and alt",plotOutput("his2"),downloadButton("downloadc3","downloadPlot")),
                          #top 5 correlation with BTC
                          tabPanel("Top5 correlation with BTC",tableOutput("top5positive"),br(),tableOutput("top5negative")))
              )),
    
    #,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,market cap analysis,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
    tabItems(
      tabItem(tabName = "cap",
              tabsetPanel(id="tabs",
                          tabPanel(value="panel3", "Average market cap",icon = icon("table"),
                                   tableOutput("table4")),
                                  tabPanel("Top 10 average market cap",tableOutput("table6")),
                                 tabPanel("Plot for Top 10",plotOutput("bar1"))
                                
              ))),
    
    strong("WELCOME TO WORLD OF CRYPTOCURENCY"),
    hr(),h2("About our Data set"),
    p("Recent growing interest in cryptocurrencies, specifically as a speculative investment vehicle, has sparked global conversation over the past 12 months. 
      Although this data is available across various sites, there is a lack of understanding as to what is driving the exponential rise of many individual currencies. 
      This data set is intended to be a starting point for a detailed analysis into what is driving price action, and what can be done to predict future movement."),
   hr(),
   h2("contents"),
   p("Consolidated financial information for the top 736 cryptocurrencies by marketcap. Attributes include:

     Currency name (e.g. bitcoin)
     Date
     Open
     High
     Low
     Close
     Volume
     Marketcap
     "),
   p("Questions to be answered:

     What is the correlation between bitcoin and alt coin prices?
     What is the average age of the top 10 coins by market cap?
     What day of the week is best to buy/sell?
     Which coins in the top two hundred are less than 6 months old?
     Which currencies are the most volatile? "),
     tabItems(#names of members
      tabItem(tabName = "MEMBERS",
              
                          tabPanel(value="panel3",h3("MEMBERS AND THERE PROFILES"),
                                   h3("Name: MPIIMA COLINS"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:collinsmpiima@gmail.com"),h6("Regno.16/U/7120/ps"),br(),
                                   
                                   h3("Name: GUMA IVAN"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:gumaivan@gmail.com"),h6("Regno.16/U/7120/ps"),br(),
                                   h3("Name: NATWIJUKIRE CRISPUS"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:natwijukirecrispus@gmail.com"),h6("Regno.16/U/7120/ps"),br(),
                                   h3("Name: NYANZI SAMUEL"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("nyanzisamuel.com"),h6("Regno.16/U/7120/ps"),br()
                          
                         
              )))
  )#body end
)