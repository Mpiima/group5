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
shinyUI(
  
  dashboardPage(
    dashboardHeader(title = em("CRYPTOCURRENCY ANALYSIS PROJECT"),titleWidth = "500"),
    
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
      
      menuItem(strong("ANALYSIS IN CLOSE PRICE"), tabName = "closeprice"),
      menuSubItem("barplot",tabName = "closepricecharts",icon = icon("bar-chart-o")),
      menuSubItem("Linegraph", tabName = "closepricelinegraph", icon = icon("line-chart")),
      menuSubItem("scatter",tabName = "closepricescatter",icon = icon("line-chart")),
      #######end of analysis in closeprice and start correlation analysis################
      
      menuItem(strong("CORRELATION ANALYSIS"), tabName=""),
      menuSubItem("btn all cryptocurrencies", tabName = "all", icon = "bar-chart-o"),
      menuSubItem(" Top5+correlated-with-bitcoin", tabName = "+bitcoins", icon = icon("table")),
      menuSubItem(" Top5-correlated-with-bitcoin", tabName = "-bitcoins", icon = icon("table")),
      
      #####end of correlation analysis and begin in analysis in market cap
      menuItem(strong("MARKET CAP ANALYSIS"), tabName = "market capital"),
      menuSubItem("average market cap", tabName = "mrktcap", icon = icon("bar-chart-o")),
      
      
      ######end in mkt cap and begin top 10 analysis###########
      
      
      menuItem(strong("TOP 10 ANALYSIS"),tabName = "top 10"),
      ######end of top 10 and begin predict ######
      menuSubItem("mean", tabName = "mean", icon = icon("table")),
      menuSubItem(" variance", tabName = "variance", icon = icon("table")),
      menuSubItem("log-return", tabName = "log", icon = icon("table")),
      
      
      menuItem(strong("predict")),
      #######end predict and then project description
      menuItem("Project Description")
    )),
    
    ###########begin of the body#############
    dashboardBody(
      tabItems(
        #defining data output system
        tabItem(tabName = "data",
          ###start############
          tabsetPanel(
            type = "tab",
            tabsetPanel(id="tabs",
                        tabPanel(value="panel1", "uploaded_data",icon = icon("table"), dataTableOutput("mytable1"),dataTableOutput("mytable2")))
          )
        )#end tab and start of analysis in close price
                    
        ),
      textOutput(bash),
      tabItem(tabName = "closeprice",
              tabsetPanel(
                type="tab",
                tabsetPanel(id="tabs",
                            tabPanel(value="panel1", "analysis in close price",icon = icon("table"),
                                     box(width=12,tags$b("close price analysis"),
                                         p("The table shows the analysed close prices of all of cryptocurrencies")),
                                     box(plotOutput("cryptocurrencies"))
                            ),
                            ######tab for the barcharts
                            tabPanel(tabName="closepricecharts",
                                     box(plotOutput("closeprice"),width = 100,height = 100)
                            )
                            ####end
                            
                ))
              
              
              )
      ############tab for the charts here in the close prices#############
      
      ############end of the chartsin closeprice and##################
      )
      ############end of the body#################
    ))
  