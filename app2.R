


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
      #tags$img(src='bitcoin.jpg', width="100"),
      
      
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
        
        
        menuItem(strong("PREDICT"),tabName = "predict")
        #######end predict 
        
        
      )),
    
    dashboardBody(
      
      fluidPage(theme = "sty.css",
                
                tabItems(
                  
                  #defining data output system
                  #  tab content
                  tabItem(tabName = "des",
                          
                          fluidPage(
                            box(width = 800, height = 1200, #background = "lime",
                                h4( strong("WELCOME TO WORLD OF CRYPTOCURRENCIES")),
                                hr(),h2("About our Data set"),
                                p("Recent growing interest in cryptocurrencies, specifically as a speculative investment vehicle, has sparked global conversation over the past few years. 
                                  Although this data is available across various sites, there is a lack of understanding as to what is driving the exponential rise of many individual currencies. 
                                  This data set is intended to be a starting point for a detailed analysis into what is driving price action, and what can be done to predict future movement."),
                                hr(),
                                h2("Contents"),
                                p("Consolidated financial information for the top 736 cryptocurrencies by marketcap. Attributes include:
                                  
                                  Currency name (e.g. bitcoin)
                                  Date,
                                  Open,
                                  High,
                                  Low,
                                  Close,
                                  Volume,
                                  Market Capital
                                  "),
                                p("Questions to be answered:
                                  
                                  What is the correlation between bitcoin and alt coin prices?
                                  What is the average age of the top 10 coins by market capital?
                                  What day of the week is best to buy/sell?
                                  Which coins in the top two hundred are less than 6 months old?
                                  Which currencies are the most volatile? "),
                                
                                h3("MEMBERS AND THERE PROFILES"),
                                fluidRow(
                                  box(side = "left",
                                      h3("Name: MPIIMA COLINS"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:collinsmpiima@gmail.com"),h6("Regno.16/U/7120/ps")
                                  ),
                                  box(side = "right",
                                      h3("Name: GUMA EVAN"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:gumaivan@gmail.com"),h6("Regno.16/U/7120/ps")
                                  ),
                                  
                                  box(side = "left",
                                      h3("Name: NATWIJUKIRE CRISPUS"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("Email:natwijukirecrispus@gmail.com"),h6("Regno.16/U/7120/ps")
                                  ),
                                  box(side = "right",
                                      h3("Name: NYANZI SAMUEL"),tags$img(src="one.jpg", width=100,height=100), h6("contact: 0786950567"),h6("nyanzisamuel.com"),h6("Regno.16/U/7120/ps")
                                  ))
                                
                                )) 
                          
                          
                          
                                ),
                  
                  #  tab content
                  tabItem(tabName = "market",
                          
                          uiOutput("mart")
                  ),
                  
                  
                  
                  #  tab content
                  tabItem(tabName = "predict",
                          uiOutput("dict")
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
                
                
                
                
      )))

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
  
  #.....................................IMPLEMENTATION IN CLOSE PRICE...........................................#
  library(reshape2)
  #Change data frame with row=date and col=close.price
  close1 <- reactive({
    close.raw <- reshape(data()[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    #Change values into numeric and get it as the dataframe style
    close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
  })
  #Change names of cols
  colnames(close) <- sub("close.", "", colnames(close))
  #Change the date column into POSIXct style
  # library(lubridate)
  #dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y", locale = "eng")
  #close$Date<- dates
  #Delete the last row since it has no information
  close <- close[-nrow(close),]
  x<-dim(close)
  
  length.col <- colSums(!is.na(close[,-1,2]))
  
  #Newest cryptocurrency in this dataset
  sort(length.col)[1]
  #Summary table about how long the cryptocurrencies are. Unit is day.
  options("scipen"=100, "digits"=4)
  table1 <- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
  #Percentage
  table2 <- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))/200*100
  #CORRELATION analysis
  
  #Take cols with more than 200
  #correlation between all cryptocurrencies
  close.1440 <- close[,colSums(!is.na(close)) >= 1440]
  #close.1440
  corr <- cor(close.1440[,-1], use = "pairwise.complete")
  
  library(corrplot)
  #Make correlation matrix between bitcoin and all of alt coins
  
  output$co3 <- renderPlot({
    corr.bit <- corr[1:ncol(corr),"BTC", drop=FALSE]
    corrplot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.7, mar = c(0,1,2,0))
  })
  
  ###Top 5 positively correlated with bitcoin
  output$co4 <- renderPrint({
    corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
    cof <- data.frame(name=corr.bit.dec.order[1:10,0], Correlation=corr.bit.dec.order[1:10,1])
    cof
  })
  
  ###Top 5 negatively correlated with bitcoin
  
  output$co5 <- renderPrint({
    corr.bit.inc.order <- corr.bit[order(corr.bit, decreasing=F),,drop=F]
    cof1 <- data.frame(name=corr.bit.inc.order[1:10,0], Correlation=corr.bit.inc.order[1:10,1])
    cof1
  })
  
  #,,,,,,,,,,,,,,,,,,,,,,,,,MARKET CAP,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
  
  #.....................................END IMPLE...............................................................#
  
  mark <- reactive({  
    markcap <- data()[c(1,2,8)]
    markcap$Market.Cap[markcap$Market.Cap == "-"] 
    #Change data frame with row=date and col=close.price
    markcap.raw <- reshape(markcap, timevar= "Currency", idvar = "Date", direction = "wide")
    markcap.raw[,"Market.Cap.Currency"] <- NULL
    #Change values into numeric
    markcap <- sapply(markcap.raw, function(z){as.numeric(gsub(",","", z))})
  })
  #Make data data.frame while deleting the last row because this has no info
  markcap <- data.frame(markcap[-nrow(markcap),])
  #Take cols with more than 200
  markcap <- markcap[,colSums(!is.na(markcap)) >= 200]
  #Change names of cols
  colnames(markcap) <- sub("Market.Cap.", "", colnames(markcap))
  #average market cap
  mean.cap <- data.frame(mean.cap=colMeans(markcap, na.rm = T))
  str(mean.cap)
  mean.cap.10.name <- rownames(mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F])[1:10]
  mean.cap.10.value <- mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F][1:10,]
  mean.cap.10 <- data.frame(name=mean.cap.10.name, mean.market.cap=mean.cap.10.value)
  str(mean.cap.10)
  # barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=2 , cex.names=0.9,
  #   main="Average market capital in top 10 Cryptocurrencies")
  
  #*****************************OUTPUTS*********************************************************************
  
  output$statement<-renderText({
    paste("there are ", x[1], "rows", " and ", x[2], "cryptocurrencies" )
  })
  output$crypto <- renderText({
    paste("newest crypto is ", sort(length.col)[1])
  })
  output$dayunit<-renderTable({
    table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
  })
  output$per <- renderTable({
    table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))/200*100
  })
  
  output$pie1 <- renderPlot({
    pie(table2)
  })
  #correlation btn all cryptocurrencies
  output$data2<-renderDataTable({
    table(corr.bit)
  })
  output$co1 <- renderPlot({
    corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
             title = "Correlation matrix (ordered by hierarchical clustering)",
             mar = c(0,1,2,0))
  })#bitcoins and alt 
  output$co2 <- renderPlot({
    plot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.8, mar = c(0,1,2,0))
  })
  output$table5<-renderPrint({
    str(markcap)
    
  })
  output$table4 <-renderTable({#data for mean of market cap
    mean.cap.1 <- data.table(mean.cap.10)
  })
  
  output$bar1<-renderPlot({
    barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=2 , cex.names=1.2, col = "blue",
            main="Average market capital in top 10 Cryptocurrencies")
  })
  
  #output of analysis of prices tab
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
              tabPanel("PieChart",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 400, height = 400, plotOutput("pie1")))
              
      )
      
    )})
  #download button for the charts
  output$downloadData <- downloadHandler({
    filename = "Models.png"
    content = function(file) 
      if(input$plot=="Bargraph"){
        png(file)
        bar<-ggplot(data(),type="l", aes(x=get((input$variablex)),y=get(input$variabley) ))+
          xlab(input$variablex)+
          ylab(input$variabley)+
          geom_bar(stat="identity",position ="dodge",color ="blue")
        print(bar)
        dev.off()
        
      }
    else #(input$plot=="piechart")
      
      png(file)
    pie<-pie(aggregate(formula(paste0(".~",input$variabley)), data(), sum)[[input$variablex]], labels = aggregate(formula(paste0(".~",input$variabley)), data(), sum)[[input$variabley]])
    print(pie) 
    dev.off()
  })
  
  output$prices2 <- renderUI({
    fluidRow(
      box(downloadButton(outputId="downloadData", label = "Download the plot"),background = "olive",width = 3)
    )
    ####end
    
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
  
  #output of correlation tab
  output$corrs <- renderUI({
    tabsetPanel( 
      tabPanel("btn all cryptocurrencies",icon = icon("bar-chart-o"),
               plotOutput("co1",width = 1000,height = 1000)
      ),
      
      tabPanel("Bitcoin and others",icon = icon("table"),
               plotOutput("co3",width = 1000,height = 300)
      ),
      
      tabPanel("+bitcoins",icon = icon("table"),
               box( width = 500, height = 500,
                    verbatimTextOutput("co4"))
      ),
      
      tabPanel("-bitcoins",icon = icon("table"),
               box( width = 500, height = 500,
                    verbatimTextOutput("co5")
               ))
      ####end
    )
  })
  
  #output of market analysis tab
  output$mart <- renderUI({
    
    tabsetPanel( 
      tabPanel("MarketCapital",icon = icon("table"),
               verbatimTextOutput("table5")
      ),
      
      tabPanel("Average MarketCapital",icon = icon("table"),
               box( width = 500,height = 500,
                    tableOutput("table4")
               )),
      
      
      tabPanel("Top Currencies",icon = icon("bar-chart-o"),
               box(width = 600,height = 500,
                   plotOutput("bar1")
               )))
  })
  
  #output of predict tab
  output$dict <- renderUI({
    
    tabsetPanel(
      tabPanel("Prediction", icon = icon("bar-chart-o"))
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)