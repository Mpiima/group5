
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(colorspace)
library(gridExtra)
library(memisc)
library(sqldf)
library(tm)
library(RColorBrewer)
library(quanteda)
library(syuzhet)
library(maps)
library(mapdata)
library(forecast)
library(plotrix)


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
        menuItem(strong("TOP 10 ANALYSIS"), tabName="topq"),
        
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
                          uiOutput("dict1")
                  ),
                  
                  
                  #  tab content
                  tabItem(tabName = "corr",
                          
                          uiOutput("corrs")
                          
                  ),
                  #  tab content
                  tabItem(tabName = "topq",
                          
                          uiOutput("tops")
                          
                  ),
                  
                  
                  #  tab content
                  tabItem(tabName = "price",
                          
                          #uiOutput("prices"),
                          uiOutput("prices1")
                          #uiOutput("prices2")
                          
                  ),
                  
                  #  tab content
                  tabItem(tabName = "data",
                          
                          fluidPage(
                            tabBox(width = 200, height = 800,
                                   #tabPanel(strong("About file"),
                                   # box(background ="teal",width = 800, height = 500,verbatimTextOutput("fill"))),
                                   tabPanel(strong("Data"),
                                            box(background ="teal",width = 800, height = 500, verbatimTextOutput("fill"))),
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
  
  datum <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    data1<-read.csv(file1$datapath, header = TRUE)
    data1
    # read.table(file=file$datapath, header= input$header, stringsAsFactors = input$stringAsFactors)
  })
  #this output contains summary of dataset in table format
  output$filedf <- renderTable({
    if(is.null(datum())){return()}
    input$file
  })
  
  
  data <- read.csv("consolidated_coin_data.csv", skip = 4)
  
  output$fill <- renderPrint({
    data[1:10,]
  })
  
  #this output contains structure of dataset in table format
  output$str <- renderPrint({
    if(is.null(data)){return()}
    str(data)
    
  })
  
  #this output contains summary of dataset in table format
  output$sum <- renderPrint({
    if(is.null(data)){return()}
    summary(data)
    
  })
  #this output contains the dataset in table format
  output$table <- renderTable({
    if(is.null(datum())){return()}
    
    head(datum())
  })
  #Analysis in close prices
  ##Close price data
  
  library(reshape2)
  #Change data frame with row=date and col=close.price
  close.raw <- reshape(data[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
  close.raw[,"Close.Currency"] <- NULL
  
  #Change values into numeric and get it as the dataframe style
  close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
  #Change names of cols
  colnames(close) <- sub("Close.", "", colnames(close))
  head(close)
  
  #Change the date column into POSIXct style
  library(lubridate)
  dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y", locale = "eng")
  close$Date<- dates
  deat <- close$Date
  deat
  #Delete the last row since it has no information
  close <- close[-nrow(close),]
  
  dim(close)
  
  
  ##Number of days past after the coin was issued
  
  length.col <- colSums(!is.na(close[,-1]))
  
  #Newest cryptocurrency in this dataset
  sort(length.col)[1]
  
  
  
  #Summary table about how long the cryptocurrencies are. Unit is day.
  options("scipen"=100, "digits"=4)
  table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
  #Percentage
  table2 <- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))/200*100
  
  
  #Correlation analysis
  
  #Take cols with more than 200
  close.180 <- close[,colSums(!is.na(close)) >= 180]
  
  
  ##Correlation between all of cryptocurrencies
  
  corr <- cor(close.180[,-1], use = "pairwise.complete")
  
  library(corrplot)
  output$cor <- renderPlot({
    corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
             title = "Correlation matrix (ordered by hierarchical clustering)",
             mar = c(0,1,2,0))
  })
  
  ##Relation between bitcoin and alt coin prices
  
  #Make correlation matrix between bitcoin and all of alt coins
  output$cor1 <- renderPlot({
    corr.bit <- corr[1:ncol(corr),"bitcoin", drop=FALSE]
    corrplot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.7, mar = c(0,1,2,0))
  })
  
  ###Top 5 positively correlated with bitcoin
  output$cor2 <- renderPrint({
    corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
    data.frame(name=corr.bit.dec.order[2:11,0], cor=corr.bit.dec.order[2:11,1])
  })
  
  ###Top 5 negatively correlated with bitcoin
  output$cor3 <- renderPrint({
    corr.bit.inc.order <- corr.bit[order(corr.bit, decreasing=F),,drop=F]
    data.frame(name=corr.bit.inc.order[1:10,0], cor=corr.bit.inc.order[1:10,1])
  })
  
  output$bag<-renderPlot({
    plot(corr.bit.dec.order[2:11] , type = "o", main = "Currencies Positively correlated with the bitcoin", 
         xlab = "currencies", ylab = "correlation" )
    
  })
  
  output$bag1<-renderPlot({
    plot( corr.bit.inc.order[1:10], type = "o", main = "Currencies Negatively correlated with the bitcoin", 
          xlab = "currencies", ylab = "correlation" )
    
  })
  
  
  
  #output of correlation tab
  output$corrs <- renderUI({
    tabsetPanel( 
      tabPanel("Between all Cryptocurrencies",icon = icon("bar-chart-o"),
               plotOutput("cor",width = 1000,height = 1000)
      ),
      
      tabPanel("Between Top Currencies",icon = icon("table"),
               plotOutput("bat2",width = 800,height = 700)
      ),
      
      tabPanel("Positively with Bitcoin",icon = icon("table"),
               verbatimTextOutput("cor2"),
               box( width = 500, height = 800,
                    plotOutput("bag")
               )
      ),
      
      tabPanel("Negatively with Bitcoins",icon = icon("table"),
               verbatimTextOutput("cor3"),
               box( width = 500, height = 800,
                    plotOutput("bag1")
               ))
      ####end
    )
  })
  
  #Analysis in market cap
  ##Extract the market cap
  
  markcap <- data[c(1,2,8)]
  markcap$Market.Cap[markcap$Market.Cap == "-"] <- NA
  
  #Change data frame with row=date and col=close.price
  markcap.raw <- reshape(markcap, timevar= "Currency", idvar = "Date", direction = "wide")
  markcap.raw[,"Market.Cap.Currency"] <- NULL
  
  #Change values into numeric
  markcap <- sapply(markcap.raw, function(z){as.numeric(gsub(",","", z))})
  #Make data data.frame while deleting the last row because this has no info
  markcap <- data.frame(markcap[-nrow(markcap),])
  #Take cols with more than 200
  markcap <- markcap[,colSums(!is.na(markcap)) >= 200]
  #Change names of cols
  colnames(markcap) <- sub("Market.Cap.", "", colnames(markcap))
  
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
  library(coinmarketcapr)
  
  market_today <- get_marketcap_ticker_all()
  
  library(treemap)
  df <- na.omit(market_today[,c('id','market_cap_usd')])
  df$market_cap_usd <- as.numeric(df$market_cap_usd)
  df$formatted_market_cap <-  paste0(df$id,'\n','$',format(df$market_cap_usd,big.mark = ',',scientific = F, trim = T))
  
  output$map<-renderPlot({
    treemap(df, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
  })
  
  library(treemap)
  df1 <- na.omit(market_today[,c('id','X24h_volume_usd')])
  df1$X24h_volume_usd <- as.numeric(df1$X24h_volume_usd)
  df1$formatted_volume <-  paste0(df1$id,'\n','$',format(df1$X24h_volume_usd,big.mark = ',',scientific = F, trim = T))
  
  output$map1<-renderPlot({
    treemap(df1, index = 'formatted_volume', vSize = 'X24h_volume_usd', title = 'Cryptocurrency Volume', fontsize.labels=c(12, 8), palette='RdYlGn')
  })
  
  library(treemap)
  df2 <- na.omit(market_today[,c('id','price_usd')])
  df2$price_usd <- as.numeric(df2$price_usd)
  df2$formatted_price <-  paste0(df2$id,'\n','$',format(df2$price_usd,big.mark = ',',scientific = F, trim = T))
  
  output$map2<-renderPlot({
    treemap(df2, index = 'formatted_price', vSize = 'price_usd', title = 'Cryptocurrency Market Price', fontsize.labels=c(12, 8), palette='RdYlGn')
  })
  
  
  ##Average market cap
  
  mean.cap <- data.frame(mean.cap=colMeans(markcap, na.rm = T))
  mean.cap.10.name <- rownames(mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F])[1:10]
  mean.cap.10.value <- mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F][1:10,]
  mean.cap.10 <- data.frame(name=mean.cap.10.name, mean.market.cap=mean.cap.10.value)
  mean.cap.10
  output$bar1<-renderPlot({
    barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=2 , cex.names=0.9,
            main="Average market capital in top 10 Cryptocurrencies")
  })
  
  #output of market analysis tab
  output$mart <- renderUI({
    
    tabsetPanel( 
      tabPanel("MarketCapital Structure",icon = icon("table"),
               verbatimTextOutput("table5")
      ),
      
      tabPanel("MarketCapital Plot",icon = icon("table"),
               box( width = 800,height = 800,
                    plotOutput("map"))
      ),
      
      tabPanel("Average MarketCapital",icon = icon("table"),
               box( width = 500,height = 500,
                    tableOutput("table4")
               )),
      
      
      tabPanel("Top Currencies",icon = icon("bar-chart-o"),
               box(width = 600,height = 600,
                   plotOutput("bar1")
               )))
  })
  
  output$pie1 <- renderPlot({
    pie(table2, radius = 1, main ="Summary of how long the cryptocurrencies are on the market")
  })
  
  library(ggplot2)
  #output of analysis of prices tab
  
  output$prices <- renderUI({
    fluidRow(
      box( side="center",background="lime"
           #sliderInput("size", "Point size:", min=0, max=4, value=2, step = 0.2, animate = T ),
           
           #selectInput(inputId = "bar","select price type", c("OpenPrice"=3,"HighPrice"=4,"LowPrice"=5,"ClosePrice"=6), selected = "OpenPrice", multiple = FALSE)
           #selectInput(inputId = "bar1","select price type", c("OpenPrice"=4,"HighPrice"=5,"LowPrice"=6,"ClosePrice"=7), selected = "OpenPrice", multiple = FALSE)
      )
    )})
  
  
  output$bar3 <- renderPlot({
    plot_top_5_currencies()
  })
  
  output$prices1 <- renderUI({
    fluidRow(
      
      
      tabBox( width = 500, height = 500, 
              tabPanel("Analysed in Close price",icon = icon("table"),
                       box(background ="teal",width = 500, height = 500, 
                           p("Plot of Close Price of a given Period"),
                           plotOutput("top")
                       )),
              
              tabPanel("BarGraph",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 600, height = 500, plotOutput("bar3"))),
              tabPanel("Price Map",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 500, height = 500, plotOutput("map2"))),
              tabPanel("Duration",icon = icon("bar-chart-o"), 
                       box(background ="teal",width = 500, height = 500,
                           
                           plotOutput("pie1")))
              
      )
      
    )})
  #download button for the charts
  output$downloadData <- downloadHandler({
    filename = "Models.png"
    content = function(file) 
      if(title=="Bargraph"){
        png(file)
        
        print(myhist)
        dev.off()
        
      }
    else #(input$plot=="piechart")
      
      png(file)
    
    print(pie1) 
    dev.off()
  })
  
  output$prices2 <- renderUI({
    fluidRow(
      box(downloadButton(outputId="downloadData", label = "Download the plot"),background = "olive",width = 3)
    )
    ####end
    
  })
  #output of predict tab
  
  output$dict <- renderPlot({
    predict(price10)
  })
  output$dict1 <- renderUI({
    
    tabsetPanel(
      tabPanel("Prediction", icon = icon("bar-chart-o"),
               box(width = 600, height = 600,plotOutput("dict"))
      )
    )
  })
  
  #Analysis of top 10 Cryptocurrencies
  ##Change close data into time series
  
  library(xts)
  rownames(close) <- close$Date
  close.xts <- as.xts(close)
  
  
  ##Closing price
  
  price10 <- as.xts(close[ ,mean.cap.10.name])
  output$top <- renderPlot({
    plot.xts(price10, main="Close Price for all Currencies")
  })
  
  ##Log-return
  
  library(PerformanceAnalytics)
  ret10.xts <- CalculateReturns(price10, method="log")
  ret10 <- data.frame(ret10.xts)
  
  
  ##Mean
  
  options(digits = 3)
  data.frame(mean.percent = sort(apply(ret10[,1:ncol(ret10)], 2, 
                                       function(x) mean(x, na.rm=TRUE)), decreasing = T))*100
  
  
  ##Variance
  
  options(digits = 3)
  data.frame(variance.percent = sort(apply(ret10[,1:ncol(ret10)], 
                                           2, function(x) sd(x, na.rm=TRUE)), decreasing = T))*100
  
  
  ##Standard deviation
  
  options(digits = 3)
  data.frame(variance.percent = sqrt(sort(apply(ret10[,1:ncol(ret10)], 
                                                2, function(x) sd(x, na.rm=TRUE)), decreasing = T)))*100
  
  
  ##CVar
  
  library(PerformanceAnalytics)
  
  
  ##Log-return chart
  output$top6 <- renderPlot({
    plot.xts(ret10.xts, main="logarithmic return for all Currencies", ylim = c(-3,2))
  })
  output$bat <- renderPlot({
    par(mfrow=c(2,1)); 
    for(i in 1:ncol(ret10)){
      print(plot(ret10.xts[,i], main=colnames(ret10.xts)[i]))
    }
  })
  output$tops <- renderUI({
    
    tabsetPanel(
      
      
      
      tabPanel("Kernel Density",
               box(width = 600, height = 600,
                   strong(h4("Distribution Analysis of each currency")),
                   plotOutput("bat3"))
      ),
      
      tabPanel("Portifolio",
               box(width = 600, height = 600,
                   plotOutput("bat4"))
      ),
      
      tabPanel("Log-Return",icon = icon("bar-chart-o"), 
               box(width = 500, height = 1000,plotOutput("top6"),
                   tags$br(),
                   strong(h4("Logarithmic Return of each currency")),
                   plotOutput("bat"))),
      
      tabPanel("Volatility",icon = icon("table"),
               box(width = 500, height = 1000,plotOutput("top8"),
                   tags$br(),
                   strong(h4("Volatility Chart of each currency")),
                   plotOutput("bat1"))),
      tabPanel("Volume",
               box(width = 600, height = 600,
                   plotOutput("map1"))
      )
      
      
    )
    
    
  })
  
  ##Volatility chart
  
  library(TTR)
  
  vol30 <- xts(apply(ret10.xts, 2, runSD,n=30), index(ret10.xts))*sqrt(252)
  output$top8 <- renderPlot({
    plot.xts(vol30, main = "Volatility Chart for all Currencies")
  })
  
  output$bat1 <- renderPlot({
    par(mfrow=c(2,1))
    for(i in 1:ncol(vol30)){
      print(plot(vol30[,i], main=colnames(vol30)[i]))
    }
  })
  
  ##Correlation chart
  output$bat2 <- renderPlot({
    chart.Correlation(ret10)
  })
  
  #Distribution analysis of each cryptocurrency
  ##Kernel Density
  
  output$bat3 <- renderPlot({
    par(mfrow=c(2,1)); 
    for(i in 1:ncol(ret10)){
      plot(density(ret10[,i], na.rm = T), main=colnames(ret10)[i])
    }
  })
  
  #We might need to compute non-parametric distribution...
  
  #Portfolio selection
  
  
  ##Deterministic optimisation method
  
  ret10.CF <- na.omit(ret10)
  mean_vect <- colMeans(ret10.CF)
  cov_mat <- cov(ret10.CF)
  sd_vect <- sqrt(diag(cov_mat))
  
  M <- length(mean_vect)
  
  library(quadprog)
  Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
  muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
  sdP <- muP # set up storage for std dev's of portfolio returns
  weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
  for (i in 1:length(muP)){
    bvec <- c(1, muP[i]) # constraint vector
    result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2) 
    sdP[i] <- sqrt(2*result$value) 
    weights[i,] <- result$solution
  }
  
  output$bat4 <- renderPlot({
    plot(100*sdP, 100*muP, type="l", xlim=c(0,250), ylim=c(0,35), 
         main="Efficient Frontier", ylab="Expected Return(%)", xlab="Standard deviation(%)")
  })
  
  
  ind.min <- which.min(sdP)
  options(digits = 3)
  
  #Expected return
  muP[ind.min]
  
  #Expected standard deviation
  sdP[ind.min]
  
  #Proportions
  weight <- data.frame(proportion = weights[ind.min,], row.names = colnames(ret10.CF))
  weight
  
}

# Run the application 
shinyApp(ui = ui, server = server)
