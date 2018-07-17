library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tm)
library(plotrix)
options(shiny.maxRequestSize=50*1024^2)
shinyServer(
 
  function(input,output,session){ #data processing implementation
    myData <- reactive({
      file1<-input$file
      if(is.null(file))return(NULL)
      
      data<-read.csv(file1$datapath, header = TRUE)
      
    })
    
    output$contents <- renderTable(
      myData()
    )
    read<-function(){
      if(is.null(input$file))
        return(NULL)
      library(ggplot2)
      
      infile<-input$file
      data<-read.csv(infile$datapath)
      
      df<-data.frame(
        Currency=data$Currency,
        Date=data$Date,
        Open=data$Open,
        High=data$High,
        Low=data$Low,
        close =data$close,
        Volume=data$Volume,
        MarketCap=data$MarketCap
       
      )
      df 
    }
    output$mytable1<-renderDataTable({read()[1:8]}) # reading the one table in the data frame and end data imple..
    
    #.....................................IMPLEMENTATION IN CLOSE PRICE...........................................#
    library(reshape2)
    #Change data frame with row=date and col=close.price
    close.raw <- reshape(data[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    #Change values into numeric and get it as the dataframe style
    close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    #Change names of cols
    colnames(close) <- sub("close.", "", colnames(close))
    #Change the date column into POSIXct style
    library(lubridate)
    dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y", locale = "eng")
    close$Date<- dates
    #Delete the last row since it has no information
    close <- close[-nrow(close),]
    x<-dim(close)
    
    length.col <- colSums(!is.na(close[,-1,2]))
    
    #Newest cryptocurrency in this dataset
    sort(length.col)[1]
    #Summary table about how long the cryptocurrencies are. Unit is day.
    options("scipen"=100, "digits"=4)
    table1<- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
    #Percentage
    table2<-table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))/200*100
    #CORRELATION analysis
    
    #Take cols with more than 200
    #correlation between all cryptocurrencies
    close.1440 <- close[,colSums(!is.na(close)) >= 1440]
    close.1440
    corr <- cor(close.1440[,-1], use = "pairwise.complete")
   library(corrplot)
    corr.bit <- corr[1:ncol(corr)]
   corr.bit
   #corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
   #corr.bit.inc.order <- corr.bit[order(corr.bit, decreasing=F),,drop=F]
   
    #correlation btn btc and alt
    #corrplot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.7, mar = c(0,1,2,0))
    
    #,,,,,,,,,,,,,,,,,,,,,,,,,MARKET CAP,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
   
    #.....................................END IMPLE...............................................................#
    
    markcap <- data[c(1,2,8)]
    markcap$Market.Cap[markcap$Market.Cap == "-"] 
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
    output$his1 <- renderPlot({
      corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
               title = "Correlation matrix (ordered by hierarchical clustering)",
               mar = c(0,1,2,0))
    })#bitcoins and alt 
    output$his2 <- renderPlot({
      plot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.8, mar = c(0,1,2,0))
    })
    output$table5<-renderTable({
      table(markcap)
    })
    output$table4 <-renderTable({#data for mean of market cap
      mean.cap.1<-data.table(mean.cap.10)
    })
    
    output$bar1<-renderPlot({
     barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=2 , cex.names=0.9,
             main="Average market capital in top 10 Cryptocurrencies")
    })
    output$data5<-renderDataTable({
      data
    })
   
  }
)