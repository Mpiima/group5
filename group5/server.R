library(shiny)
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
    d <- 
    
    barplot(data$Close,data$Date, ylab = "close price", xlab = "date")
  })
###############end in closeprice analysis and start in correlation analsis#############
  }