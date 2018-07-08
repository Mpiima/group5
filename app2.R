library(shiny)

library(shinydashboard)

ui <- dashboardPage(
                     dashboardHeader(title = em("CRYPTOCURRENCY ANALYSIS PROJECT") ,titleWidth = 500
                                     ),
                     
                     dashboardSidebar(
                       
                       #cryptocurrency image
                       tags$img(src="image2.jpg", width="100"),
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
                         
                         menuItem(strong("ANALYSIS IN CLOSE PRICE"), tabName = "close"),
                         menuSubItem("barplot",tabName = "closepricecharts",icon = icon("bar-chart-o")),
                         menuSubItem("Linegraph", tabName = "closepricelinegraph", icon = icon("line-chart")),
                         menuSubItem("scatter",tabName = "closepricescatter",icon = icon("line-chart")),
                         #######end of analysis in closeprice and start correlation analysis################
                         
                         menuItem(strong("CORRELATION ANALYSIS"), tabName="corr"),
                         menuSubItem("btn all cryptocurrencies", tabName = "all", icon = "bar-chart-o"),
                         menuSubItem(" Top5+correlated-with-bitcoin", tabName = "+bitcoins", icon = icon("table")),
                         menuSubItem(" Top5-correlated-with-bitcoin", tabName = "-bitcoins", icon = icon("table")),
                         
                         #####end of correlation analysis and begin in analysis in market cap
                         menuItem(strong("MARKET CAP ANALYSIS"), tabName = "market"),
                         menuSubItem("average market cap", tabName = "mrktcap", icon = icon("bar-chart-o")),
                         
                         
                         ######end in mkt cap and begin top 10 analysis###########
                         
                         
                         menuItem(strong("TOP 10 ANALYSIS"),tabName = "top"),
                         ######end of top 10 and begin predict ######
                         menuSubItem("mean", tabName = "mean", icon = icon("table")),
                         menuSubItem(" variance", tabName = "variance", icon = icon("table")),
                         menuSubItem("log-return", tabName = "log", icon = icon("table")),
                         
                         
                         menuItem(strong("predict"),tabName = "predict"),
                         #######end predict and then project description
                         menuItem("Project Description")
                       )),
                     dashboardBody(
                       # Boxes need to be put in a row (or column)
                       tabItems(
                         # First tab content
                         tabItem(tabName = "close",
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
                                               
                                   )),
                                 fluidRow(
                                   box(plotOutput("plot1", height = 250)),
                                   
                                   box(
                                    
                                     
                                   )
                                 )
                         ),
                         #  tab content
                         tabItem(tabName = "corr",
                                 fluidRow(
                                   box(plotOutput("plot1", height = 250)),
                                   
                                   box(
                                     
                                     
                                   )
                                 )
                         ),
                         #  tab content
                         tabItem(tabName = "market",
                                 fluidRow(
                                   box(plotOutput("plot1", height = 250)),
                                   
                                   box(
                                     
                                     
                                   )
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
                                 h2("Widgets tab content")
                         ),
                         # tab content
                         tabItem(tabName ="data",h1("Cryptocurrency Data"),
                                
    
                                   tabsetPanel(id="tabs",
                                               tabPanel(value="panel1", "uploaded_data",icon = icon("table"), 
                                                        tableOutput("contents"),dataTableOutput("mytable2")))
                                 
                                 ),
                         # tab content
                         tabItem(tabName = "Upload",
                                 
                                 fluidRow(
                                   box(
                                     fileInput("file","upload the file"),
                                     radioButtons("sep","Seperator", choices = c(Comma=',',Period='.',Tilde="~",minus="-")),
                                     checkboxInput("header","Header?")
                                     # 
                                   ),
                                   box(tableOutput("input_file"))
                                 )
                         )
                       ))
)

#options(shiny.maxRequestSize=50*1024^2)
server <- function(input,output,session){
  #implementing file input##############
  myData <- reactive({
    file1<-input$file
    if(is.null(file1)){return(NULL)}
    
    dataB<-read.csv(file1$datapath, header = TRUE)
    dataB
  })
  ###########end and start of output uploaded data#######
  output$contents <- renderTable(
    head(myData)
  )
  ####eerror ########
  
  # reading the two tables in the data frame###############
  output$mytable1<-renderDataTable({read[,1:6]})
  
  ##############end of output and start of analysis in close price barplot####################################################
  
  output$closeprice <- renderPlot({
     barplot(data$Close,data$Date, ylab = "close price", xlab = "date")
  })
  ###############end in closeprice analysis and start in correlation analsis#############
}

shinyApp(ui =ui , server = server)

