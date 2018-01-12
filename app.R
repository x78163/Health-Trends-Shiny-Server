## app.R ##
library(shinydashboard)
library(gtrendsR)
library(shiny)
library(prophet)

# Define WebCrawl Variables

googleGeo = "US-TN"
googleTime= "today+5-y"  #pull trends from last five years
# googleTime= "now 1-H" #pull last hour of trends
# googleTime= "now 1-d"  # pull last day of trends
# googleTime= "today 1-m"  # pull last month of thrends
# googleTime= "2010-01-01 2010-04-03" #Pull trends from date range
#googleTime= "all"  #pull all trends

# narcotics <- read_csv("~/Ubiqum Data Science/Health-Trends-Shiny-Server/narcotics.csv", 
#                       col_names = FALSE)

#test = gtrends(c("bitcoin", "ethereum", "litecoin", "ripple", "monero"), geo = googleGeo, time = googleTime, gprop = c("web", "news", "images", "froogle", "youtube"),  hl = "en-US")
test = gtrends("fentanyl", geo = googleGeo, time = googleTime, gprop = c("web", "news", "images", "froogle", "youtube"),  hl = "en-US")
# Begin Crawl

ui <- dashboardPage(
  dashboardHeader(title = "Community Health Dashboard", dropdownMenu(type = "messages",
                                                          messageItem(
                                                            from = "Sales Dept",
                                                            message = "Sales are steady this month."
                                                          ),
                                                          messageItem(
                                                            from = "New User",
                                                            message = "How do I register?",
                                                            icon = icon("question"),
                                                            time = "13:45"
                                                          ),
                                                          messageItem(
                                                            from = "Support",
                                                            message = "The new server is ready.",
                                                            icon = icon("life-ring"),
                                                            time = "2014-12-01"
                                                          )
  ),
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "5 searches for Quillen Today",
                 icon("users")
               ),
               notificationItem(
                 text = "12 items delivered",
                 icon("truck"),
                 status = "success"
               ),
               notificationItem(
                 text = "Server load at 86%",
                 icon = icon("exclamation-triangle"),
                 status = "warning"
               )
  ),
  
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, color = "green",
                        "Behavioral"
               ),
               taskItem(value = 17, color = "aqua",
                        "Orthopedics"
               ),
               taskItem(value = 30, color = "yellow",
                        "Oncology"
               ),
               taskItem(value = 10, color = "yellow",
                        "Communicable Disease"
               ),
               taskItem(value = 75, color = "yellow",
                        "Urology"
               ),
               taskItem(value = 80, color = "red",
                        "Cardio"
               )
  )
  
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    tags$br(),
    tags$br(),
    tags$br(),
    title = "Controls",
    HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510908400/Calendar_f5yruq.png" style ="width="60", height="20"></center>'),
    tags$br(),
    dateRangeInput("dateSelect", "Select Date Range:",
                   start = Sys.Date()-30,
                   end   = Sys.Date()),
    
    
    tags$br(),
    HTML('<center><img src="http://iconbug.com/data/95/256/8696325e0e7407823058632e68fb5970.png" style ="width="30", height="40"></center>'),
    tags$br(),
   
    sliderInput("predict", "Number of Days to Predict:",
                min = 0, max = 60, step = 1,
                value = 30)
    
    
  )),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("drug", height = 250)),
                
                box(plotOutput("prophetPrediction", height = 250))
                

                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  prophetInput <- reactive({
    ds = test$interest_over_time$date
    y = test$interest_over_time$hits
    forecasting = data.frame(ds, y)  #set up our variables in a data frame
    predictRange = 30
    predictRange = as.numeric(input$predict)
    prophetPredictions = prophet(forecasting)  #This step releases the wizard (generates model)
    future = make_future_dataframe(prophetPredictions, periods=predictRange) #Set the number of days (periods) you want to predict
    forecast = predict(prophetPredictions, future) # Unleash the wizard on the data dragon (applies model)
    forecast[1,]
    
    plotted = 0
    plotted$ds = as.data.frame(forecast$ds)
    plotted$predict = forecast$yhat
    plotted =  as.data.frame(plotted)
    plotted = tail(plotted, n=60)
    # plot(plotted$forecast.ds, plotted$predict)
    futureValue = plotted$predict[nrow(plotted)]
    presentValue = plotted$predict[(nrow(plotted)-30)]
    
    if(futureValue>presentValue)
    {
      print("trending up")
    }
    if(presentValue>futureValue)
    {
      print("trending down")
      futureValue-presentValue
    }
    
    plot(prophetPredictions, forecast)#+aes(xintercept=as.numeric(as.POSIXct("2017-01-01")))#+(ylab = "Relative Interest"+ xlab = "Date" + main = "30 Day Prediction with Prophet")#,coord_cartesian(ylim=c(0, 0.1)))
    
    
  })
  
  output$drug <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    plot(test)
  })
  
  output$prophetPrediction =  renderPlot({
    HTML('<center><img src="https://oxycodoneinformation.files.wordpress.com/2015/04/oxycodone.png" style ="width="300", height="300"></center>')
    prophetInput()
    
  })
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)