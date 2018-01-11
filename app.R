#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#if(!require(gtrendsR)) install.packages("gtrendsR",repos = "http://cran.us.r-project.org")
library(gtrendsR)
library(shiny)
library(prophet)

# Define WebCrawl Variables

googleGeo = "US-TN"
googleTime= "today+5-y"

# narcotics <- read_csv("~/Ubiqum Data Science/Health-Trends-Shiny-Server/narcotics.csv", 
#                       col_names = FALSE)

#test = gtrends(as.character(narcotics[1,]), geo = googleGeo, time = googleTime, gprop = c("web", "news", "images", "froogle", "youtube"),  hl = "en-US")
test = gtrends("fentanyl", geo = googleGeo, time = googleTime, gprop = c("web", "news", "images", "froogle", "youtube"),  hl = "en-US")
# Begin Crawl




# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
   # Application title
   titlePanel("Let's get this Party Started!!!!!!!"),
   tabPanel("Home" , 
            h1("Hello Ruben", align = "center"),
            HTML('<center><img src="http://cultofthepartyparrot.com/assets/sirocco.gif" style ="width="300", height="300"></center>'),
            #HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1512060481/partyparrot_lcjgj2.gif" style ="width="300", height="300"></center>'),
            #  HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907256/DS_logo_rmmtbo.png" style ="width="300", height="300"></center>'),
            h3("Time to make your data party like a parrot!!!!!", align = "center"),
            HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907481/headshot_foglex.png" style ="width="100", height="100"></center>')
            
   ),
   tabPanel("Presentation", 
            
            #---------> Code to Insert a Powerpoint Presentation-----------------------------------------------------------------------------------------------------------------------
            
            tags$iframe(style="height:50vw; width:90vw; scrolling=no", 
                        src="https://onedrive.live.com/embed?cid=D091F528EDB75B0A&resid=D091F528EDB75B0A%2111092&authkey=AJlOeVwrPeQJKDc&em=2")),
   # <iframe src="https://onedrive.live.com/embed?cid=D091F528EDB75B0A&resid=D091F528EDB75B0A%2111092&authkey=AJlOeVwrPeQJKDc&em=2" width="402" height="327" frameborder="0" scrolling="no"></iframe>
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         dateRangeInput("dateSelect", "Select Date Range:",
                        start = "2007-01-01",
                        end   = "2007-01-31"),
         
         
         tags$br(),
         HTML('<center><img src="http://iconbug.com/data/95/256/8696325e0e7407823058632e68fb5970.png" style ="width="75", height="100"></center>'),
         tags$br(),
         tags$br(),
         sliderInput("predict", "Number of Days to Predict:",
                     min = 0, max = 60, step = 1,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("drug"),
         plotOutput("prophetPrediction")
      )
   )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
 #  prophetInput <- reactive({
 #    ds = test$interest_over_time$date
 #    y = test$interest_over_time$hits
 #  forecasting = data.frame(ds, y)  #set up our variables in a data frame
 # predictRange = 100
 #  prophetPredictions = prophet(forecasting)  #This step releases the wizard (generates model)
 #  future = make_future_dataframe(prophetPredictions, periods=predictRange) #Set the number of days (periods) you want to predict
 #  forecast = predict(prophetPredictions, future) # Unleash the wizard on the data dragon (applies model)
 #  plot(prophetPredictions, forecast, ylab = "Relative Interest", xlab = "Date", main = "30 Day Prediction with Prophet")
 #  })
 #  
 #   output$drug <- renderPlot({
 #      # generate bins based on input$bins from ui.R
 #       x    <- faithful[, 2] 
 #       bins <- seq(min(x), max(x), length.out = input$bins + 1)
 #      # 
 #      # # draw the histogram with the specified number of bins
 #      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
 #     plot(test)
 #   })
 #   
 #   output$prophetPrediction =  renderPlot({ 
 #     
 #     prophetInput()
 #     
 #   })
}

# Run the application 
shinyApp(ui = ui, server = server)

