#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Let's get this Party Started!!!!"),
   tabPanel("Home" , 
            h1("Ubiqum Data Science Consultants", align = "center"),
            HTML('<center><img src="http://cultofthepartyparrot.com/assets/sirocco.gif" style ="width="300", height="300"></center>'),
            #HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1512060481/partyparrot_lcjgj2.gif" style ="width="300", height="300"></center>'),
            #  HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907256/DS_logo_rmmtbo.png" style ="width="300", height="300"></center>'),
            h3("Time to make your data party like a parrot!!!!!", align = "center"),
            HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907481/headshot_foglex.png" style ="width="100", height="100"></center>')
            
   ),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

