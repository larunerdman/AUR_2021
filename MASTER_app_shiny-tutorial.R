#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(RColorBrewer)

my_colours <- brewer.pal(3, "Greens")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
         plotOutput("distPlot"),
         plotlyOutput("distPlot2"),
         
         # hr(),
         # h4("My data:"),
         DT::dataTableOutput("my_df")
      )
      
      
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Base R:
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # ggplot:
    # draw the histogram with the specified number of bins
    # ggplot(faithful, aes(x = waiting)) +
    #   geom_histogram( bins = input$bins, color = "mediumseagreen", fill="darkseagreen")
    
  })
  
  output$distPlot2 <- renderPlotly({
    # ggplot:
    # draw the histogram with the specified number of bins
    # use the colour palette you just generated above for 
    # the color and fill fields

    ggp <- ggplot(faithful, aes(x = waiting)) +
      geom_histogram(bins = input$bins, color = my_colours[1], fill= my_colours[2])
    
    plotly::ggplotly(ggp)
  })
  
  output$my_df <-  DT::renderDT(faithful)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

