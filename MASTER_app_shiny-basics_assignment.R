#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(DT)


#### Integrated Assignment Outline ####
####
#### 1. Summary stats of Example data 3, as a data table
#### 2. Show data table at on another tab of the page, raw output, add DT formatting
#### 3. Fit regression model, show the verbatim text output of the model
####
#########


#### 1.1 load data

## NOTES: ways to save and read RDS files into R: ##
# saveRDS(genes_zbmi, "./workshop_example/data/genes_zbmi.RDS")
# genes_zbmi <- readRDS(file = "./workshop_example/data/genes_zbmi.RDS")
# genes_zbmi <- readRDS(file = ".data/genes_zbmi.RDS")
# readRDS("data/genes_zbmi.RDS")
##
genes_zbmi <- readRDS("data/genes_zbmi.rds")


# Define UI for application that draws a histogram
ui <- fluidPage( 
  # shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cosmo"),
  title = "for science", # Passed to the web browser as the title of the whole site
  
  # Sidebar with information on the currently displayed model 
  sidebarLayout(
    sidebarPanel(
      h4("Variable selection"),
      br(),
      h5("Model equation:"),
      verbatimTextOutput("lm_formula"),
      br(),
      
      h5("Customize regression model:"),
      br(),
      fluidRow(
        column(width = 6 , offset = 0,
               selectizeInput("response", 
                              label = "Select a response variable", 
                              choices = colnames(genes_zbmi), 
                              selected = colnames(genes_zbmi)[1], 
                              multiple = FALSE,
                              width = '100%')),
        
        column(width = 6, offset = 0,
               selectizeInput("predictor", 
                              label = "Select a predictor", 
                              choices = colnames(genes_zbmi), 
                              selected = colnames(genes_zbmi)[1], 
                              multiple = FALSE,
                              width = '100%'))
        
      ),
      
      fluidRow( 
        column(width = 12,
               verbatimTextOutput("lm_summary"))
      )
      
    ),
    
    mainPanel(
      
      # Page title (titlePanel gives it nice formatting)
      titlePanel("Explorations with regression!"),
      
      h3("Predictions"), # HTML tag for type 3 header 
                         # (there are 5 avaiilable,  varying in size. )
      
      
      # main plot plotly wrapper for ggplot objects
      plotlyOutput("lm_plot"),
      
      # horizontal line break 
      hr(),
      
      h3("Diagnostics"),
      
      # base R plot output
      plotOutput("diagnostics"),
      
      hr(),
      h3("Raw data"),
      
      # DT-formated datatable object
      DT::dataTableOutput("lm_df")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Define a linear model object in a reactive context
  my_lm <- reactive({
    lm(genes_zbmi[[input$response]] ~ 
         genes_zbmi[[input$predictor]])
  })

  # Call the my_lm object in a reactive way (note the parentheses)
  output$lm_summary <- renderPrint(summary(my_lm()))
  ## Alternatively, here's the model fit explicitly in this function:
  # (lm(genes_zbmi[[input$response]] ~ 
  #       genes_zbmi[[input$predictor]])))
  
  
  output$lm_df <- renderDT(genes_zbmi)
 
  output$lm_formula <- renderText({
    c(input$response, "~", input$predictor)
  })
  
  
  output$lm_plot <- renderPlotly({
    
    # because our input$predictor and input$response are strings,
    # we use the double square bracket syntax to access the data
    # that our user is requesting
    # NOTE: .data refers to the data object you are passing to
    # ggplot in the function 
    plotly::ggplotly(
      ggplot(genes_zbmi, aes(x=.data[[input$predictor]], y=.data[[input$response]])) +
        geom_point(shape=1, color ="darkblue") +    # Use hollow circles
        geom_smooth(method=lm) +   # Add linear regression line
        ylab(input$response) +
        xlab(input$predictor)
    )
    
  })
  
  output$diagnostics <- renderPlot({
    par(mfrow=c(2,2))
    
    plot(lm(genes_zbmi[[input$response]] ~ genes_zbmi[[input$predictor]]),
         which = c(1:4))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

