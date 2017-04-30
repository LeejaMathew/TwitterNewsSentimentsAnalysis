#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
counties <- readRDS("/Users/jc/Downloads/counties.rds")
head(counties)
library(maps)
library(mapproj)
source("/Users/jc/Downloads/helpers.R")
counties <- readRDS("/Users/jc/Downloads/counties.rds")
percent_map(counties$white, "darkblue", "% White")

emotionRes <- readRDS("/Users/jc/Downloads/EmotionResults.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sentiment Analysis"),
   
   # Sidebar with a slider input for number of bins 

   sidebarLayout(
     sidebarPanel(
       helpText("View sentiment over time"),
       
       selectInput("var", 
                   label = "Choose a variable to display",
                   choices = c("Samsung", "Starbucks",
                               "Bill Oreilly", "France Elections"),
                   selected = "Starbucks"),
       
       sliderInput("range", 
                   label = "Range of interest:",
                   min = 0, max = 100, value = c(0, 100))
     ),
     
     mainPanel(plotOutput("map")
     )
   ),
   sidebarLayout(
     sidebarPanel(
       helpText("View sentiment over time"),
       
       selectInput("var", 
                   label = "Choose a variable to display",
                   choices = c("Samsung", "Starbucks",
                               "Bill Oreilly", "France Elections"),
                   selected = "Starbucks"),
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
     ),
     mainPanel(
       plotOutput("distPlot")
     )
   )
   #NEW SIDEBAR
      
      # Show a plot of the generated distribution
   
     
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$map <- renderPlot({
    
    data <- switch(input$var, 
                   "Samsung" = counties$white,
                   "Starbucks" = counties$black,
                   "Bill Oreilly" = counties$hispanic,
                   "France Elections" = counties$asian)
    
    color <- switch(input$var, 
                    "Samsung" = "darkgreen",
                    "Starbucks" = "black",
                    "Bill Oreilly" = "darkorange",
                    "France Elections" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Samsung" = "% White",
                     "Starbucks" = "% Black",
                     "Bill Oreilly" = "% Hispanic",
                     "France Elections" = "% Asian")
    
    percent_map(var = data, 
                color = color, 
                legend.title = legend, 
                max = input$range[2], 
                min = input$range[1])
  })
  
  
  
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   
   #NEW SIDEBAR
   
    
}


# Run the application 
shinyApp(ui = ui, server = server)

