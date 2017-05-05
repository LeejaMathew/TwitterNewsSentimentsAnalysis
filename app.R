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
library(maps)
library(mapproj)
source("/Users/jc/Downloads/helpers.R")
source("/Users/jc/Downloads/SentimentHelpers.R")

BillOreilly<- gettxt("/Users/jc/Desktop/TwitterR/franceKy23.json")
counties <- readRDS("/Users/jc/Downloads/counties.rds")
#BillOreilly <- readRDS("/Users/jc/Desktop/TwitterR/em3t.rds")
Samsung <- readRDS("/Users/jc/Desktop/TwitterR/emotion.rds")
Elections <- readRDS("/Users/jc/Desktop/TwitterR/emot.rds")
Starbucks <- readRDS("/Users/jc/Desktop/TwitterR/emot.rds")
 
#percent_map(counties$white, "darkblue", "% White")

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
       # Copy the line below to make a text input box
       textInput("text", label = h3("Text input"), value = "Enter text"),
       
       hr(),# Copy the line below to make an action button
       actionButton("action", label = "Action"),
       
       hr(),# You can access the value of the widget with input$action, e.g.
    
   
       
       selectInput("var2", 
                   label = "Choose a variable to display",
                   choices = c("BillOreilly","Samsung","France Elections","Starbucks"),
                   selected = "BillOreilly")
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
  
  # You can access the value of the widget with input$text, e.g.
  output$value <- renderPrint({ 
    input$text 
    })
  # You can access the value of the widget with input$action, e.g.
  output$value <- renderPrint({ 
    input$action 
    })
  
  datasetInput <- reactive({
    switch(input$var2,
           "BillOreilly" = BillOreilly,
           "Samsung" = Samsung,
           "France Elections" = Elections,
           "Starbucks" = Starbucks)
  })
  
   output$distPlot <- renderPlot({
     
     
     dataset <- datasetInput()     
     dataset
       ggplot(dataset, aes(x=emotion)) +
         geom_bar(aes(y=..count.., fill=emotion)) +
         scale_fill_brewer(palette='Dark2') +
         labs(x='emotion categories', y='number of tweets') +
         ggtitle('Sentiment Analysis of Tweets\n(classification by emotion)') +
         theme(plot.title = element_text(size=12, face='bold'))

   })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

