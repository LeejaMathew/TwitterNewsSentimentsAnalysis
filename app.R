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
library("RgoogleMaps")
source("/Users/jc/Downloads/helpers.R")
source("/Users/jc/Downloads/SentimentHelpers.R")
source("/Users/jc/Downloads/stream.R")
source("/Users/jc/Projects/sourceFiles.R")
#BillOreilly<- gettxt("/Users/jc/Desktop/TwitterR/franceKy23.json")
BillOreilly <- readRDS("/Users/jc/Projects/TWSentimentsDS/BillOreillyMapResults.rds")
Samsung <- readRDS("/Users/jc/Projects/TWSentimentsDS/FrenchElectionMapResults.rds")
Elections <- readRDS("/Users/jc/Projects/TWSentimentsDS/FrenchElectionMapResults.rds")
Starbucks <- readRDS("/Users/jc/Projects/TWSentimentsDS/StarbucksMapResults.rds")
 
#percent_map(counties$white, "darkblue", "% White")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sentiment Analysis"),
   
   # Sidebar with a slider input for number of bins 

   sidebarLayout(
     sidebarPanel(
       helpText("View Sentiment Map"),
       
       selectInput("var", 
                   label = "Choose item to display",
                   choices = c("Samsung", "Starbucks",
                                "Bill Oreilly", "France Elections"),
                   selected = "Bill Oreilly"),
       selectInput("senti", 
              label = "Choose a sentiment to display",
                choices = c("Anger", "Joy",
                   "Sadness", "Fear","Surprise", "Disgust"),
                   selected = "Anger")),
     
     mainPanel(plotOutput("map")
     )
   ),
   sidebarLayout(
     sidebarPanel(
       helpText("View sentiment over time"),
      
       selectInput("var2", 
                   label = "Choose a variable to display",
                   choices = c( 
                     "OreillyKentuckyApril20", 
                     "OreillyKentuckyApril21",
                     "OreillyKentuckyApril22", 
                     "OreillyKentuckyApril23", 
                     "OreillyKentuckyApril24",
                     "OreillyKentuckyApril28", 
                     "OreillyKentuckyApril29",
                     "OreillyMiamiApril20",
                     "OreillyMiamiApril21" ,
                     "OreillyMiamiApril22", 
                     "OreillyMiamiApril23", 
                     "OreillyMiamiApril24",
                     "OreillyMiamiApril27", 
                     "OreillyMiamiApril28", 
                     "OreillyMiamiApril29", 
                     "OreillyNYCApril20" ,
                     "OreillyNYCApril21" ,
                     "OreillyNYCApril22", 
                     "OreillyNYCApril23" ,
                     "OreillyNYCApril24",
                     "OreillyNYCApril27" ,
                     "OreillyNYCApril28", 
                     "OreillyNYCApril29", 
                     "OreillyUtahApril20" ,
                     "OreillyUtahApril21" , 
                     "OreillyUtahApril22" , 
                     "OreillyUtahApril23" , 
                     "OreillyUtahApril24" , 
                     "OreillyUtahApril27", 
                     "OreillyUtahApril28", 
                     "OreillyUtahApril29" , 
                     "FranceKentuckyApril23",
                     "FranceKentuckyApril24" , 
                     "FranceKentuckyApril25" , 
                     "FranceKentuckyApril26" , 
                     "FranceKentuckyApril27" ,
                     "FranceKentuckyApril28" ,
                     "FranceKentuckyApril29", 
                     "FranceMiamiApril23" ,  
                     "FranceMiamiApril24" , 
                     "FranceMiamiApril25" , 
                     "FranceMiamiApril26", 
                     "FranceMiamiApril27" , 
                     "FranceMiamiApril28", 
                     "FranceMiamiApril29", 
                     "FranceNYApril23" ,  
                     "FranceKYApril25", 
                     "FranceKYApril26", 
                     "FranceKYApril27", 
                     "FranceKYApril28", 
                     "FranceKYApril29" , 
                     "FranceUtahApril23",  
                     "FranceUtahApril24",  
                     "FranceUtahApril25",  
                     "FranceUtahApril26",  
                     "FranceUtahApril27",  
                     "FranceUtahApril28",  
                     "FranceUtahApril29"),
                   selected = "OreillyKentuckyApril20")),
     mainPanel(
       plotOutput("distPlot")
   )
   ),
   
   #NEW SIDEBAR
   sidebarLayout(
     sidebarPanel(
       helpText("Type Tweet"),
       # Copy the line below to make a text input box
       textInput("text22", label = h3("Text input"), value = "Enter text"),
       
       hr(),# Copy the line below to make an action button
       actionButton("action2", label = "Action")
       
       # You can access the value of the widget with input$action, e.g.
     ),
   
     mainPanel(
       plotOutput("distPlot22")
     )
   )
)      # Show a plot of the generated distribution
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput2 <- reactive({ 
  switch(input$var,
          "Bill Oreilly" = BillOreilly,
          "Samsung" = Samsung,
          "France Elections" = Elections,
          "Starbucks" = Starbucks
  )
  })
  sentiInput <- reactive({ 
    switch(input$senti,
        "Anger" = "anger", 
        "Joy" = "joy",
        "Sadness" = "sadness", 
        "Fear" = "fear",
        "Surprise" = "surprise",
        "Disgust" = "disgust"
    )
  })
  
    output$map <- renderPlot({
      dataset2 <- datasetInput2()     
          emotions <- sentiInput()
    
    MyMap <- GetMap(center = c(lat = 39, lon = -98), size = c(640, 640),
                    zoom = 4, path = "", sensor = "true",
                    maptype = c("terrain"),
                    format = c("png32"),
                    extraURL = "", RETURNIMAGE = TRUE, GRAYSCALE = FALSE, NEWMAP = TRUE,
                    SCALE = 1, API_console_key = NULL, verbose = 0)
    
    tmp <- PlotOnStaticMap(MyMap, lat = dataset2$lat,
                           lon = dataset2$long, cex=((dataset2$emotions)/dataset2$total)*50,pch=20,
                           col="red", add=FALSE)
    
    
  })
  

  # You can access the value of the widget with input$action, e.g.

  datasetInput <- reactive({
    switch(input$var2,
           "OreillyKentuckyApril20" = OreillyKentuckyApril20, 
           "OreillyKentuckyApril21" = OreillyKentuckyApril21,
           "OreillyKentuckyApril22" = OreillyKentuckyApril22, 
           "OreillyKentuckyApril23" = OreillyKentuckyApril23, 
           "OreillyKentuckyApril24" = OreillyKentuckyApril24,
           "OreillyKentuckyApril28" = OreillyKentuckyApril28, 
           "OreillyKentuckyApril29" = OreillyKentuckyApril29,
           "OreillyMiamiApril20" = OreillyMiamiApril20,
           "OreillyMiamiApril21" = OreillyMiamiApril21,
           "OreillyMiamiApril22" = OreillyMiamiApril22, 
           "OreillyMiamiApril23" = OreillyMiamiApril23, 
           "OreillyMiamiApril24" = OreillyMiamiApril24,
           "OreillyMiamiApril27" = OreillyMiamiApril27, 
           "OreillyMiamiApril28" = OreillyMiamiApril28, 
           "OreillyMiamiApril29" = OreillyMiamiApril29, 
           "OreillyNYCApril20" = OreillyNYCApril20,
           "OreillyNYCApril21" = OreillyNYCApril21,
           "OreillyNYCApril22" = OreillyNYCApril22, 
           "OreillyNYCApril23" = OreillyNYCApril23,
           "OreillyNYCApril24" = OreillyNYCApril24,
           "OreillyNYCApril27" = OreillyNYCApril27,
           "OreillyNYCApril28" = OreillyNYCApril28, 
           "OreillyNYCApril29" = OreillyNYCApril29, 
           "OreillyUtahApril20" = OreillyUtahApril20,
           "OreillyUtahApril21" = OreillyUtahApril21, 
           "OreillyUtahApril22" = OreillyUtahApril22, 
           "OreillyUtahApril23" = OreillyUtahApril23, 
           "OreillyUtahApril24" = OreillyUtahApril24, 
           "OreillyUtahApril27" = OreillyUtahApril27, 
           "OreillyUtahApril28" = OreillyUtahApril28, 
           "OreillyUtahApril29" = OreillyUtahApril29, 
           "FranceKentuckyApril23" = FranceKentuckyApril23,
           "FranceKentuckyApril24" = FranceKentuckyApril24, 
           "FranceKentuckyApril25" = FranceKentuckyApril25, 
           "FranceKentuckyApril26" = FranceKentuckyApril26, 
           "FranceKentuckyApril27" = FranceKentuckyApril27,
           "FranceKentuckyApril28" = FranceKentuckyApril28,
           "FranceKentuckyApril29" = FranceKentuckyApril29, 
           "FranceMiamiApril23" = FranceMiamiApril23,  
           "FranceMiamiApril24" = FranceMiamiApril24, 
           "FranceMiamiApril25" = FranceMiamiApril25, 
           "FranceMiamiApril26" = FranceMiamiApril26, 
           "FranceMiamiApril27" = FranceMiamiApril27, 
           "FranceMiamiApril28" = FranceMiamiApril28, 
           "FranceMiamiApril29" = FranceMiamiApril29, 
           "FranceNYApril23" = FranceNYApril23,  
           "FranceKYApril25" = FranceNYApril25, 
           "FranceKYApril26" = FranceNYApril26, 
           "FranceKYApril27" = FranceNYApril27, 
           "FranceKYApril28" = FranceNYApril28, 
           "FranceKYApril29" = FranceNYApril29, 
           "FranceUtahApril23" = FranceUtahApril23,  
           "FranceUtahApril24" = FranceUtahApril24,  
           "FranceUtahApril25" = FranceUtahApril25,  
           "FranceUtahApril26" = FranceUtahApril26,  
           "FranceUtahApril27" = FranceUtahApril27,  
           "FranceUtahApril28" = FranceUtahApril28,  
           "FranceUtahApril29" = FranceUtahApril29 
           
           )
  })
  
   output$distPlot <- renderPlot({
     
     
     dataset2 <- datasetInput()     
     dataset <-gettxt(dataset2)
     
       ggplot(dataset, aes(x=emotion)) +
         geom_bar(aes(y=..count.., fill=emotion)) +
         scale_fill_brewer(palette='Dark2') +
         labs(x='emotion categories', y='number of tweets') +
         ggtitle('Sentiment Analysis of Tweets\n(classification by emotion)') +
         theme(plot.title = element_text(size=12, face='bold'))+ coord_polar()

   })
   
   observeEvent(input$action2, {
       BillOreilly2 <- start(input$text22)
       output$distPlot22 <-  renderPlot({
         ggplot(BillOreilly2, aes(x=emotion)) +
           geom_bar(aes(y=..count.., fill=emotion)) +
           scale_fill_brewer(palette='Dark2') +
           labs(x='emotion categories', y='number of tweets') +
           ggtitle('Sentiment Analysis of Tweets\n(classification by emotion)') +
           theme(plot.title = element_text(size=12, face='bold'))
       })
     
       })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

