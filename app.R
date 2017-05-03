#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(maps)
library(mapproj)
source("/Users/jc/Downloads/helpers.R")
counties <- readRDS("/Users/jc/Downloads/counties.rds")
emotionRes <- readRDS("/Users/jc/Downloads/OreillyResults.rds")
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
     gettxt <- function(file){
       # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include      lat/lon information in that data frame.
       file.df <- parseTweets(file, simplify = FALSE)
       tweet_txt <-subset(file.df, select = "text")
       tweet_txt <-file.df$text
       # remove retweet entities
       tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_txt)
       # remove at people
       tweet_txt = gsub("@\\w+", "", tweet_txt)
       # remove punctuation
       tweet_txt = gsub("[[:punct:]]", "", tweet_txt)
       # remove numbers
       tweet_txt = gsub("[[:digit:]]", "", tweet_txt)
       # remove html links
       tweet_txt = gsub("http\\w+", "", tweet_txt)
       # remove unnecessary spaces
       tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
       tweet_txt = gsub("^\\s+|\\s+$", "", tweet_txt)
       tweet_txt = gsub("[^0-9a-zA-Z ,./?><:;’~`!@#&*’]","", tweet_txt)
       
       # Perform Sentiment Analysis
       # classify emotion
       
       classify(tweet_txt)
     }
     
     classify <- function(x){
       class_emo = classify_emotion(x, algorithm='bayes', prior=1.0)
       
       # get emotion best fit
       emotion <- class_emo[,7]
       
       # substitute NA's by 'unknown'
       emotion[is.na(emotion)] = 'unknown'
       
       # classify polarity
       class_pol = classify_polarity(x, algorithm='bayes')
       # get polarity best fit
       polarity = class_pol[,4]
       
       # Create data frame with the results and obtain some general statistics
       # data frame with results
       sent_df = data.frame(text=x, emotion=emotion,
                            polarity=polarity, stringsAsFactors=FALSE)
       # sort data frame
       sent_df = within(sent_df,
                        emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
       
       
       # Let’s do some plots of the obtained results
       # plot distribution of emotions
       ggplot(sent_df, aes(x=emotion)) +
         geom_bar(aes(y=..count.., fill=emotion)) +
         scale_fill_brewer(palette='Dark2') +
         labs(x='emotion categories', y='number of tweets') +
         ggtitle('Sentiment Analysis of Tweets\n(classification by emotion)') +
         theme(plot.title = element_text(size=12, face='bold'))
       
     }
     
     gettxt("/Users/jc/Desktop/TwitterR/franceKy23.json") ##ADD JSON FILE HERE
     
      # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
     # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
     # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   
   #NEW SIDEBAR
   
    
}


# Run the application 
shinyApp(ui = ui, server = server)

