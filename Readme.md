We have created a Shiny app that will run a certain phrase through a sentiment analysis, which uses the Naïve Bayes Classifier with a set time and location. The goal was to retrieve our own data from the Twitter API and determine how people are feeling toward a specific subject in real time at different locations. 

Hashtags Observed: 
•	Bill Oreilly
o	Keywords- “Fox”, "FowNews", "Oreilly", "@oreillyfactor”
o	Days-8
o	Time- 300 seconds (5 minutes)
•	Starbucks’ Unicorn Frappuccino
o	Keywords- "Starbucks", "UnicornFrappuccino", "Unicorn frap"
o	Days-8
o	Time- 300 seconds (5 minutes)
•	Samsung’s Galaxy
o	Keywords- "s8 samsung", "galaxys8", "samsung", "samsunggalaxy8", "samsungalaxy 8"
o	Days-8
o	Time- 300 seconds (5 minutes)
•	The French Election
o	Keywords - "french election", "le pen", "emmanuel macron"
o	Days-7
o	Time- 300 seconds (5 minutes)

Locations (latitude/longitude):
San Francisco, California (37.63, -123.17)
Cincinnati, Ohio (39.05, -84.71)
Boise, Idaho (43.51, -116.37)
Chicago, Illinois (40.76, -111.89)
Louisville, Kentucky (41.64, -87.94)
New York City, New York (40.71, -74.005)
Salt Lake City, Utah (25.76, -80.19)
Florida, Miami (38.25, -85.75)

Packages: 
pacman, ggplot , RgoogleMaps, ggmap

Libraries:
ROAuth, twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc, streamR, RgoogleMaps

Retrieving Data
First, we declared the Twitter API credentials and created the handshake. Second, we saved that information into an Rdata file. Third, read in the positive and negative words that were already created. These files were retrieved from github and  would be used in the sentiment analysis later on. The used the filterStream function that filtered tweets at our specified location that contained the keywords for a set time (300 secs.). The location’s latitude and longitude was obtained through the geocode function from ggmap. This function also stored the tweet collected into a .json file. Lastly, we created a function that parses the tweets from the file and “cleans” the tweets. The function removed retweets, websites, @’s, etc.

Creation of Shiny App:

Map Visualization
Displays the emotion people are feeling at each location for the set topics. 
A data frame was created based on the results from the sentiment analysis. This data frame contains the total amount of tweets collected for each sentiment over the set time period. The size of each point illustrates the amount of tweets that were collected for that sentiment. 

Pie Chart Visualization
Presents the overall sentiments for a trend on a certain day. The data was classified using a classify_emotion function. Then the best fit for the resulting data was found. Next, NA’s were taken care of by being replaced by unknown. After, the data’s polarity was classified and best fit was found. Finally, the tweets were sorted and put into a data frame and plotted.

Bar Graph Visualization


Faults/Difficulties:
-Retrieving our own data, is time consuming to do for multiple trends
-ggmap had a bug when trying to plot on a Google map image
-Several unknowns in data (ex. Spam) despite cleaning, may not be completely accurate
-Take into consideration that several people use Twitter to complain which can alter the sentiment interpretation 
-Size of point for plots stopped working once moved to Shiny App

