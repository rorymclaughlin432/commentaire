

#====Commentaire - An Application for Data Extraction and Text Mining of YouTube Comments====#

#Project by Rory McLaughlin#

#This software will endeavour to extract YouTube data and manipulate it into various ways.
#The data will have many purposes and this will be displayed for the user on screen.
#Comment data is useful for user metrics and understanding the users and their perspective

          #====================R Packages======================#

#The packages below help shape the basic structure of the software
#shiny and shinydashboard are for the visual elements
#vosonSML and tuber are for getting the YouTube Data
#the other packages are mainly for arranging and manipulating data

suppressPackageStartupMessages({
#package for visual user interface
library(shinydashboard)
library(shiny)

# shinyBS for validation of text input
library(shinyBS)

#shinyjs for validating submit button input
library(shinyjs)
  
#for processing data tables
library(data.table)
library(DT)

#packages for extracting YouTube Data
library(vosonSML)
library(tuber)

#package for fixing the date format
library(lubridate)

#package for plots
library(plotly)

#package for refreshing and loading output data
library(shinycssloaders)

#packages for mining/sorting/arranging the extracted data
library(tidyverse)
library(stringr)
library(dplyr)

#packages for processing/analysing/stemming words in comments for data dictionary/sentiment analysis
library(tidytext)
library(tidyr)
library(SnowballC)
library(udpipe)
library(stopwords)
library(syuzhet)
library(wordcloud2)
library(ggwordcloud)
library(textdata) 

#saving wordcloud  
library(htmlwidgets)
library(webshot)
library(glue)
  suppressMessages(
webshot::install_phantomjs())
})



#=================================Verification and Authentication for YouTube Data API==============================#

#The main purpose of the project is extracting YouTube data
#For this to happen we must gain access to the API
#The 'yt_oath' function of tuber is used
#'yt_oath' uses the client_id and the client_secret of the developer to authenticate their credentials
#After this the 'Authenticate' function of vosonSML is used
#'Authenticate' verifies the developer's API Key

#YouTube Client ID for using YouTube API
client_id <- 
client_secret <- 

# YouTube oauth for authenticating ID 
yt_oauth(app_id = client_id,
         app_secret = client_secret,
         scope = "ssl",
         token = ".httr-oauth") 

# you may be redirected to your browser to verify the software through google accounts

#api key must be created to process YouTube API requests and verification/authentication
apikey <- 



#======================================Youtube Categories =======================================================#
#There is no facility to extract category details. Therefore, I have found the category names and their category ID online
#'I have put them in separate data tables which will be combined in the video category in server side
#'They will then be matched to the category ID of the ID Key

title <- c("Film & Animation","Autos & Vehicles","Music","Pets & Animals","Sports","Short Movies","Travel & Events",
           "Gaming","Videoblogging","People & Blogs","Comedy","Entertainment",
           "News & Politics","Howto & Style","Education","Science & Technology","Nonprofits & Activism","Movies","Anime/Animation","Action/Adventure",
           "Classics","Comedy","Documentary","Drama","Family","Foreign","Horror","Sci-Fi/Fantasy","Thriller","Shorts",
           "Shows","Trailers")
id <- c(1,2,10,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)

#===Converting Or to then===#
`%then%` <- shiny:::`%OR%`

#The max_char and min_char are for the YouTube ID Key textInput box  
#text input validation parameters for when an input is too short or too long
max_char = 11
min_char = 0

#======================================SAVE/LOAD STORED DATA=======================================================#
#Saving and Loading Data to review all the data that has been processed
#with help from
#https://www.pharmasug.org/proceedings/2020/AD/PharmaSUG-2020-AD-046.pdf#

#outputDir can be used to locally store data in a specific directory/folder
#tempDir() function can be used instead to store data temporarily in temporary folders.
#Temporary folders might be easier for using the software on a server

#outputDir <- "C:/responses"
#dir.create(file.path(outputDir), showWarnings = FALSE)

#for saving main comment data
saveData <- function(data) {
  colnames(data) <- NULL
  # Write the data to a temporary file locally
  file <- file.path(tempdir(), file="youtubecommentdata.csv")
  # Write the file to the local system
  data<-write.table(data, file, sep=',',append = TRUE)
  data<-data.frame(data)
  data
}

#for loading main comment data
loadData <- function() {
  file.create("youtubecommentdata.csv")
  file.copy("youtubecommentdata.csv", tempdir())
  f <- file.path(tempdir(), file="youtubecommentdata.csv")
  ytcolnames <- c("Number","Word","Category","Video","NumberofTimesUsed")
  #data<-read.csv(f, sep=",", stringsAsFactors=FALSE)
  data<-read.table(f, sep=",", stringsAsFactors=FALSE,col.names = ytcolnames, header = TRUE, fill = TRUE)
  keeps <- c("Word","Category","Video","NumberofTimesUsed")
  data<-data[keeps]
}

#for saving video stats/details
saveViewDetailsData <- function(data) {
  colnames(data) <- NULL
  # Write the data to a temporary file locally
  f <- file.path(tempdir(), file="youtubeviewdetailsdata.csv")
  # Write the file to the local system
  data<-write.table(data, f, sep=',', append = TRUE)
  data<-data.frame(data)
  data
}

#for loading video stats/details
loadViewDetailsData <- function() {
  file.create("youtubeviewdetailsdata.csv")
  file.copy("youtubeviewdetailsdata.csv", tempdir())
  f <- file.path(tempdir(), file="youtubeviewdetailsdata.csv")
  ytcolnames<- c("Number","Video","Date","Views","Likes","Dislikes","NumberofComments","Category")
  data<-read.table(f, sep=",", stringsAsFactors=FALSE,col.names = ytcolnames,header=FALSE)
  keeps <- c("Video","Date","Views","Likes","Dislikes","NumberofComments","Category")
  data<-data[keeps]
  
}

#==================================================USER INTERFACE(UI)===========================================================#

#==============================================Start of  User Interface ========================================================#
  
ui <- function(req) {
  
  dashboardPage(
    
    title="Commentaire", 
  
  #makes theme red
  skin = "red",
  
#===============start of header=============#
  dashboardHeader(
    
    #Commentaire icon on left side of header
    title = tags$a(tags$img(src='https://i.ibb.co/YBGmMGy/commentairesmall.png',width='150'),width = 600),
    
    #About Commentaire on right side of header
    tags$li(
      class = "dropdown",
      a(strong("About Commentaire"),
        onclick = "openTab('aboutcommentaire')",
        href = NULL,
        style = "cursor: pointer;"
      )
    )
    
  ),
#===============End of header=============#
  
#===============start of sidebar=============#
  dashboardSidebar(
    
    #Displays sidebar widgets to display data e.g. datatables, plot graphs, video info
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome!", tabName = "welcome", icon = icon("comment-dots")),
      
      menuItem("Start", tabName = "startpage", icon = icon("play-circle")),
      
      menuItem("Video Information", tabName = "vidinfo", icon = icon("info")),
      
      menuItem("Comments", tabName = "commentstab", icon = icon("th")),
      
      menuItem("Popularity of Comments", tabName = "commentdatestab", icon = icon("th")),
      
      menuItem("Analysis of Words", tabName = "analysisofwordstable", icon = icon("th")),
      
      menuItem("Informative Words", tabName = "informativewords", icon = icon("th")),
      
      menuItem("Likes Vs Replies", tabName = "lvr", icon = icon("chart-line")),
      
      menuItem("Top 10 Likes vs Replies", tabName = "toptenlikesvsrepliesgraph", icon = icon("chart-line")),
      
      menuItem("Most Used Words", tabName = "mostusedwords", icon = icon("chart-bar")),
      
      menuItem("Sentimentality Analysis", tabName = "sentimentalplottab", icon = icon("chart-bar")),
      
      menuItem("Positive and Negative Words", tabName = "positivenegativeplotgraph", icon = icon("chart-bar")),
      
      menuItem("Stored Data", tabName = "storeddata", icon = icon("th")),
      
      menuItem("Stored Data Video Stats", tabName = "storedvideodetailsdata", icon = icon("th")),
      
      menuItem("Stored Data WordCloud", tabName = "storeddatawordscloud", icon = icon("chart-bar")),
    
      menuItem("Stored Data Plots & Graphs", tabName = "mostviewedvideosstoreddata", icon = icon("chart-bar")),
      
      menuItem("Stored Data By Category", tabName = "storeddatabycategory", icon = icon("chart-bar")),
      
      menuItem("About", tabName = "aboutcommentaire", icon = icon("question-circle")),
      
      uiOutput('style_tag') # This is to change the about page to a red background with white text
    )
    
  ),
  

#========================================================Body of software ============================================#
  dashboardBody(
    
    shinyjs::useShinyjs(), #enable shiny javascript package to use in software
    
    #========================HTML tags for adjustments================================================#
    #makes 'About Commentaire' logo in top right corner go to About tab on click
    #With help from 
    #https://community.rstudio.com/t/how-to-create-this-user-panel-on-top-right-corner-of-shiny/7806/7#
    tags$head(tags$script(HTML("
          var openTab = function(tabName){
            $('a', $('.sidebar')).each(function() {
              if(this.getAttribute('data-value') == tabName) {
                this.click()
              };
            });
          }
          "))),
    
    #when data is loading the details will hide until fully processed. When processed they will show
    #With help from
    #https://www.r-bloggers.com/add-calculation-in-process-indicator-for-shiny-application/#
    tags$head(HTML("<script>
      var url = '/scripts/script.js';
      $.getScript(url);
              setInterval(function(){
                if ($('html').attr('class')=='shiny-busy') {
                  $('div.videodetailsbusy').show()
                  $('div.viddetails').hide()
                } else {
                  $('div.videodetailsbusy').hide()
                  $('div.viddetails').show()
                }
              },100)
              </script>")),
    
    #Style changes in HTML 
    #turns text boxes in welcome tab grey
    #highlights sidebar tab names in red
    tags$style(HTML("
    .box.box-solid.bg-yellow{background-color: slategrey!important;}
    .skin-red .main-sidebar .sidebar .sidebar-menu .active a{background-color: #ff0000;}
    
    
    
                ")),
    
    
#================================================SIDEBAR TABS ==================================================#
    
  tabItems(
      
#=====================Welcome Page=====================#
      
      tabItem(tabName = "welcome", 
              div(
                box(
                width = 400, height = 120, background = "red",
                tags$a(
                  tags$img(src='https://i.ibb.co/YBGmMGy/commentairesmall.png',width='300'),
                  width = 600)
                ),
                h1(strong("Welcome to Commentaire!")),
                  h2("An Application for Data Extraction and Text Mining of YouTube Comments"),
                  style="text-align: center;"),
              br(),
              div(fluidRow(align="center",
              actionButton(inputId = "startbutton", label ="Let's Start!", 
              style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;" )
              )),
              br(),
              div(fluidRow(
                box(
                  width = 4, height = 130, background = "red",
                  h4("This software will help you to sort and rank comments of a YouTube video by different criteria")
                ),
                box(
                  width = 4, height = 130, background = "yellow",
                  h4("Compare and contrast comment data with different graphs, charts and data tables")
                ),
                box(
                  width = 4, height = 130, background = "red",
                  h4("Explore the variety of opinions and sentiments in comments and how they impact a video")
                )
              ),style="text-align: center;"),
              div(fluidRow(
                box(
                  width = 4, height = 130, background = "yellow",
                  h4("Analyse words and phrases from comments to see how effective they are in shaping opinions")
                ),
                box(
                  width = 4, height = 130, background = "red",
                  h4("Evaluate the positive and negative perspectives within comments")
                ),
                box(
                  width = 4, height = 130, background = "yellow",
                  h4("Download and enjoy the comment data offline")
                )
              ),style="text-align: center;")
              
              
              
      ),
      
#=====================Start=====================#
      tabItem(tabName = "startpage",
              div(
                h2(strong("What is a YouTube ID Key?")),
                style="text-align: center;"),
              div(fluidRow(align="center",
                  h4("It is the 11 character code at the end of a YouTube link in", strong("bold"), "below:"),
                  h4("youtube.com/watch?v=",strong("1SvZsXAMpqQ")),
                  h4(strong("1SvZsXAMpqQ"),"is a YouTube ID Key")
                
              ),style="text-align: center;"),
              
              useShinyjs(),
              div(
                fluidRow(
                align="center",
                h4("Find the YouTube ID Key of your video and enter it in the text box", strong("below:")),
                textInput(inputId = "commentkey",label = "",value = ""),
                div(tags$style("#container * {display: inline;}"),
                    div(id="container",
                actionButton(inputId = "submitcommentkey", label ="Submit",icon("paper-plane"), 
                             style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;margin-right:25px;" ),
                
                actionButton(inputId = "resetbutton", label ="Reset",icon("refresh"), 
                             style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%" )
                ),style="display:inline")),
                br(),
                div(align="center",
                    h4("Click the", strong("'Reset'"), "button to clear the text box and start with a new ID Key")
                )
              )
              
              
      ),
      
#=====================Video Information=====================#
      tabItem(tabName = "vidinfo",
              
              h2(strong("Video Information")),
              br(),
              
              div(class = "videodetailsbusy",
                  div(
                    h2(strong("Searching for Comments. Please wait...")),
                    img(
                      tags$img(src="https://icon-library.com/images/loading-please-wait-icon/loading-please-wait-icon-22.jpg")
                    ),
                    style="text-align: center;"
                  )
              ),
              #The div class viddetails is used to hide/show the details when the ID Key is changed/updated
              div(class = "viddetails",
              htmlOutput("videophoto"),
              
              h4(strong("Video:")), h4(textOutput("videoname")),
              
              h4(strong("Uploaded By:")), h4(textOutput("uploader")),
              
              h4(strong("Upload Date:")),h4(textOutput("date")),
              
              h4(strong("Number of Comments:")), h4(textOutput("videocommentcount")),
              
              h4(strong("Number of Views:")), h4(textOutput("videoviewcount")),
              
              h4(strong("Number of Likes:")), h4(textOutput("videolikecount")),
             
              h4(strong("Number of Dislikes:")), h4(textOutput("videodislikecount")),
              
              h4(strong("Genre/Category:")), h4(textOutput("youtubegenre"))
              )
        ),

#===========DATA TABLES================#

      #===Data table of all comments===#
        tabItem(tabName ="commentstab",
                h2(strong("Comments")),
                h4("Listed below are the extracted comments in full"),
                h4("You can view the comments in different formats from the drop down menu below"),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Searching for Comments. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
                selectInput("commentstype", label = "Choose Type of Comments:",
                            choices = c("All", 
                                        "Top 10% of All Comments", 
                                        "Top 10 Most Liked Comments", 
                                        "Top 10 Most Replied Comments",
                                        "Top 10% of Liked Comments",
                                        "Top 10% of Replied Comments"), selected = 'All'),
                
                downloadButton("commentstableCSV", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"), 
                br(),
                br(),
                DT::dataTableOutput('commentstablehere') %>% withSpinner(color="#f0151a")
                
                
        ),

        #data table of Popularity of Comments
        tabItem(tabName ="commentdatestab",
                h2(strong("Popularity of Comments")),
                h4("This tab shows the popularity of comments based on likes and replies by Date"),
                h4("The user can view comments based on Day, Month or Year"),
                br(),
                selectInput("commentdates", label = "Choose Type of Date:",
                            choices = c("Days", "Months", "Years"), selected = 'Days'),
                selectInput("likesreplies", label = "Choose:",
                            choices = c("Likes", "Replies"), selected = 'Likes'),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Comments Charts loading. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
                downloadButton("popularitycommentsCSV", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"), 
                br(),
                br(),
                plotOutput("datesplothere") %>% withSpinner(color="#f0151a")
                
        ),

      #data table of words extracted from comments
      tabItem(tabName ="analysisofwordstable",
              h2(strong("Analysis of Words")),
              h4("These are words that are extracted from the comments"),
              h4("They are categorised by frequency of use"),
              h4("Top 200 Words are selected to focus on the most popular words"),
              h4("Word Type (e.g. noun, verb) and Number of Times Used are shown below"),
              downloadButton("analysisofwordstableCSV", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
              br(),
              div(class = "videodetailsbusy",
                  div(
                    h2(strong("Words loading. Please wait...")),
                   
                    style="text-align: center;"
                  )
              ),
              br(),
              DT::dataTableOutput("analysisofwordstable") %>% withSpinner(color="#f0151a")
              
      ),

      #data table of informative words used in comments
      #two words that are likely to be combined together
      tabItem(tabName ="informativewords",
              h2(strong("Informative Words")),
              h4("These are two-word phrases that are extracted from the comments"),
              h4("They are calculated based on their relationship to one another"),
              h4("They add some significance and perspective to what users are discussing in the comments"),
              downloadButton("informativewordsCSV", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
              br(),
              div(class = "videodetailsbusy",
                  div(
                    h2(strong("Informative Words loading. Please wait...")),
                    
                    style="text-align: center;"
                  )
              ),
              br(),
              DT::dataTableOutput("informativewordstable") %>% withSpinner(color="#f0151a")
              
      ),
      
#===================================================================PLOTS==================================================================================#
    
      #Plot Graph of Likes vs. Replies
        tabItem(tabName ="lvr", 
                h2(strong("Overall Comments Based on Likes Vs. Replies")),
                h4("The graph below shows all comments based on the number of likes and replies they have received"),
                h4("The graph points are the comments and the line will track the estimated progression"),
                h4("The top comment by most likes is included as well"),
                br(),
                 downloadButton("downloadPlot1", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                br(),
                br(),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Likes Vs. Replies loading. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
               
                  fluidRow(
                  box(
                    title = "Top Comment By Most Likes", status = "danger", solidHeader = TRUE,
                    textOutput("number1comment") %>% withSpinner(color="#f0151a")
                    ),style="text-align: center;"
                  ),
                br(),
                 plotlyOutput("likesversusreplies",height = "500px") %>% withSpinner(color="#f0151a")
                
        ), 
        
        #Plot Top 10 Comments By Likes vs. Replies Graph
        tabItem(tabName ="toptenlikesvsrepliesgraph", 
                h2(strong("Top 10 Comments Based on Likes vs. Replies")),
                h4("The graph below displays Top 10 Comments based on the number of Likes and Replies"),
                downloadButton("downloadPlot2", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                br(),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Top 10 Likes Vs. Replies Loading. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
                br(),
                plotlyOutput("toptenlvrgraph",height = "500px") %>% withSpinner(color="#f0151a")
                
                
        ),

      
    #Plot bar chart of Most Used Words
        tabItem(tabName ="mostusedwords", 
                h3(strong("Most Frequently Used Words")),
                h4("These are words that users have used the most frequently in their comments"),
                h4("This is a good insight into what interests the users most about the video"),
                downloadButton("downloadPlot3", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                br(),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Most Frequently Used Words are loading. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
                br(),
                plotlyOutput("mostusedwords", height = "700px") %>% withSpinner(color="#f0151a")
                
                
        ),

      #Plot bar chart evaulating emotions and sentimentality in comments
      tabItem(tabName ="sentimentalplottab", 
              h3(strong("Sentimentality Analysis of Youtube Comments")),
              h4("This graph analyses how comments reflect a wide variety of emotions. The emotions are categorised by their frequency in the comments"),
              h4("It displays how user's opinions and attitudes impact a video"),
              downloadButton("downloadPlot5", "Download", style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
              br(),
              div(
                class = "videodetailsbusy",
                  div(
                    h2(strong("The sentiment dictionary takes a while to load. Please wait...")),
                    
                    style="text-align: center;"
                  )
              ),
              br(),
              plotOutput("sentimentalplot", height = "700px") %>% withSpinner(color="#f0151a")             
      ),
    
    #Plot bar chart of Positive Negative Words Plot Graph
    tabItem(tabName ="positivenegativeplotgraph", 
            h3(strong("Positive and Negative Words")),
            h4("These words are assessed and categorised as either a 'positive' word or a 'negative' word"),
            h4("The sentiment is evaluated and they are organised by frequency of use"),
            downloadButton("downloadPlot4", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
            br(),
            div(class = "videodetailsbusy",
                div(
                  h2(strong("Positive and Negative Words loading. Please wait...")),
                  style="text-align: center;"
                )
            ),
            br(),
            plotlyOutput("positivenegativeplot", height = "700px") %>% withSpinner(color="#f0151a")
            
            
        ),

#===================================================================STORED DATA==================================================================================#

        #Locally Stored Data - This comment data is accumulated every time a user
        #'runs a new ID Key. The data from each key is saved in the saveData() function
        #'It is then loaded in the loadData() function in an output
        tabItem(tabName ="storeddata", 
                h3(strong("Stored Youtube Comments")),
                h4("This data shows all the stored data"),
                h4("Every ID Key the user analyses, the comment data can be saved here"),
                h4("If ", strong("'Please input ID Key!'"), "appears below then please input a new ID Key to see your previously stored comment data"),
                br(),
                
                div(tags$style("#container * {display: inline;}"),
                    div(id="container",
                     downloadButton("storeddataCSV", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;")
                    ),style="display:inline"),
                br(),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Stored Data Comments loading. Please wait...")),
                      style="text-align: center;"
                    )
                ),
                br(),
                dataTableOutput("overallstoreddata", height = "700px") %>% withSpinner(color="#f0151a")
        ),

        #Stored Data Word Cloud
        #This is a word cloud that accumulates popular words from the stored comment data
        tabItem(tabName ="storeddatawordscloud", 
                h3(strong("Word Cloud for Stored Data")),
                h4("The data here is shown in a word cloud"),
                h4("The words are displayed by popularity"),
                h4("If you want to download, please wait for the wordcloud to fully load then click on", strong("'Download'")),
                br(),
                actionButton(inputId = "loaddatabuttonwordcloud", label ="Load Data", 
                             style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;margin-right:25px;" ),
                downloadButton("storedwordcloudplot", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                br(),
                div(class = "videodetailsbusy",
                    div(
                      h2(strong("Wordcloud loading. Please wait...")),
                      
                      style="text-align: center;"
                    )
                ),
                br(),
                div(align="center",
                    wordcloud2Output("storeddatawords",width = "1380px", height = "720px") %>% withSpinner(color="#f0151a")
                )
                
        ),

          #Locally Stored Video Stats
          tabItem(tabName ="storedvideodetailsdata", 
                  h3(strong("Analysis of Stored Data Video Stats")),
                  h4("This tab shows all the stored data video statistics"),
                  h4("Every ID Key the user analyses, the Video Information data is saved here"),
                  br(),
                  div(tags$style("#container * {display: inline;}"),
                      div(id="container",
                        downloadButton("storedvideostatisticsCSV", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;")
                      ),style="display:inline"),
                  br(),
                  div(class = "videodetailsbusy",
                      div(
                        h2(strong("Video Stats loading. Please wait...")),
                        
                        style="text-align: center;"
                      )
                  ),
                  br(),
                  dataTableOutput("videodetailsstoreddata", height = "700px") %>% withSpinner(color="#f0151a")
                  
                  
          ),#

          #Stored Data Plots and Graphs
          #This is a word cloud that accumulates popular words from the stored comment data
          tabItem(tabName ="mostviewedvideosstoreddata", 
                  h3(strong("Stored Data Plots and Graphs")),
                  h4("The data here is shown in a variety of graphs"),
                  h4("The Video Titles have been truncated and shortened for visual purposes"),
                  br(),
                  actionButton(inputId = "loaddatabuttonmostviewed", label ="Load Data", 
                               style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;margin-right:25px;" ),
                  downloadButton("storedplotsandgraphsplot", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                  selectInput("vidstats", label = "Choose:",
                              choices = c("Views", "Likes", "Dislikes","Number of Comments")),
                  br(),
                  div(class = "videodetailsbusy",
                      div(
                        h2(strong("Most Viewed Graph loading. Please wait...")),
                        
                        style="text-align: center;"
                      )
                  ),
                  br(),
                  div(align="center",
                      plotlyOutput("mostviewedvideos", width = "1280px", height = "720px") %>% withSpinner(color="#f0151a")
                  )
                  
          ),

          #Stored Data by Category
          tabItem(tabName ="storeddatabycategory", 
                  h3(strong("Stored Data By Category")),
                  h4("The data here is shown based on category/genre"),
                  h4("The words are processed then grouped by the category/genre of their video"),
                  br(),
                  actionButton(inputId = "loaddatabuttoncategory", label ="Load Data", 
                               style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;margin-right:25px;" ),
                  downloadButton("storedcategoriesplot", "Download",style="color: #fff; font-weight:bold; background-color: red; padding:10px; font-size:150%;"),
                  selectInput("vidcategory", label = "Choose:",
                              choices = c("Comments","Word Frequency","Views", "Likes", "Dislikes")),
                  br(),
                  div(class = "videodetailsbusy",
                      div(
                        h2(strong("Categories loading. Please wait...")),
                        
                        style="text-align: center;"
                      )
                  ),
                  br(),
                  
                  div(align="center",
                      plotOutput("databycategory",width = "1280px", height = "720px") %>% withSpinner(color="#f0151a")
                  )
                  
          ),

    #===About Page====#
    tabItem(tabName ="aboutcommentaire", 
            fluidRow(
              tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "800px", width = "100px",
                tabPanel("About", "",
                         tags$a(
                           tags$img(src='https://i.ibb.co/YBGmMGy/commentairesmall.png',width='300'),
                           width = 600),
                         br(),
                         br(),
                         div(tags$style("#container * {display: inline;}"),
                             div(id="container",h4("Copyright 2020 All Rights Reserved"), icon("copyright"),style="display:inline")),
                         h3(strong("Version 1.0.0.6")),
                         h3(strong("Created By: Rory McLaughlin")),
                         h3(strong("Project Concept and Idea: Dr. Reza Rafiee")),
                         br(),
                         h4(strong("What is Commentaire?")),
                         h4("Commentaire is a data extraction software that specifically interacts with YouTube Comments"),
                         h4("Anyone can enter a YouTube ID Key and the video information will be processed and displayed"),
                         h4("It displays the data in a variety of formats e.g. data tables, graphs, word clouds"),
                         h4("It gives an in-depth insight into how commenters think and what their opinion about a video is"),
                         h4("It evaluates a video's popularity based on the popularity of comments by day, month or year"),
                         h4("Comments can be assessed based on sentimentality and frequency of emotions"),
                         br(),
                         h4(strong("What is the purpose of Commentaire?")),
                         h4("The current structure of comments on YouTube is vague, limited and disorganised"),
                         h4("Therefore, the purpose of this software is to make YouTube comments more accessible and easier to understand"),
                         h4("Commentaire provides an intuitive, creative alternative that is catered towards any and all users"),
                         style="text-align: center;"
                         
                ),
                tabPanel("Version History", "",
                         h2(strong("Version History")),
                         br(),
                         h4(strong("Version 1.0.0.6")),
                         h4("Minor Fixes"),
                         h4("Informative Words fixed"),
                         br(),
                         h4(strong("Version 1.0.0.5")),
                         h4("Minor Fixes"),
                         h4("Category numbering fixed"),
                         br(),
                         h4(strong("Version 1.0.0.4")),
                         h4("Minor Fixes"),
                         h4("Updated visual aspects such as colour coding"),
                         h4("Improved version history log"),
                         br(),
                         h4(strong("Version 1.0.0.3")),
                         h4("Minor Fixes"),
                         h4("Removed save buttons for stored data"),
                         h4("Added download button for stored data"),
                         br(),
                         h4(strong("Version 1.0.0.2")),
                         h4("Minor Fixes"),
                         h4("Updated save data/load data functions for stored data"),
                         h4("Introduced Categories/Genres"),
                         br(),
                         h4(strong("Version 1.0.0.1")),
                         h4("Minor Fixes"),
                         h4("First implementation"),
                         style="text-align: center;"
                )
                
              )
            )
        )
  
      
      )#end of tabs
    )#end of dashboardbody
  )#end of dashboardpage
}#end of ui

#=====================================================Server Side=====================================================================================================#

server <- function(input, output,session) {
  
#================OBSERVE EVENTS FOR VALIDATION=========================================#  
  
#====Error Handling for inputting ID Key====#
#'Determines whether an ID Key is too long or too short

    observeEvent(input$commentkey,{
    if(nchar(input$commentkey)>max_char){
      updateTextInput(session,'commentkey',value="")
      showModal(modalDialog(
        title = "",
        div(
          h3(strong("Error!")),
          img(
            tags$img(src="https://i.ibb.co/L0TWYrL/unnamed-1.png", height=40, width=40)
          ), 
          h4(strong("Your ID Key is too long!")),
          h4(strong("Please check it has 11 characters only!")),
          style="text-align: center;"
        ),
        easyClose = TRUE
      ))
    } else if((nchar(input$commentkey)<max_char) && (nchar(input$commentkey)>min_char)) {
      updateTextInput(session,'commentkey',value=substr(input$commentkey,11,max_char))
      showModal(
        modalDialog(
          title = "",
          div(
            h3(strong("Error!")),
            img(
              tags$img(src="https://i.ibb.co/L0TWYrL/unnamed-1.png", height=40, width=40)
            ), 
            h4(strong("Your Key is too short!")),
            h4(strong("Please check it has 11 characters only!")),
            style="text-align: center;"
          ),
          
          easyClose = TRUE
        )
      )
    } 
  }
  )
  
  #==styling for about tab - when the user clicks on the about tab its background colour and text colour change===#
  output$style_tag <- renderUI({
    if(input$tabs=='aboutcommentaire'){
      return(
        
        tags$head(tags$style(HTML(
          '.content-wrapper {background-color:#f2594e; color:white;}
          
          .nav-tabs-custom>.tab-content{
          background:#f2594e;
          }
          
          .nav-tabs-custom{
          background:#f2594e;
          box-shadow:0 1px 1px #f2594e;
          }
          
          .nav-tabs-custom>.nav-tabs>li.active:hover>a, .nav-tabs-custom>.nav-tabs>li.active>a{
          font-weight:bolder;
          }
          
          .nav-tabs-custom>.nav-tabs>li>a, .nav-tabs-custom>.nav-tabs>li>a:hover{
          font-weight:bolder;
          background: red;
          color:black;
          }
          
          .nav-tabs-custom>.nav-tabs>li.active>a{
          border-right-color:red;
          border-left-color:red;
          border-top-color:red;
          background: red;
          color:white;
          }
          
          .nav-tabs-custom>.nav-tabs>li>a{
          color:white;
          font-weight:bolder;
          font-size: 150%;
          }
          
          .nav-tabs-custom>.nav-tabs>li.active{
          border-top-color:#f2594e;
          }
          
          .nav-tabs-custom>.nav-tabs{
          border-bottom-color:#f2594e;
          margin: 0px 0px 0px 20px;
          }
          
          '
          
            ))
          
          )
        
        
        )
    } else {
      return(tags$head(tags$style(HTML('.content-wrapper {background-color:#ecf0f5; color:black;}'))))
      
    }
  })
  
  #===================Buttons======================================================#
    
    #Start Button#  
    observeEvent(input$startbutton, {
      updateTabItems(session, "tabs", selected = "startpage")
    })
    
    #Submit Button# 
    observeEvent(input$submitcommentkey, {
      updateTabItems(session, "tabs", selected = "vidinfo")
      
    })
    
    
#==============Reset Button=========================#  
    # Clears ID Key in text box
    observeEvent(input$resetbutton, {
      updateTextInput(session,'commentkey',value="")
    })
    
#=====Submit Button Validation=====#
  #Validate submit button click - if text box is empty then submit button is disabled
  observe({
    if (is.null(input$commentkey) || input$commentkey == "") {
      shinyjs::disable("submitcommentkey")
    } else {
      shinyjs::enable("submitcommentkey")
    }
  })
    
#==REACTIVE OBJECTS===========================================================#
    
#=====Extracting comment data=====#
  
    #'This part is essential
    #'It uses the unique API Key with the provided YouTube Video ID Key
    #'to acquire and extract the data related to the video  
    
    yt_data <- reactive({
      #Validation for ID Key errors
    shiny::validate(
      need(input$commentkey != '', message = 'Please input ID Key!')
      %then% need(input$commentkey != 
      c(get_video_details(input$commentkey)$items[[1]]$kind), 
      message = 'ID Key is incorrect. Double check the ID Key in YouTube or your browser')
      )
    options(max.print = .Machine$integer.max)
    
    videoIDs <- c(input$commentkey)
    
    yt_data <- Authenticate("youtube", apiKey = apikey) %>%
      Collect(videoIDs = videoIDs, writeToFile=FALSE,verbose=TRUE)
  })

#====Comments extracted as a reactive object====================#
  #It can be used throughout for less data repetition===#    
    comments<-reactive({
      
      comment <-(yt_data()$Comment)
      username <-(yt_data()$AuthorDisplayName)
      dateposted <-(yt_data()$PublishedAt)
      dateposted <- ymd_hms(dateposted)
      likeCount <- (yt_data()$LikeCount)
      likes <- as.numeric(likeCount)
      replyCount <- (yt_data()$ReplyCount)
      numberofreplies <- as.numeric(replyCount)
      CommentList <- cbind.data.frame(comment,username,dateposted,numberofreplies,likes)
    })
    
    
#========================================Video Details============================================#  
        
  #Video Image - image is extracted
    videophoto <- reactive({
      yt_data()
      vidphoto <- (get_video_details(input$commentkey)$items[[1]]$snippet$thumbnails$medium$url)
    })
    
    output$videophoto <- renderText({
      c('<img src="',videophoto(),'">')
    })
  
  #Video Name - searches for the name of the Video being extracted
  videotitle <- reactive({
    yt_data()
    videonam <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    cat(videonam)
  })
  
  output$videoname <- renderPrint({
    videotitle()
  })
  
  #Channel Name - searches for the name of the channel
  uploader <- reactive({
    yt_data()
    channel <- (get_video_details(input$commentkey)$items[[1]]$snippet$channelTitle)
    cat(channel)
  })
  
  output$uploader <- renderPrint({
    uploader()
  })
  #Upload Date - searches for the date published/posted
  date <- reactive({
    yt_data()
    uploaddate <- (get_video_details(input$commentkey)$items[[1]]$snippet$publishedAt)
    uploaddate <- ymd_hms(uploaddate)
    dateee<- format(uploaddate, "%d-%m-%Y") # year
    cat(dateee)
  })
  output$date <- renderPrint({
    date()
  })
  
  #Video Comment Count - searches for number of comments for video
  videocommentcount <- reactive({
    yt_data()
    vidcommentcount <- (get_stats(input$commentkey)$commentCount)
    cat(vidcommentcount)
  })
  
  output$videocommentcount <- renderPrint({
    videocommentcount()
  })
  
  #Video View Count - searches for number of views of video
  videoviewcount <- reactive({
    yt_data()
    vidviewcount <- (get_stats(input$commentkey)$viewCount)
    cat(vidviewcount)
  })
  output$videoviewcount <- renderPrint({
  videoviewcount()
    })
  
  #Video Like Count - searches for number of likes of video
  videolikecount <- reactive({
    yt_data()
    vidlikecount <- (get_stats(input$commentkey)$likeCount)
    cat(vidlikecount)
  })
  
  output$videolikecount <- renderPrint({
    videolikecount()
  })
  
  #Video Dislike Count - searches for number of dislikes of video
  videodislikecount <- reactive({
    yt_data()
    viddislikecount <- (get_stats(input$commentkey)$dislikeCount)
    cat(viddislikecount)
  })
  
  output$videodislikecount <- renderPrint({
    videodislikecount()
  })
  
  #Video Genre/Category - searches for video's genre
  youtubegenrename <- reactive({
    yt_data()
    categories <- data.frame(id,title)
    CategoryID<-(get_video_details(input$commentkey)$items[[1]]$snippet$categoryId)
    CategoryID<-as.numeric(CategoryID)
    ca<-categories[which(CategoryID == categories$id),]
    ytcategory<-data.frame(ca)
    category <- ytcategory[, c("title")]
    cat(category)
  })
  
  output$youtubegenre <- renderPrint({
    youtubegenrename()
  })
#============================END OF VIDEO DETAILS=========================================#


  
#===================================Comment Tables================================#
  
  commentstable <- reactive({
    Comment <-(yt_data()$Comment)
    Username <-(yt_data()$AuthorDisplayName)
    dateposted <-(yt_data()$PublishedAt)
    Date <- ymd_hms(dateposted)
    likeCount <- (yt_data()$LikeCount)
    Likes <- as.numeric(likeCount)
    replyCount <- (yt_data()$ReplyCount)
    NumberOfReplies <- as.numeric(replyCount)
    CommentList <- cbind.data.frame(Comment,Username,Date,NumberOfReplies,Likes)
    CommentList
    
    if(input$commentstype=='All'){
      
      CommentList[order (-CommentList$Likes),]
      
    } else if(input$commentstype=='Top 10% of All Comments'){
      
      head(CommentList[order(CommentList$Likes,decreasing=T),],.10*nrow(CommentList)) 
      
    } else if(input$commentstype=='Top 10 Most Liked Comments'){
      
      CommentList[order(CommentList$Likes,decreasing = T)[1:10],]
      
    } else if(input$commentstype=='Top 10 Most Replied Comments'){
      
      CommentList[order(CommentList$NumberOfReplies,decreasing = T)[1:10],]
      
    }else if(input$commentstype=='Top 10% of Liked Comments'){
      n <- 10
      top10percc <- CommentList[CommentList$Likes > quantile(CommentList$Likes,prob=1-n/100),]
      top10percentoflikes <- top10percc[order(top10percc$Likes,decreasing = T),]
      
    }else if(input$commentstype=='Top 10% of Replied Comments'){
      n <- 10
      top10percc <- CommentList[CommentList$NumberOfReplies > quantile(CommentList$NumberOfReplies,prob=1-n/100),]
      top10percentofreplies <- top10percc[order(top10percc$NumberOfReplies,decreasing = T),]
      
    }
    
  })

  output$commentstablehere <- DT::renderDataTable({
    datatable(commentstable(),rownames= FALSE)
  })
  
  #Download CSV of Comments
  output$commentstableCSV <- downloadHandler(
    filename = function() {
      paste("commentstable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(commentstable(), file)
    }
  )
  
#===================Popularity of Comments================================#
  #'This plot graph is outlining the popularity of comments over time
  #'It categorises them by likes or replies against day,month or year
  #' select inputs are used to handle these variables
  datesplot <- function()({
    Comment<-yt_data()$Comment
    l<-yt_data()$LikeCount
    Likes<-as.numeric(l)
    reps<-yt_data()$ReplyCount
    Replies<-as.numeric(reps)
    dateposted <-(yt_data()$PublishedAt)
    dateposts <- ymd_hms(dateposted)
    day<-format(dateposts, "%A")
    day<-factor(day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
    month<-format(dateposts, "%B")
    month<-factor(month,levels = c("January", "February", "March", "April", "May", "June","July","August","September","October","November","December"))
    year<- format(dateposts, "%Y")
    ds<-data.frame(day,month,year,Likes,Replies,Comment)
    
    if(input$commentdates=='Days'){
     days<- ds %>% group_by(Days = day)
     days<-data.frame(days)
    } else if(input$commentdates=='Months'){
     months<- ds %>% group_by(Months = month) 
     months<-data.frame(months)
    } else if(input$commentdates=='Years'){
     years<- ds %>% group_by(Years = year)
     years <-data.frame(years)
    } else if(input$likesreplies=='Likes'){
      popularlikes<- ds %>% group_by(Likes = Likes)
      popularlikes <-data.frame(popularlikes)
    }else if(input$likesreplies=='Replies'){
      popularreplies<- ds %>% group_by(Replies = Replies)
      popularreplies <-data.frame(popularreplies)
    }
    
  })
  
  output$datesplothere <- renderPlot({
    ggplot(datesplot(), aes_string(x =input$commentdates, y = input$likesreplies)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=10),
            axis.title=element_text(size=14,face="bold")) + 
      labs(x="Dates", 
           y="Count", 
           title="Popularity of Comments By Likes/Replies and Dates")
    
  })
  
  #Download Popularity of Comments
  output$popularitycommentsCSV<-downloadHandler(
      filename = function() {
        paste(input$commentkey,'popularitycomments',Sys.Date(), 'png', sep='.')
      },
      content=function(file){
        ggsave(file, width = 34, height = 20, units = "cm")
      }
    )
 
#===================WORDS================================#  
   
  #=====Analysis of WORDS TABLE=====#
  #Analyzing words within the comments#
  #With help from http://uc-r.github.io/creating-text-features#
  analysisofwordstable <- reactive({
    
    add_stopw <- as.data.frame(c("http","https","t.co","amp", "rt", "www.youtube.com","watch","video","youtube","videos"), stringsAsFactors = FALSE)
    colnames(add_stopw) <- "word"
    
    udmodel <- udpipe_download_model(language = "english")
    udmodel<- udpipe_load_model(file = udmodel)
    
    ytdata <- as.data.frame(comments())
    
    partsofspeech <- as_tibble(ytdata) %>%
      tidytext::unnest_tokens(word, `comment`) %>%
      anti_join(stop_words) %>%
      anti_join(add_stopw) %>%
      filter(
        !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
        !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
        !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
        !str_detect(word, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
        !str_detect(word, pattern = "^http:\\/\\/.*"),    # removes http
        !str_detect(word, pattern = "^https:\\/\\/.*")    # removes https
      ) %>%
      dplyr::count(word, sort = TRUE)%>%
      distinct() %>%
      group_by(word) %>%
      summarize(n = first(n)) %>%
      arrange(desc(n))%>%
      head(200)
    
    x <- udpipe_annotate(udmodel, x = partsofspeech$word, doc_id = partsofspeech$n)
    x <- as.data.frame(x)
    poswords <- as.data.frame(x)
    
    doc_id<-as.numeric(poswords$doc_id)
    
    #poswords <- x[, c("doc_id","lemma","upos")]
    
    Word <- poswords$lemma
    WordType <- poswords$upos
    NumberOfTimesUsed <- doc_id
    
    wordsList <- cbind.data.frame(Word,WordType,NumberOfTimesUsed)  
    
  })
  
  output$analysisofwordstable <-DT::renderDataTable({
    analysisofwordstable()
    
    
  })
  
  #Download CSV of Analysis of Words
  output$analysisofwordstableCSV <- downloadHandler(
    filename = function() {
      paste("analysisofwordstable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(analysisofwordstable(), file)
    }
  )
  
  
  #====INFORMATIVE WORDS TABLE====#
  #Combining two informative words from the video#
  #With help from http://uc-r.github.io/creating-text-features#
  informativewords <- reactive ({
    
    ytdata <- as.data.frame(comments())
    
    ngram_list <- ytdata %>%
      unnest_tokens(informativewords, `comment`, token = "ngrams", n = 2) %>%  
      separate(informativewords, c("word1", "word2"), sep = " ") %>%
      filter(
        !word1 %in% stop_words$word,                # remove stopwords from both words in bi-gram
        !word2 %in% stop_words$word,
        !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
        !str_detect(word2, pattern = "[[:digit:]]"),
        !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
        !str_detect(word2, pattern = "[[:punct:]]"),
        !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
        !str_detect(word2, pattern = "(.)\\1{2,}"),
        !str_detect(word1, pattern = "\\b(.)\\b"),   # removes any remaining single letter words
        !str_detect(word2, pattern = "\\b(.)\\b"),
        !str_detect(word1, pattern = "^http:\\/\\/.*"),
        !str_detect(word2, pattern = "^http:\\/\\/.*"),
        !str_detect(word1, pattern = "^https:\\/\\/.*"),
        !str_detect(word2, pattern = "^https:\\/\\/.*")
      )%>%
      count(word1,word2, sort = TRUE) %>%
      distinct() %>%
      arrange(desc(n))
    
    # compute counts for word 1 & 2 independently
    count_w1 <- ngram_list %>%
      count(word1)
    
    count_w2 <- ngram_list %>%
      count(word2)
    
    # compute counts for bi-grams
    informativewordslist <- ngram_list %>%
      arrange(desc(n))%>%
      select(word1, word2) %>%
      unite(informativewords, word1, word2, sep = " ")%>%
      head(5, informativewords)
    
    
    
  })
  
  output$informativewordstable <-DT::renderDataTable({
    informativewords()
    
    
  })
  
  #Download CSV of Informative Words
  output$informativewordsCSV <- downloadHandler(
    filename = function() {
      paste("informativewords-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(informativewords(), file)
    }
  )

#=====================PLOTS========================================#
    
  #=====OVERALL LIKES VS REPLIES PLOT======
  likesversusreplies <- reactive({
   
    videonam <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    videona <- as.character(videonam)
    #cat(videona)
    
    likeCount <- comments()$likes
    Likes <- as.numeric(likeCount)
    
    repcount <-comments()$numberofreplies
    Replies <- as.numeric(repcount)
    
   ggplot(yt_data(), aes(x=Likes, y = Replies, label = Comment)) +
     geom_point(aes(color = Likes)) +
     geom_smooth(method="auto", se=FALSE) +
     coord_cartesian() +
     scale_colour_gradient(low = "blue", high = "red") +
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=14,face="bold")) + 
      labs(x="Like Count", 
           y="Reply Count", 
           title="Overall Comments Based on Replies and Likes",subtitle=(videona))
   
  })
  
  output$likesversusreplies <-renderPlotly({
    likesversusreplies()
  })
  
  output$info <- renderUI({
    paste0("x=", input$plot_hover$x, "\ny=", input$plot_click$y)
  })
  
  #Top Comment by Most Likes
  number1comment <-reactive({
    SortedCommentList <- comments()[order (-comments()$likes),]
    num1comment <- head(SortedCommentList$comment, n=1)
    cat(num1comment)
  })
  
  output$number1comment <-renderPrint({
    number1comment()
    
  })
  
  #Likes vs Replies PLOT Download as PNG
  
  output$downloadPlot1<-downloadHandler(
    filename = function() {
      paste(input$commentkey,'likesversusreplies',Sys.Date(), '.png', sep='')
    },
    content=function(file){
      png(file, width = 1280, height = 720)
      print(likesversusreplies())
      dev.off()
    },
    contentType='image/png'
  )
  

#======Top 10 LIKES VS REPLIES PLOT=======#
  toptenlvrgraph <- reactive({
   
    videonam <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    videona <- as.character(videonam)
    cat(videona)
    
    tops<-comments()[order(comments()$likes,decreasing = T)[1:10],]
    
    ggplot(tops, aes(x=likes, y = numberofreplies, label = comment)) +
      geom_point(aes(color = likes)) +
      geom_smooth(method="auto", se=FALSE) +
      coord_cartesian() +
      scale_colour_gradient(low = "blue", high = "red") +
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=14,face="bold")) + 
      labs(x="Like Count", 
           y="Reply Count", 
           title="Top 10 Comments Based on Likes Vs. Replies",subtitle=(videona))
    
  })
  
  output$toptenlvrgraph <-renderPlotly({
    toptenlvrgraph()
    
  })
  
  #Top 10 Likes vs Replies PLOT Download as PNG
  
  output$downloadPlot2<-downloadHandler(
    filename = function() {
      paste(input$commentkey,'toptenlikesvsreplies',Sys.Date(), '.png', sep='')
    },
    content=function(file){
      png(file, width = 1280, height = 720)
      print(toptenlvrgraph())
      dev.off()
    },
    contentType='image/png'
  )

  
  #======Top 25 Most USED WORDS BAR CHART PLOT=====
  #Words calculated from the comments#
  #With help from https://www.cspoerlein.com/files/textanalyse.html#word_and_text_similarity#
  
  mostusedwords <- reactive({
    
    add_stopw <- as.data.frame(c("http","https","t.co","amp", "rt", "www.youtube.com","watch","video","videos","love","list","youtube","lol","guy", "lmao", "pretty"), stringsAsFactors = FALSE)
    colnames(add_stopw) <- "word"
    
    videonam <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    videona <- as.character(videonam)
    cat(videona)
    
    ytdata <- as.data.frame(comments())
    
    topmostwords <- as_tibble(ytdata) %>%
      tidytext::unnest_tokens(word, `comment`) %>%
      anti_join(stop_words) %>%
      anti_join(add_stopw) %>%
      filter(
        !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
        !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
        !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
        !str_detect(word, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
        !str_detect(word, pattern = "^http:\\/\\/.*"),    # removes http
        !str_detect(word, pattern = "^https:\\/\\/.*")    # removes https
      ) %>%
      dplyr::count(word, sort = TRUE) %>%
      distinct() %>%
      mutate(word_lemma = textstem::lemmatize_words(word)) %>%
     # mutate(word = if_else(n < 10, "infrequent", word)) %>% 
      group_by(word) %>%
      summarize(n = first(n)) %>%
      arrange(desc(n))%>%
      head(25,word)
    
    topwordcount <- topmostwords %>% 
      mutate(word=reorder(word,n))
    
    yttable <- ggplot(topwordcount, aes(x=word, y=n, fill=word)) +
      geom_col() + 
      coord_flip() +
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
    
    yttable + labs(x = "Words", y = "No. of Times Word Used", title = "Most Frequently Used Words",subtitle=(videona))
    
    #ggplotly(yttable)
    
  })
  
  output$mostusedwords <-renderPlotly({
    mostusedwords()
  })
  
  #DOWNLOAD MOST USED WORDS PLOT as PNG
  
  output$downloadPlot3<-downloadHandler(
    filename = function() {
      paste(input$commentkey,'top25mostusedwords',Sys.Date(), '.png', sep='')
    },
    content=function(file){
      png(file, width = 1280, height = 720)
      print(mostusedwords())
      dev.off()
    },
    contentType='image/png'
  )
  
  #======sentimentality of Emotions PLOT=====#
  #Identifying which sentiment/emotions are in the comments===#
  #'The comments are processed through the NRC emotion lexicon and tallied against the sentiment score#
  #'The score is output in a plot with the emotions#
  #With help from 
  #http://rstudio-pubs-static.s3.amazonaws.com/568201_5bc98e9113c9426c8000c1f395db9183.html#
  sentimentalplot <- reactive({
    
    comments <- iconv(comments()$comment, to = "utf-8")
    sentiment_score <- get_nrc_sentiment(comments)
    sentiment_score$neutral <-ifelse(sentiment_score$positive+sentiment_score$negative ==0, 1, 0)
    
    keeps <- c("anger","anticipation","disgust","fear","joy","sadness", "surprise", "trust")
    sentiment_score<-sentiment_score[keeps]
    
    Sentscore1<-100 * colSums(sentiment_score)/sum(sentiment_score)
    Sentscore<-as.data.frame(Sentscore1)
    
    data <- tibble(
      Sentiments=c("Anger","Anticipation","Disgust","Fear","Joy","Sadness", "Surprise", "Trust"),  
      Percentage = Sentscore$Sentscore
    )
    
    #Barplot
    sentimenttable <- ggplot(data, aes(x=Sentiments, y=Percentage, fill = Sentiments)) + 
      geom_bar(stat = "identity")+ 
      coord_flip()+
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))+ 
      labs(x = "Sentiments", y = "Frequency of Sentiments (Percentage)", title = "Sentimentality in YouTube Comments")
    sentimenttable
    
    
  })
  
  output$sentimentalplot <-renderPlot({
    sentimentalplot()
    
  })
  
  #DOWNLOAD SENTIMENT PLOT as PNG
  
  output$downloadPlot5<-downloadHandler(
    filename = function() {
      paste(input$commentkey,'sentimentalplot',Sys.Date(), '.png', sep='')
    },
    content=function(file){
      png(file, width = 1280, height = 720)
      print(sentimentalplot())
      dev.off()
    },
    contentType='image/png'
  )
  
  
  #======VIEW POSITIVE NEGATIVE SENTIMENT WORDS PLOT=====#
  #Identifying which words have a positive or negative connotation===#
  #With help from https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html#
  positivenegativeplot <- reactive({
    
    add_stopw <- data.frame(c("http","https","t.co","amp", "rt","www.youtube.com","watch","video","videos","love","list","youtube","lol","guy", "lmao", "pretty"), stringsAsFactors = FALSE)
    colnames(add_stopw) <- "word"
    
    videonam <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    videona <- as.character(videonam)
    cat(videona)
    
    ytdata <- data.frame(yt_data())
    
    sentimentwordslist <- as_tibble(ytdata) %>%
      tidytext::unnest_tokens(word, `Comment`) %>%
      anti_join(stop_words) %>%
      anti_join(add_stopw) %>%
      filter(
        !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
        !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
        !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
        !str_detect(word, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
        !str_detect(word, pattern = "^http:\\/\\/.*"),    # removes http
        !str_detect(word, pattern = "^https:\\/\\/.*")    # removes https
      ) %>%
      dplyr::count(word, sort = TRUE) %>%
      distinct() %>%
      mutate(word_lemma = textstem::lemmatize_words(word)) %>%
      group_by(word) %>%
      summarize(n = first(n)) %>%
      arrange(desc(n))
    
    bing <<- get_sentiments("bing")
    
    posnegwords <- sentimentwordslist %>%
      inner_join(bing) %>%
      mutate(word = reorder(word, n))%>%
      head(40)
      
    
    pos_neg_graph <- ggplot(posnegwords,aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales ="free_y")+
      coord_flip()+
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.title=element_text(size=14,face="bold")
      )+
      labs(x = "Words", y = "Sentimental Frequency", title = "Sentiment analysis of Positive and Negative Words",subtitle=(videona))
    
      pos_neg_graph
  })
  
  output$positivenegativeplot <-renderPlotly({
    positivenegativeplot()
  })
  
  #DOWNLOAD POSITIVE NEGATIVE PLOT as PNG
  
  output$downloadPlot4<-downloadHandler(
    filename = function() {
      paste(input$commentkey,'positivenegativewords',Sys.Date(), '.png', sep='')
    },
    content=function(file){
      png(file, width = 1280, height = 720)
      print(positivenegativeplot())
      dev.off()
    },
    contentType='image/png'
  )
  
  
#==================================STORED DATA=========================================================#
  
#Extracting Comments using saveData#
  commentsonly<-reactive({
    Comment <-(yt_data()$Comment)
    Video <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    categories <- data.frame(id,title)
    CategoryID<-(get_video_details(input$commentkey)$items[[1]]$snippet$categoryId)
    CategoryID<-as.numeric(CategoryID)
    ca<-categories[which(CategoryID == categories$id),]
    ytcategory <- data.frame(ca)
    Category<- ytcategory$title
    
    storedcomments<-cbind.data.frame(Video,Category,Comment)
    
    add_stopw <<- as.data.frame(c("http","https","t.co","amp", "rt", "www.youtube.com","watch","video","videos","love","list","youtube","lol","guy", "lmao", "pretty","cute"), stringsAsFactors = FALSE)
    colnames(add_stopw) <- "word"
    
    words <- as_tibble(storedcomments) %>%
      tidytext::unnest_tokens(word, `Comment`) %>%
      anti_join(stop_words) %>%
      anti_join(add_stopw) %>%
      filter(
        !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
        !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
        !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
        !str_detect(word, pattern = "\\b(.)\\b"),    # removes any remaining single letter words
        !str_detect(word, pattern = "^http:\\/\\/.*"),    # removes http
        !str_detect(word, pattern = "^https:\\/\\/.*")    # removes https
      ) %>%
      group_by(word,Category,Video) %>%
      count(word, sort = TRUE) %>%
      distinct() %>%
      mutate(word = if_else(n < 2, "", word))%>%
      summarize(n = first(n)) %>%
      arrange(desc(n))
    
    storeddata<-cbind.data.frame(words)
    
    storeddata
  })
  
#Extracting Video Statistics using saveViewCountData#
  viewcountdata<-reactive({
    videoname <- (get_video_details(input$commentkey)$items[[1]]$snippet$title)
    videocommentcount<-(get_stats(input$commentkey)$commentCount)
    uploaddate <- (get_video_details(input$commentkey)$items[[1]]$snippet$publishedAt)
    uploaddate <- ymd_hms(uploaddate)
    vidviewcount <- (get_stats(input$commentkey)$viewCount)
    vidlikecount <- (get_stats(input$commentkey)$likeCount)
    viddislikecount <- (get_stats(input$commentkey)$dislikeCount)
    categories <- data.frame(id,title)
    CategoryID<-(get_video_details(input$commentkey)$items[[1]]$snippet$categoryId)
    CategoryID<-as.numeric(CategoryID)
    ca<-categories[which(CategoryID == categories$id),]
    ytcategory <- data.frame(ca)
    ytcategorytitle<- ytcategory$title
    storedstatsdata<-cbind.data.frame(videoname,uploaddate,vidviewcount,vidlikecount,viddislikecount,videocommentcount,ytcategorytitle)
  })
  
  
  
#==============================STORED DATA TABLES AND PLOTS===========================================================#
  
#=====Stored Data Table=====#
  
  output$overallstoreddata <-DT::renderDataTable({
    saveData(commentsonly())
    data <-loadData()[!duplicated(loadData()), ]
    datatable(data,rownames= FALSE)
    
  })
  
  #Download Stored Data Comments
  output$storeddataCSV<-downloadHandler(
    filename = function() {
      paste("storeddataCSV-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadData(), file)
    }
  )
  
  
#======WORDCLOUD of Stored Data=====#
  #Using the combined stored comment data to make a word cloud===#
  #With help from 
  #http://rstudio-pubs-static.s3.amazonaws.com/568201_5bc98e9113c9426c8000c1f395db9183.html#
  
  
  storeddatawordcloud <- reactive({
    req(input$loaddatabuttonwordcloud)
    ytdata<-data.frame(loadData())
    ytdata<- na.omit(ytdata)
    ytcolnames <- c("Number","Word","Category","Video","NumberofTimesUsed")
    keeps <- c("Word","NumberofTimesUsed")
    words<-ytdata[keeps]
    
    words1<- words%>%
      filter(NumberofTimesUsed >= 10) %>% 
      arrange(desc(NumberofTimesUsed))
    
    words1<-data.frame(words1)
    storeddatawordcloud<- wordcloud2(words1,2)
    storeddatawordcloud
    
  })
  
  output$storeddatawords <- renderWordcloud2({
    storeddatawordcloud()
  })
  
  #Download Stored word cloud
  output$storedwordcloudplot<-downloadHandler(
    filename = function() {
      paste('storedwordcloudplot',Sys.Date(), 'png', sep='.')
    },
    content=function(file){
      hw<-storeddatawordcloud()
      saveWidget(hw,"storedwordcloudplot.html",selfcontained = F)
      webshot::webshot("storedwordcloudplot.html",file,vwidth = 1280, vheight = 720, delay =5)
    }
  )
  
#=====Video Details Stored Data=====#
  
  output$videodetailsstoreddata <-DT::renderDataTable({
    saveViewDetailsData(viewcountdata())
    data <-loadViewDetailsData()[!duplicated(loadViewDetailsData()), ]
      datatable(data,rownames= FALSE)
  })
  
  #Download Stored Data Video Statistics Table 
  output$storedvideostatisticsCSV<-downloadHandler(
    filename = function() {
      paste("storedvideostatisticsCSV-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadViewDetailsData(), file)
    }
  )
  
#====== Plots of Video Stats of Stored Data=====#
  #Using the combined stored comment data to make most viewed videos chart===#
  #Stored Comment Data is shown in graphs by Views/Likes/Dislikes/Comments 
  #With help from 
  #https://rpubs.com/arafath/YouTubeVideos_Analysis#
  
  
  output$mostviewedvideos <- renderPlotly({
    req(input$loaddatabuttonmostviewed)
    data<-as_tibble(loadViewDetailsData())
    
    if(input$vidstats=='Views'){
      mostviewedvideos<- data %>% 
        mutate(Video = strtrim(Video, 25)) %>%
        mutate(Video = reorder(Video,Views)) %>% 
        ggplot(aes(Video, Views)) + 
        scale_y_continuous(name="Views (in Millions)", labels = scales::comma)+
        geom_bar(stat="identity", fill="steelblue")+
        coord_flip() +
        theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
              plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14),
              axis.title=element_text(size=14,face="bold"))+ 
        labs(x = "Video", y = "Number of views (in Millions)", title = "Most Viewed Videos")
      
      mostviewedvideos
    } else if(input$vidstats=='Likes'){
      mostlikedvideos<- data %>% 
        mutate(Video = strtrim(Video, 25)) %>%
        mutate(Video = reorder(Video,Likes)) %>% 
        ggplot(aes(Video, Likes)) + 
        scale_y_continuous(name="Likes", labels = scales::comma)+
        geom_bar(stat="identity", fill="steelblue")+
        theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
              plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14,angle=45,hjust=1),
              axis.title=element_text(size=14,face="bold"))+ 
        labs(x = "Video", y = "Number of Likes", title = "Most Liked Videos")
      
      mostlikedvideos
    } else if(input$vidstats=='Dislikes'){
      mostdislikedvideos<- data %>% 
        mutate(Video = strtrim(Video, 25)) %>%
        mutate(Video = reorder(Video,Dislikes)) %>% 
        ggplot(aes(Video, Dislikes)) + 
        scale_y_continuous(name="Dislikes", labels = scales::comma)+
        geom_bar(stat="identity", fill="steelblue")+
        theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
              plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14,angle=45,hjust=1),
              axis.title=element_text(size=14,face="bold"))+ 
        labs(x = "Video", y = "Number of Dislikes", title = "Most Disliked Videos")
      
      mostdislikedvideos
    }else if(input$vidstats=='Number of Comments'){
      mostcommentedvideos<- data %>% 
        mutate(Video = strtrim(Video, 25)) %>%
        mutate(Video = reorder(Video,NumberofComments)) %>% 
        ggplot(aes(Video, NumberofComments)) + 
        scale_y_continuous(name="Number of Comments", labels = scales::comma)+
        geom_bar(stat="identity", fill="steelblue")+
        theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
              plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14,angle=45,hjust=1),
              axis.title=element_text(size=14,face="bold"))+ 
        labs(x = "Video", y = "Number of Comments", title = "Most Commented Videos")
      
      mostcommentedvideos
    }
    
    
  })
  
  #Download Stored Data Plots and Graphs
  output$storedplotsandgraphsplot<-downloadHandler(
    filename = function() {
      paste('storedplotsandgraphsplot',Sys.Date(), 'png', sep='.')
    },
    content=function(file){
      ggsave(file, width = 34, height = 20, units = "cm")
    }
  )
  
  #======Stored Data by Category=====#
  #Using the stored comment data to analyze by category===#
  #The data is combined by category and plots are used to assess most comments/views/likes/dislikes
  #word cloud plot is used to assess most frequently used words in the categories#
  
  #With help from 
  #https://www.cspoerlein.com/files/textanalyse.html#
  
  
  output$databycategory <- renderPlot({
    req(input$loaddatabuttoncategory)
    
    #stored data for word frequency graph by category
    data<- loadData()
    keeps <- c("Word","Category","NumberofTimesUsed")
    words<-data[keeps]
      
    words1<-words %>% 
      arrange(desc(NumberofTimesUsed))%>% 
      group_by(Category) %>% 
      head(300)
    
    #stored data for other categories - views/comments/likes/dislikes
    data_viewdetails<-as_tibble(loadViewDetailsData())
    
    if(input$vidcategory=='Word Frequency'){
    categoryplot<- 
             ggplot(words1%>% filter(NumberofTimesUsed>30), aes(label = Word,color=NumberofTimesUsed)) +
      geom_text_wordcloud(show.legend = TRUE,rm_outside = TRUE,area_corr = TRUE) +
      scale_size_area(max_size = 10) +
      theme_minimal() +
      theme(plot.title=element_text(size=20,face="bold",hjust = 0.5),
            legend.title=element_text(size=20,face="bold",hjust = 0.5),
            text=element_text(size=20,face="bold",hjust = 0.5))+ 
      labs(title = "Words of Stored Data based on Categories",color='Word Frequency')+
      scale_color_gradientn(colours = rainbow(6))+
      facet_wrap(~Category)
    
    categoryplot
    } else if(input$vidcategory=='Views'){
      categoryviewsplot<- data_viewdetails %>%
        ggplot(aes(Category, Views)) + 
        scale_y_continuous(name="Views (in Millions)", labels = scales::comma)+
        geom_bar(stat="identity", fill="steelblue")+
        theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
              plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14),
              axis.title=element_text(size=14,face="bold"))+ 
        labs(x = "Categories", y = "Views (in Millions)", title = "Most Popular Categories Based on Views")
      
      categoryviewsplot
      
      }else if(input$vidcategory=='Likes'){
        categorylikesplot<- data_viewdetails %>%
          ggplot(aes(Category, Likes)) + 
          scale_y_continuous(name="Likes", labels = scales::comma)+
          geom_bar(stat="identity", fill="steelblue")+
          theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
                plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
                axis.text.y=element_text(size=14),
                axis.text.x=element_text(size=14),
                axis.title=element_text(size=14,face="bold"))+ 
          labs(x = "Categories", y = "Likes", title = "Most Popular Categories Based on Likes")
        
        categorylikesplot
      }else if(input$vidcategory=='Dislikes'){
    categorydislikesplot<- data_viewdetails %>%
      ggplot(aes(Category, Dislikes)) + 
      scale_y_continuous(name="Dislikes", labels = scales::comma)+
      geom_bar(stat="identity", fill="steelblue")+
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))+ 
      labs(x = "Categories", y = "Dislikes", title = "Most Popular Categories Based on Dislikes")
    
    categorydislikesplot
      }else if(input$vidcategory=='Comments'){
    categorycommentsplot<- data_viewdetails %>%
      ggplot(aes(Category, NumberofComments)) +
      scale_y_continuous(name="Comments", labels = scales::comma)+
      geom_bar(stat="identity", fill="steelblue")+
      theme(plot.title=element_text(size=16,face="bold",hjust = 0.5),
            plot.subtitle=element_text(size=14,face="bold",hjust = 0.5),
            axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))+ 
      labs(x = "Categories", y = "Comments", title = "Most Popular Categories Based on Comments")
    
    categorycommentsplot
      }
  })
  
  #Download Stored Categories Plots
  output$storedcategoriesplot<-downloadHandler(
    filename = function() {
      paste('storedcategoriesplot',Sys.Date(), 'png', sep='.')
    },
    content=function(file){
      ggsave(file, width = 34, height = 20, units = "cm")
    }
  )
  
}

shinyApp(ui, server)
