library(shiny)
library(plyr)
library(tuber)
library(purrr)
options(max.print=100000000)

ui <- fluidPage(
  title = "YT Comments",
  titlePanel("Ranking YouTube Comments"),
  sidebarLayout(
    sidebarPanel(
      p("Sort and Rank Comments of a YouTube Video"),
      helpText("The key for a YouTube video is the code at the end of a YouTube link as shown in", strong("bold"), "below"),
      p("www.youtube.com/watch?v=",strong("1SvZsXAMpqQ")),
      textInput(inputId = "commentkey",
                label = "Enter YouTube Comment Key:",
                value = ""),
    
      downloadButton("downloadCSV", "Download CSV"),
      br(),
      br(),
      
      strong("Video Details"),
      textOutput('commentsdetails')
    ),
    
    
    
    mainPanel(
        id = 'dataset',
        DT::dataTableOutput("commenttable")
    )
  )
)

server <- function(input, output) {
    
    output$commentsdetails <- renderText({ paste(get_video_details(input$commentkey))})
    
    output$commenttable <- DT::renderDataTable({
      validate(
        need(input$commentkey != '', 'Please enter YouTube Key...(some comments may take a long time to load)')
      )
      DT::datatable(get_all_comments(input$commentkey, src = "youtube", auto.assign = FALSE, colnames = c('Comment' = 2)),
       options = list(pageLength = 10,lengthMenu = list(c(5, 15, -1), list('5', '15', 'All'))))
    })
    
    output$downloadCSV <- downloadHandler(
      filename = function() {
        paste("YT.csv", sep = "")
      },
      content = function(file) {
        write.csv(get_all_comments(input$comments, src = "youtube", auto.assign = FALSE), file)
      }
    )
  
}

shinyApp(ui, server)
