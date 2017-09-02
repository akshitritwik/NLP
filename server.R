# server.R
library(shiny)

Trim <- function( x ) {
  # http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
http://127.0.0.1:37673/rstudio/clear.cache.gif
# load ngram data set
all_ngrams <- read.table('c:/cordova/pubmed_cardiology_ngrams.csv', sep=",", header=TRUE)
all_ngrams <- as.character(all_ngrams$x)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    find_next_word(tolower(input$current_sentence))
  })
  
  # You can access the value of the widget with input$text, e.g.
  output$value <- renderPrint({ paste(tolower(input$text), find_next_word(tolower(input$text))) })
  
  find_next_word <- function(current_sentence) { 
    if (nchar(Trim(current_sentence)) == 0)
      return ('')
    
    # find the best next word
    # trailing space at end to avoid picking last word
    matches <- c()
    current_sentence <- paste0(Trim(current_sentence)," ")
    for (sentence in all_ngrams) {
      # find exact match with double backslash and escape
      if (grepl(paste0('\\<',current_sentence), sentence)) {
        matches <- c(matches, sentence)
      }
    }
    
    # didn't find a match so return nothing
    if (is.null(matches))
      return ('')
    
    # find highest probability word
    precision_match <- c()
    for (a_match in matches) {
      # how many spaces in from of search word
      precision_match <- c(precision_match,nchar(strsplit(x = a_match, split = word)[[1]][[1]]))
    }
    
    # use highest number and a random of highest for multiples
    best_matched_sentence <- sample(matches[precision_match == max(precision_match)],size = 1)
    # split the best matching sentence by the search word
    best_match <- strsplit(x = best_matched_sentence, split = current_sentence)[[1]]
    # split second part by spaces and pick first word
    best_match <-  strsplit(x = best_match[[2]], split = " ")[[1]]
    # return first word
    return (best_match[[1]]) 
  }
})
