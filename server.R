library(shiny)
library(readr)
library(RTextTools)
library(corpustools)
library(semnet)
library(DT)
source('lib.r')

shinyServer(function(input, output, session) {
  ## update input parameters after selecting a file, using the column names to provide the options
  observeEvent(input$text, 
               updatetextInputParameters(session, input, try_defaults = T))
  observeEvent(input$tokens, 
               updateTokenInputParameters(session, input, try_defaults = T))
  observeEvent(input$meta, 
               updateMetaInputParameters(session, input, try_defaults = T))
  
  ## load data
  observeEvent(input$load, 
               loadData(session, input, d))  
  
  
  ## update corpus parameters
  observeEvent(d$tokens, 
               updatePosFilter(session, d))
  observeEvent(d$meta,
               updateDocfreqFilter(session, d))

  ## prepare corpus (DTM, tokens, meta)
  getTokens <- reactive(filterTokens(d, input)) # reactive tokenlist. Returns the tokenlist after filtering, Should be used instead of d$tokens (which is the raw data). 
  getDTM <- reactive(createDTM(d, input, getTokens())) # creates a reactive DTM, meaning that it the DTM will only be updated if d$text of one of the input values changes. Without changes, getDTM will get the dtm from memory
  metaDTM <- reactive(matchMeta(d, input, getDTM())) # meta matched to DTM
  
  ## output
  output$meta <- renderDataTable({
    d$meta
    }, options = list(autoWidth = T, scrollX=T), rownames=F)
    
  output$tokens <- renderDataTable({
    getTokens()
  }, options = list(autoWidth = T, scrollX=T), rownames=F)
  
  
  output$dtmsummary <- renderText({
    dtm = getDTM()
    sprintf('<h4>Number of rows/documents: %s</h4>
             <h4>Number of columns/terms : %s</h4>
             <br>', nrow(dtm), ncol(dtm))
  })
  
  output$termstats <- renderDataTable({
    termstats = corpustools::term.statistics(getDTM())
    termstats[,c('term','characters','number','termfreq','docfreq','reldocfreq')]
    }, options = list(autoWidth = T, scrollX=T), rownames=F)
  
  
  observeEvent(input$action,{
    if(input$actions == 'wordcloud'){
      output$actionplot <- renderPlot(plotWordcloud(input, getDTM()), 
                                      width = 600, height = 600)  
    }
    if(input$actions == 'semnet'){
      output$actionplot <- renderPlot(plotSemnet(input, getDTM(), getTokens()), 
                                      width = 600, height = 600)
    }
    if(input$actions == 'compare'){
      output$actionplot <- renderPlot(plotCompare(input, getDTM(), getTokens()), 
                                      width = 600, height = 600)
    }
  })
  
})


