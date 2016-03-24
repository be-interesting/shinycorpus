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

  ## update action parameters
  observeEvent(input$load, 
               updateWorddistanceParameters(session, input, d))
  
  ## prepare corpus (DTM, tokens, meta)
  getTokens <- reactive(filterTokens(d, input)) # reactive tokenlist. Returns the tokenlist after filtering, Should be used instead of d$tokens (which is the raw data). 
  getDTM <- reactive(createDTM(d, input, getTokens())) # creates a reactive DTM, meaning that it the DTM will only be updated if d$text of one of the input values changes. Without changes, getDTM will get the dtm from memory
  metaDTM <- reactive(matchMeta(d, input, getDTM())) # meta matched to DTM
  

  ## output
  output$meta <- renderDataTable({
    d$meta
    }, options = list(autoWidth = F, scrollX=T, columnDefs = list(list(
      targets = '_all',
      render = JS("function(data, type, row, meta) {", "return type === 'display' && data.length > 20 ?",
                  "'<span style=\"color:blue\" title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;", "}")
    ))), rownames=F, callback = JS('table.page(3).draw(false);')) ## abbreviate long texts and show full text on hover
  
  output$tokens <- renderDataTable({
    getTokens()
  }, options = list(autoWidth = F, scrollX=T), rownames=F)
  
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
  

  ## By using recordplot() (for R device plots) within a reactive, plotting parameters can be modified without having to reload the plot
  observe({
    p = actionPlot()
    input$plotres ## does not trigger observe with renderPlot call
    output$actionplot = renderPlot(p, res= input$plotres * (input$plotzoom/100),
                                      width=input$plotwidth * (input$plotzoom/100),
                                      height=input$plotheight * (input$plotzoom/100))
  })
  
  getActiondata <- eventReactive(input$plotaction, {
    actiondata = list()
    if(input$actions == 'wordcloud') actiondata = c(actiondata, prepareWordcloud(input, getDTM()))
    if(input$actions == 'semnet') actiondata = c(actiondata, prepareSemnet(input, getDTM(), getTokens()))
    if(input$actions == 'compare') actiondata = c(actiondata, plotCompare(input, getDTM(), getTokens()))
    
    output$actionmessage = if('message' %in% names(actiondata)) renderText(actiondata$message) else ''
    actiondata
  })
  
  actionPlot <- eventReactive(input$plotaction, {
    actiondata = getActiondata()
    par(mar=c(0,0,0,0))
    if(input$actions == 'wordcloud') plotWordcloud(actiondata)
    if(input$actions == 'semnet') plotSemnet(actiondata)
    if(input$actions == 'compare') plotCompare(actiondata)
    return(recordPlot())
  })
  
  actionRawData <- eventReactive(input$plotaction, {
    actiondata = getActiondata()
    if(input$actions == 'wordcloud') return(actiondata$termfreq)
    if(input$actions == 'semnet') return(get.data.frame(actiondata$g, 'edges'))
    if(input$actions == 'compare') return(actiondata$compare)
  })
})


