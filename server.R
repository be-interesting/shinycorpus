library(shiny)
library(readr)
library(RTextTools)
library(corpustools)
library(semnet)
library(DT)
source('lib.r')

shinyServer(function(input, output, session) {
  d <- reactiveValues(tokens = NULL, meta=NULL, text=NULL, termstats=NULL) ## global values
  
  ## raw input: update fields based on file information
  observeEvent(input$raw,
  {
    rawcols = colnames(read.csv(input$raw$datapath, nrows=1))
    updateSelectInput(session, 'raw_docid_col', choices = rawcols)
    updateSelectInput(session, 'raw_text_col', choices = rawcols)
    
    # try defaults
    if('id' %in% rawcols) updateSelectInput(session, 'raw_docid_col', selected = 'id')
    text_cols = c()
    if('headline' %in% rawcols) text_cols = c(text_cols, 'headline')
    if('text' %in% rawcols) text_cols = c(text_cols, 'text')
    updateSelectInput(session, 'raw_text_col', selected = text_cols)
  })
  
  ## tokens input: update fields based on file information
  observeEvent(input$tokens,
   {
     tokenscols = colnames(read.csv(input$tokens$datapath, nrows=1))
     updateSelectInput(session, 'tokens_docid_col', choices = tokenscols)
     updateSelectInput(session, 'tokens_text_col', choices = tokenscols)
     
     # try defaults
     if('id' %in% tokenscols) updateSelectInput(session, 'tokens_docid_col', selected = 'id')
     if('word' %in% tokenscols) updateSelectInput(session, 'tokens_text_col', selected = 'word')
     if('lemma' %in% tokenscols) updateSelectInput(session, 'tokens_text_col', selected = 'lemma')
   })
  
  ## meta input: update fields based on file information
  observeEvent(input$meta,
   {
     metacols = colnames(read.csv(input$meta$datapath, nrows=1))
     updateSelectInput(session, 'meta_docid_col', choices = metacols)
     if('id' %in% metacols) updateSelectInput(session, 'meta_docid_col', selected = 'id')
   })
  
  observeEvent(input$load,
  {
    if(is.null(input$raw) & is.null(input$tokens)) return(NULL)
    if(input$datatype == 'raw'){
      dm = readr::read_csv(input$raw$datapath)
      d$text = do.call(paste, c(as.list(dm[,input$text_col, drop=F]), sep='\n\n'))
      d$meta = dm[,!colnames(dm) %in% input$text_col, drop=F]}
    if(input$datatype == 'tokens'){
      d$tokens = readr::read_csv(input$tokens$datapath)
      
      updateSelectInput(session, 'poscolumn', choices = colnames(d$tokens))
      if('pos1' %in% colnames(d$tokens)) updateSelectInput(session, 'poscolumn', selected = 'pos1')
      if(!is.null(input$meta)) d$meta = readr::read_csv(input$meta$datapath) ## meta is optional
    }
    n = nrow(d$meta)  
    updateSliderInput(session, 'docfreq', min=1, max=n, value=c(1,n))
  })
  
  observeEvent(input$poscolumn, {
    if(input$poscolumn == '') return(NULL)
    postags = unique(d$tokens[,input$poscolumn])
    if(length(postags) > 50) {
      output$poswarning = renderText('<div style="color:#FF0000">Too many unique values</div>')
      updateSelectInput(session, 'poscolumn', selected = '')
      updateSelectInput(session, 'posfilter', selected = '')
    } else {
      updateSelectInput(session, 'posfilter', choices = postags, selected = postags)
    }
  })
  
  ## reactive tokenstats, to be used in getTokens, so that they only need to be calculated for new tokenlists. 
  getTokenstats <- reactive({
    if(is.null(d$tokens)) return(NULL)
    dt = d$tokens[,c(input$tokens_docid_col, input$tokens_text_col)]
    dt[,input$tokens_text_col] = tolower(dt[,input$tokens_text_col])
    dt = unique(dt) # one term occurence per document
    docfreq = table(dt[,input$tokens_text_col])
    tstats = data.frame(term=as.character(names(docfreq)), docfreq=as.numeric(docfreq), stringsAsFactors = F)
    tstats$wordlen = nchar(tstats$term)
    tstats
  })
  
  ## reactive tokenlist. Returns the tokenlist after filtering, Should be used instead of d$tokens (which is the raw data). 
  getTokens <- reactive({
    if(is.null(d$tokens)) return(NULL)
    
    tstats = getTokenstats()
    tstats$filter = tstats$docfreq >= input$docfreq[1] & tstats$docfreq <= input$docfreq[2] &
             tstats$wordlen >= input$wordlength[1] & tstats$wordlen <= input$wordlength[2]
    if('numbers' %in% input$filters) tstats$filter = tstats$filter & grepl('[a-zA-Z]', tstats$term)
    if('punctuation' %in% input$filters) tstats$filter = tstats$filter & !grepl('[.,();:"!?@]', tstats$term)
    
    tokens = d$tokens
    if(!input$poscolumn == '') tokens = tokens[tokens[,input$poscolumn] %in% input$posfilter,]
    levels(tokens) = tolower(levels(tokens))
    tokens[tokens[,input$tokens_text_col] %in% tstats$term[tstats$filter],]
  })
  
  ## creates a reactive DTM, meaning that it the DTM will only be updated if d$text of one of the input values changes. Without changes, getDTM will get the dtm from memory
  getDTM <- reactive({
    if(is.null(d$text) & is.null(d$tokens)) return(NULL)
    
    if(input$datatype == 'raw'){
      dtm = RTextTools::create_matrix(
              d$text, 
              language = input$language, 
              minDocFreq = input$docfreq[1],
              maxDocFreq = input$docfreq[2],
              minWordLength = input$wordlength[1],
              maxWordLength = input$wordlength[2],
              removeNumbers = 'numbers' %in% input$filters,
              removePunctuation = 'punctuation' %in% input$filters,
              removeStopwords='stopwords' %in% input$filters,
              stemWords= 'stem' %in% input$filters,
              toLower= T)
    }
    if(input$datatype == 'tokens'){
      tokens = getTokens() # use reactive version of tokens
      dtm = corpustools::dtm.create(
              documents = tokens[,input$tokens_docid_col],
              terms = tokens[,input$tokens_text_col],
              freqs = rep(1, nrow(tokens)))
    }
    dtm
  })
  
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
    }, options = list(autoWidth = T), rownames=F)
  
  observeEvent(input$action,{
    dtm = getDTM()
    output$actionplot <- renderPlot({
      dtm.wordcloud(dtm, nterms=input$wordcloud_nterms, scale=input$wordcloud_range)
    })  
  })
  
})


