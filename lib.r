customStyle <- function(sidebarheight='90vh', inputcontainer_height='65px'){
  list(tags$head(tags$style(HTML(  ## add scroll to sidebar (because conditinalpanels mess up the height)
                            sprintf(".sidebar {height: %s; overflow-y: auto;}", sidebarheight)
                            ))),  
       tags$head(tags$style(HTML(  ## reduce height of input containers
                            sprintf('.shiny-input-container {height:%s;}', inputcontainer_height)
                            )))) 
}

## iterates through default_candidates to see if they occur in choices, and selects the candidate if it does. 
## breaks after match, so preferred defaults should be put first
selectInputDefault <- function(session, inputID, choices, default_candidates){
  for(default in default_candidates){
    if(default %in% choices) {
      updateSelectInput(session, inputID, selected = default)
      break
}}}

## similar to updateSelectDefault(), but if multiple candidates match the choices, they are all selected (in the given order)
selectInputDefaultMulti <- function(session, inputID, choices, default_candidates){
  defaults = c()
  for(default in default_candidates){
    if(default %in% choices) defaults = c(defaults, default)
  }
  if(length(defaults) > 0) updateSelectInput(session, inputID, selected = defaults)
}



#############################################################################################################
############################################### text input functions ######################################## 
## ui: textInputUI
## server: updatetextInputParameters; loadtextInput
updatetextInputParameters <- function(session, input, try_defaults=T){
  cols = colnames(read.csv(input$text$datapath, nrows=1))
  if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X
  
  updateSelectInput(session, 'text_docid_col', choices = cols)
  updateSelectInput(session, 'text_text_col', choices = cols)
  updateSelectInput(session, 'text_meta_col', choices = cols, selected = cols)
  
  if(try_defaults){
    selectInputDefault(session, 'text_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid', 'id'))
    selectInputDefaultMulti(session, 'text_text_col', cols, c('headline','byline','lead','abstract','text'))
  }
}

## function that adds user interface elements if the condition is TRUE.
textInputUI <- function(condition='true'){
  conditionalPanel(condition = condition,
                    fileInput('text', label = "Upload a csv file with text and meta", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                    selectInput('text_docid_col', label = 'Document id column', choices=c(), multiple = F),
                    selectInput('text_text_col', label = 'Text column(s)', choices=c(), multiple = T),
                    selectInput('text_meta_col', label = 'meta columns', choices=c(), multiple = T)
  )
}

############################################################################################################
########################################## token input functions ###########################################
## ui: tokensInputUI
## server: updateTokensInputParameters; loadTokensInput

updateTokenInputParameters <- function(session, input, try_defaults=T){
  cols = colnames(read.csv(input$tokens$datapath, nrows=1))
  if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X
  
  updateSelectInput(session, 'tokens_docid_col', choices = cols)
  updateSelectInput(session, 'tokens_text_col', choices = cols)
  updateSelectInput(session, 'tokens_position_col', choices = c(cols, '[not available]')) # optional
  updateSelectInput(session, 'tokens_pos_col', choices = c(cols, '[not available]')) # optional
  
  if(try_defaults){
    selectInputDefault(session, 'tokens_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid', 'id'))
    selectInputDefault(session, 'tokens_text_col', cols, c('lemma','word','term','text'))
    selectInputDefault(session, 'tokens_position_col', c(cols, '[not available]'), c('position','word_id','id','[not available]')) # optional
    selectInputDefault(session, 'tokens_pos_col', c(cols, '[not available]'), c('pos1','pos', '[not available]')) # optional
  }
}

## function that adds user interface elements if the condition is TRUE.
tokensInputUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    fileInput('tokens', label = "Upload a csv file with tokens", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    selectInput('tokens_docid_col', label = 'document.id', choices=c(), multiple = F),
    selectInput('tokens_text_col', label = 'text (word/lemma)', choices=c(), multiple = F),
    selectInput('tokens_position_col', label = 'text position (optional)', choices=c(), multiple = F),
    selectInput('tokens_pos_col', label = 'part-of-speech (optional)', choices=c(), multiple = F),
    br(),
    fileInput('meta', label = "Upload a csv file with meta", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    selectInput('meta_docid_col', label = 'document.id', choices=c(), multiple = F),
    selectInput('meta_meta_col', label = 'meta columns', choices=c(), multiple = T)
  )
}

############################################################################################################
########################################## meta input functions ############################################
## ui: metaInputUI
## server: updateMetaInputParameters; loadMetaInput

updateMetaInputParameters <- function(session, input, try_defaults=T){
    cols = colnames(read.csv(input$meta$datapath, nrows=1))
    if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X
    
    updateSelectInput(session, 'meta_docid_col', choices = cols)
    updateSelectInput(session, 'meta_meta_col', choices = cols, selected = cols)
    
    if(try_defaults){
      selectInputDefault(session, 'meta_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid', 'id'))
    }
}


############################################################################################################
########################################## load data functions ############################################
## ui: loadDataButton
## server: loadData

loadDataButton <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('load', 'load data', icon = icon('play-circle'), width = 200, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
}

loadData <- function(session, input, d){
  if(is.null(input$text) & is.null(input$tokens)) return(NULL)
  
  if(input$datatype == 'text'){
    d$tokens = NULL; d$tokenstats = NULL
    dm = readr::read_csv(input$text$datapath)
    d$text = do.call(paste, c(as.list(dm[,input$text_text_col, drop=F]), sep='\n\n'))
    d$meta = dm[,input$text_meta_col, drop=F]
    colnames(d$meta)[colnames(d$meta) == input$text_docid_col] = 'doc_id' 
  }
  
  if(input$datatype == 'tokens'){
    d$text = NULL
    tokens = readr::read_csv(input$tokens$datapath)
    d$tokens = data.frame(doc_id = tokens[,input$tokens_docid_col],
                          text = tokens[,input$tokens_text_col])
    if(!input$tokens_position_col == '[not available]') d$tokens$position = tokens[,input$tokens_position_col]
    if(!input$tokens_pos_col == '[not available]') d$tokens$pos = tokens[,input$tokens_pos_col]

    if(!is.null(input$meta)) {
      d$meta = readr::read_csv(input$meta$datapath) ## meta is optional
      d$meta = d$meta[,unique(c(input$meta_docid_col, input$meta_meta_col)), drop=F]
      colnames(d$meta)[colnames(d$meta) == input$meta_docid_col] = 'doc_id' 
    }
    
    d$tokenstats = calculateTokenStats(d)
  }
}

calculateTokenStats <- function(d){
  dt = d$tokens[, c('doc_id', 'text')]
  dt$text = tolower(dt$text)
  dt = unique(dt) # one term occurence per document
  docfreq = table(dt$text)
  tstats = data.frame(term=as.character(names(docfreq)), docfreq=as.numeric(docfreq), stringsAsFactors = F)
  tstats$wordlen = nchar(tstats$term)
  tstats
}

############################################################################################################
########################################## corpus parameters ###############################################
## ui: textCorpusParametersUI; tokensCorpusParametersUI; generalCorpusParametersUI
## server: updatePosFilter; updateDocfreqFilter

## ui's
textCorpusParametersUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    selectInput('language', 'Language', c('english','dutch','german','french','spanish'), multiple=F, selected='english'),
    selectInput('filters', 'filter/transform terms', 
                list('remove stopwords'='stopwords', 'remove numbers'='numbers','remove punctuation'='punctuation','stem words'='stem'), 
                multiple=T, selected=c('stopwords','numbers','punctuation','stem'))
  )
}



tokensCorpusParametersUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    selectInput('filters', 'filter/transform terms', 
                list('remove numbers'='numbers','remove punctuation'='punctuation'), 
                multiple=T, selected=c('numbers','punctuation')),
    htmlOutput('poswarning'),
    selectInput('posfilter', 'Part-of-speech filter', choices=c(), multiple = T)
  )
}

generalCorpusParametersUI <- function(){
  list(
    br(),
    sliderInput('docfreq', label = 'min/max document frequency', step=1, min = 5, max=10000, value=c(1,10000)),
    br(),
    sliderInput('wordlength', label = 'min/max word length', step=1, min=2, max=200, value=c(1, 200))
  )
}

## update filters
updatePosFilter <- function(session, d){
  updateSelectInput(session, 'posfilter', choices = c())
  if('pos' %in% colnames(d$tokens)){
    postags = unique(d$tokens$pos)
    if(length(postags) > 100) {
      output$poswarning = renderText('<div style="color:#FF0000">Too many unique POS values</div>')
    } else {
      updateSelectInput(session, 'posfilter', choices = postags, selected = postags)
    }
  }
}

updateDocfreqFilter <- function(session, d){
  n = nrow(d$meta)  
  updateSliderInput(session, 'docfreq', min=5, max=n, value=c(1,n))
}

############################################################################################################
########################################## preapre corpus ###############################################
## ui: 
## server: filterTokens; createDTM

filterTokens <- function(d, input){
  if(is.null(d$tokens)) return(NULL)
  
  tstats = d$tokenstats
  tstats$filter = tstats$docfreq >= input$docfreq[1] & tstats$docfreq <= input$docfreq[2] & 
    tstats$wordlen >= input$wordlength[1] & tstats$wordlen <= input$wordlength[2]
  if('numbers' %in% input$filters) tstats$filter = tstats$filter & grepl('[a-zA-Z]', tstats$term)
  if('punctuation' %in% input$filters) tstats$filter = tstats$filter & !grepl('[.,();:"!?@]', tstats$term)
  
  tokens = d$tokens
  if('pos' %in% colnames(d$tokens)) tokens = tokens[tokens$pos %in% input$posfilter,]
  levels(tokens$text) = tolower(levels(tokens$text)) # the terms in tstats are also lowercase
  tokens[tokens$text %in% tstats$term[tstats$filter],]
}

createDTM <- function(d, input, tokens){
  if(is.null(d$text) & is.null(tokens)) return(NULL)
  ## note that the tokens used here is the result of getTokens(), which gives the already filtered version of d$tokens. 
  ## d$text can be used directly because there is no preprocessing involved (this all happens below in RTextTools::create_matrix)
  
  if(input$datatype == 'text'){
    dtm = RTextTools::create_matrix(
      textColumns = d$text, 
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
    dtm = corpustools::dtm.create(
      documents = tokens$doc_id,
      terms = tokens$text,
      freqs = rep(1, nrow(tokens)))
  }
  dtm
}

matchMeta <- function(d, input, dtm){
  if(is.null(dtm)) return(NULL)
  d$meta[match(rownames(dtm), d$meta$doc_id),]
}


############################################################################################################
########################################## ACTIONS ###############################################
## ui: plotWordcloudUI; plotSemnetUI; PlotCompareUI
## server:

wordcloudParameters <- function(){
  list(
  sliderInput('wordcloud_nterms', 'Number of terms', min=1, max=200, value=100),
  sliderInput('wordcloud_range', 'Wordsize range', min=0.1, max=15, value=c(0.5, 8))
  )
}

plotWordcloudUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    h4('Create a wordcloud of the most frequent words in the DTM'),
    br(),
    wordcloudParameters()
  )
}

plotSemnetUI <- function(condition = 'true'){
  conditionalPanel(
    condition = condition,
    h4('Make a semantic network'),
    conditionalPanel(condition = 'input.datatype == "tokens" && input.tokens_position_col != "[not available]"',
                     checkboxInput('use_window', label = 'measure co-occurence within a given word distance', value = F)),
    conditionalPanel(condition = 'input.use_window == true && input.tokens_position_col != "[not available]"',
                     br(),
                     sliderInput('windowsize', label = 'Word distance window', min = 2, max=50, value=20))
  )
}

## todo: server side of compareUI
plotCompareUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    h4('Compare two corpora'),
    br(),
    selectInput('compare_class_col', 'Class column', choices = c()),
    selectInput('compare_date_col', 'Date column', choices = c()),
    radioButtons('compare_type', 'Comparison type', choices = c('corpus X to corpus Y', 'corpus X to all else'), inline = T),
    
    fluidPage(
      box(
        h5('Corpus X'),
        selectInput('compare_class_x', 'class', choices = c()),
        dateRangeInput('compare_date_x', 'date', start = '2000-01-01', end = '2100-01-01')
      ),
      conditionalPanel(condition = 'input.compare_type == "corpus X to corpus Y"',
        box(
          h5('Corpus Y'),
          selectInput('compare_class_y', 'class', choices = c()),
          dateRangeInput('compare_date_y', 'date', start = '2000-01-01', end = '2100-01-01')
        )
      )
    ),
    h5('Wordcloud parameters'),
    wordcloudParameters()
  )
}


############################################################################################################
########################################## OUTPUT ###############################################
## ui: 
## server: plotWordcloud; plotSemnet

plotWordcloud <- function(input, dtm){
  dtm.wordcloud(dtm, nterms=input$wordcloud_nterms, scale=rev(c(input$wordcloud_range)))
}

plotSemnet <- function(input, dtm, tokens){
  termselect = col_sums(dtm)
  termselect = head(names(termselect[order(-termselect)]), 500) # only use top 500 terms to speed up computation
  if(input$use_window){
    tokens = tokens[tokens$text %in% termselect,]
    g = semnet::wordWindowOccurence(tokens$position, tokens$text, tokens$doc_id, window.size = input$windowsize)
  } else {
    dtm = dtm[,termselect]
    print(dtm)
    g = semnet::coOccurenceNetwork(dtm)
  }
  g = semnet::getBackboneNetwork(g, alpha=0.001, max.vertices=100, use.original.alpha = F)
  V(g)$cluster = igraph::edge.betweenness.community(g, directed=F)$membership
  g = semnet::setNetworkAttributes(g, V(g)$cluster, V(g)$freq)
  g
}

#plotCompare <- function(input, dtm, meta){
#  updateSelectInput(session, 'compare_class_col', choices = c(colnames(d$meta), '[No class filter]'))
#  updateSelectInput(session, 'compare_date_col', choices = c(colnames(d$meta), '[No date filter]'))
#}

