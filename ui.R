#devtools::install_github('kasperwelbers/corpus-tools')
#devtools::install_github('kasperwelbers/semnet')
library(shinydashboard)
library(shiny)


sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto;}"))),  ## add scroll to sidebar (because conditinalpanels mess up the height)
  
  sidebarMenu(
    h3('Read data'),
    radioButtons('datatype', '', list('Raw text'='raw', 'Tokens'='tokens'), 'raw', inline = T),

    ## if datatype is raw (text), show one upload button for a csv containing the raw texts and meta information in columns
    conditionalPanel(
      condition = "input.datatype == 'raw'",
      fileInput('raw', label = "", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      selectInput('raw_docid_col', label = 'Document id column', choices=c(), multiple = F),
      selectInput('raw_text_col', label = 'Text column(s)', choices=c(), multiple = T)
    ),
    
    ## if datatype is tokens, show two upload buttons: one for a csv with the tokens and one for a csv with the meta information
    conditionalPanel(
      condition = "input.datatype == 'tokens'",
      fileInput('tokens', label = "Tokens", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      selectInput('tokens_docid_col', label = 'Document id column (tokens)', choices=c(), multiple = F),
      selectInput('tokens_text_col', label = 'Text column', choices=c(), multiple = F),
      fileInput('meta', label = "Meta", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      selectInput('meta_docid_col', label = 'Document id column (meta)', choices=c(), multiple = F)
    ),
    br(),
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('load', 'read data', icon = icon('play-circle'), width = 200, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
    
    br(),
    hr(),
    h3('Corpus parameters'),
    conditionalPanel(
      condition = 'input.datatype == "raw"',
      selectInput('language', 'Language', c('english','dutch','german','french','spanish'), multiple=F, selected='english'),
      selectInput('filters', 'filter/transform terms', 
                  list('remove stopwords'='stopwords', 'remove numbers'='numbers','remove punctuation'='punctuation','stem words'='stem'), 
                  multiple=T, selected=c('stopwords','numbers','punctuation','stem'))
    ),
    conditionalPanel(
      condition = 'input.datatype == "tokens"',
      selectInput('filters', 'filter/transform terms', 
                  list('remove numbers'='numbers','remove punctuation'='punctuation'), 
                  multiple=T, selected=c('numbers','punctuation')),
      selectInput('poscolumn', 'Part-of-speech tags column', choices=c(), multiple = F),
      htmlOutput('poswarning'),
      selectInput('posfilter', 'Part-of-speech filter', choices=c(), multiple = T)
    ),
    sliderInput('docfreq', label = 'min/max document frequency', min = NA, max=NA, value=c(NA,NA)),
    sliderInput('wordlength', label = 'min/max word length', min=1, max=200, value=c(1, 200))
  )
)

## add codebook box
body <- dashboardBody(
  fluidRow(
    tabBox(
      tabPanel(title = 'Meta',
               dataTableOutput('meta')
      ),
      tabPanel(title = 'Tokens',
               dataTableOutput('tokens')
      ),
      tabPanel(title = 'DTM',
               htmlOutput('dtmsummary'),
               dataTableOutput('termstats')
      ),
      tabPanel(title = 'Action',
               radioButtons('actions', 'Action:', list('Wordcloud'='wordcloud', 'LDA'='lda'), selected='wordcloud', inline=T),
               br(),
               conditionalPanel(
                 condition = "input$actions == 'wordcloud'",
                  h4('Create a wordcloud of the most frequent words in the DTM'),
                  sliderInput('wordcloud_nterms', 'Number of terms', min=1, max=200, value=100),
                  sliderInput('wordcloud_range', 'Wordsize range', min=0.1, max=10, value=c(0.5, 6))
               ),
               conditionalPanel(
                 condition = "input$actions == 'lda'",
                  h4('Make a topicmodel using Latent Dirichlet Allocation')
               ),
               br(),
               actionButton('action', 'Perform action')
      )
    ),
    tabBox(
      tabPanel(title = "Plot", solidHeader = TRUE,
               shiny::plotOutput('actionplot')
      )
    )
  )
)

dashboardPage(
  dashboardHeader(title = ''),
  sidebar,
  body
)