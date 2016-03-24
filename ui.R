#devtools::install_github('kasperwelbers/corpus-tools')
#devtools::install_github('kasperwelbers/semnet')
library(shinydashboard)
library(shiny)

#form-group

sidebar <- dashboardSidebar(width = 300,
  customStyle(),
                            
  sidebarMenu(
    ################################ READ DATA ####################################
    h3('Read data'),
    radioButtons('datatype', '', list('Text'='text', 'Tokens'='tokens'), 'text', inline = T),
    
    textInputUI(condition = "input.datatype == 'text'"),
    tokensInputUI(condition = "input.datatype == 'tokens'"),
    
    br(),
    loadDataButton(),
        
    br(), hr(),
    ############################ CORPUS PARAMETERS ################################
    h3('Corpus parameters'),
    textCorpusParametersUI('input.datatype == "text"'),
    tokensCorpusParametersUI('input.datatype == "tokens"'),
    generalCorpusParametersUI()
  )
)



## add codebook box
body <- dashboardBody(
  fluidRow(
    tabBox(width = 5,
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
               radioButtons('actions', 'Action:', list('Wordcloud'='wordcloud', 'Semantic network'='semnet', 'Compare corpora'='compare'), selected='wordcloud', inline=T),
               br(),
               plotWordcloudUI(condition = "input.actions == 'wordcloud'"),
               plotSemnetUI(condition = "input.actions == 'semnet'"),
               plotCompareUI(condition = "input.actions == 'compare'"),
               br(),
               actionButton('plotaction', 'Perform action')
      ),
      tabPanel(title = 'Output',
               plotParametersUI()
      )
    ),
    tabBox(width=7,
      tabPanel(title = "Plot", solidHeader = TRUE,
               htmlOutput('actionmessage'),
               plotOutput('actionplot', width = '95%', height='95%')
      )
    )
  )
)

dashboardPage(
  dashboardHeader(title = '', titleWidth = 300),
  sidebar,
  body
)