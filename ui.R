library(shinydashboard)
library(shiny)
library(DT)
library(plotly)
library(dashboardthemes)
KH_THEME <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "#☼B2BCB6"
    ,primaryFontColor = "rgb(255, 38, 38)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(250, 250, 250)"
    
    ### header
    ,logoBackColor = "rgb(0, 130, 108)"
    
    ,headerButtonBackColor = "rgb(0, 130, 108)"
    ,headerButtonIconColor = "rgb(250, 250, 250)"
    ,headerButtonBackColorHover = "rgb(0, 130, 108)"
    ,headerButtonIconColorHover = "rgb(0, 130, 108)"
    
    ,headerBackColor = "rgb(0, 130, 108)"
    ,headerBoxShadowColor = "#e6e6ff"
    ,headerBoxShadowSize = "5px 5px 5px"
    ##rgb(92, 6, 41)
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(0, 130, 108)"
        ,colorMiddle = "rgb(123, 227, 194)"
        ,colorEnd = "rgb(245, 247, 247)"
        ,colorStartPos = 5
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarPadding = 10
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 10
    ,sidebarMenuBorderRadius = 50
    
    ,sidebarShadowRadius = "10px 10px 10px"
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(0, 145, 181)"
    
    ,sidebarSearchBackColor = "rgb(255, 255, 255)"
    ,sidebarSearchIconColor = "rgb(97, 235, 237)"
    ,sidebarSearchBorderColor = "rgb(97, 235, 237)"
    #########################
    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(35,106,135)"
    ,sidebarTabBorderWidth = 1
    
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(255, 255, 255)"
        ,colorMiddle = "rgba(255, 255, 255)"
        ,colorEnd = "rgba(255, 255, 255)"
        ,colorStartPos = 10
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(0,0,0)"
    ,sidebarTabRadiusSelected = "20px 20px 20px 20px"
    
    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(255, 255, 255)"
        ,colorMiddle = "rgba(255, 255, 255)"
        ,colorEnd = "rgba(255, 255, 255)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(167, 65, 91)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "20px 20px 20px 20px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ###
    ,boxPrimaryColor = "rgb(232, 241, 255)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(255, 255, 255)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(235, 255, 246)"
    ,buttonTextColor = "rgb(102, 0, 24)"
    ,buttonBorderColor = "rgb(99, 255, 250)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(0, 255, 247)"
    
    ,textboxBackColor = "rgb(250, 250, 250)"
    ,textboxBorderColor = "rgb(30, 112, 135)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(250, 250, 250)"
    ,textboxBorderColorSelect = "rgb(250, 250, 250)"
    
    ### tables #☻
    ,tableBackColor = "rgb(250, 250, 250)"
    ,tableBorderColor = "rgb(220, 234, 242)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)
dashboardPage(
    
    dashboardHeader(
        title =  "ConvMsa"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text="Introduction",tabName="intro",  icon = icon("fas fa-table")
            ),
            menuItem(
                text="Data conversion",tabName="data",  icon = icon("fas fa-table")
            ),
            
            menuItem(
                text="sequence alignment",tabName="msa",  icon = icon("fas fa-dna")
            ),
            menuItem(
                text="Contact",tabName="contact"
                
            )
        )
    ),
    dashboardBody(
        KH_THEME ,
        tabItems(
            tabItem("intro",
                   
                    
                    
                    div(id="Abstract",
                        p(strong("Abstract")),
                        p("Although there is an exponential increase and extensive availability of genome-wide association studies data,
                       the visualization of this data remains difficult for non-specialist users.
                          ConvMsa is an  R shiny web application, that allows any general user to convert genomic ID and  visualize data."), 
                        tags$hr(),
                        p(strong("Data conversion")),
                        p("This application accept CSV and TXT files, please upload files with two colums only and which contain ID of genomic sequences."),
                        h5("there is test data, you can upload them to test the application"), 
                        titlePanel(title=div(img(src="capture.PNG"))),
                        tags$hr(),
                        p(strong("Sequences alignement")),
                        p("please upload fasta files with multiple or single sequence."), 
                        titlePanel(title=div(img(src="1.PNG"))),
                        tags$hr(),
                        
                       
                        p("check that your file extension is .fa or .fasta"), 
                        titlePanel(title=div(img(src="fasta.PNG"))),
                        h5("sequences type allowed:"),
                        h6("BStringSet"),
                        h6("DNAStringSet"),
                        h6("RNAStringSet"),
                        h6("AAStringSet"),
                    
                    )),
            tabItem(
                
                tabName = "data",
                titlePanel(""),
                tabsetPanel(
                   
                    tabPanel("File Input",
                             sidebarLayout(
                                 sidebarPanel(
                                     h5("Welcome to the ConvMsa homepage! "),
                                     fileInput('file1', 'Choose a File',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv',
                                                        '.xls')),
                                     h6("CSV and TXT file are accepted."),
                                     
                                     # added interface for uploading data from
                                     # http://shiny.rstudio.com/gallery/file-upload.html
                                     tags$br(),
                                     checkboxInput('header', 'Header', TRUE),
                                     radioButtons('sep', 'Separator',
                                                  c(Comma=',',
                                                    Semicolon=';',
                                                    Tab='\t'),
                                                  ','),
                                     tags$b("Required file format(s)"),
                                     h6("the data must have two colums"),
                                     titlePanel(title=div(img(src="image.PNG"))),
                                     
                                     tags$br(),
                                     h5("You can also run a test alignment to test out the functionality of the tool"),
                                     tags$b("Download a Test Data"),
                                     tags$br(),
                                     downloadButton("downloadData", "Oryza sativa"),
                                     
                                     downloadButton("downloadData1", "Vitis vinifera")
                                     
                                     
                                     
                                 ),
                                 mainPanel(
                                     DT::dataTableOutput("data") 
                                     
                                 )
                                 
                                 
                             )
                    ),
                    tabPanel("Data Description",
                             
                             fluidRow(
                                 box(
                                     
                                     title = "",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     sidebarPanel(
                                         h4("You can rename the colums   "),
                                         textInput("v1", label = h5("first colum"), value = ""),
                                         textInput("v2", label = h5("second column"), value = "")
                                         
                                     ),
                                     id = "ta",side = "right", width = 1200 , height = 500,
                                     h3("Data Structure"),
                                     verbatimTextOutput("StrData"),
                                     verbatimTextOutput("NaText1"),
                                     verbatimTextOutput("NaText2"),
                                     #),
                                     #fluidRow(
                                    # h3("missing values"),
                                    # tableOutput("DataNa1")
                                    # ),
                                     
                                    
                                     plotlyOutput("donuts")
                                     
                                     
                                    
                                     #)
                                 )
                                 
                                 
                             )
                             
                             
                             
                    ),
                    tabPanel("Conversion",
                             
                             fluidRow(
                                 
                                 
                                 box(
                                     
                                     title = "ID converter ",solidHeader = TRUE,status = "primary",
                                     
                                     
                                     h6("(You can submit one or multiple IDs. comma is acceptable as delimiter.)"),
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     side = "right",  height = 450,
                                     
                                     textInput("ID", label = h3(""), value = ""),
                                     tags$br(),
                                     h5("Your identifier corresponds to"),
                                     verbatimTextOutput("retour")
                                     
                                 ),
                                 box(
                                     
                                     title = "Conversion Table",solidHeader = TRUE,status = "primary",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     side = "right", height = 450,
                                     
                                     tableOutput("IDRetour"),
                                     
                                 )
                                 
                                 
                                 
                             )
                             
                             
                             
                    )
                    
                    
                    
                    
                    
                )
            ),
            
            tabItem(
                tabName = "msa",
                titlePanel(""),
                tabsetPanel(
                    tabPanel("fasta file",
                             sidebarLayout(
                                 sidebarPanel(
                                     h5(" To start please upload a mulitple sequence alignment in fasta format (.fa or .fasta)"),
                                     fileInput('fasta', 'Choose your fasta file'
                                     ),
                                     h6(" Only Fasta files are accepted."),
                                     # added interface for uploading data from
                                     # http://shiny.rstudio.com/gallery/file-upload.html
                                     tags$br(),
                                     
                                     radioButtons('type', 'sequence type',
                                                  c(readBStringSet='BStringSet',
                                                    readDNAStringSet='DNAStringSet',
                                                    readRNAStringSet='RNAStringSet',
                                                    readAAStringSet='AAStringSet'
                                                  ),
                                                  ','),
                                     
                                     h4("Download a Test Data"),
                                     downloadButton("downloadData2", "Covid_prot"),
                                     downloadButton("downloadData3", "Covid_nuc")
                                     
                                     
                                 ),
                                 mainPanel(
                                     
                                     verbatimTextOutput("fastadata1"),
                                     
                                     
                                     
                                 )
                                 #
                                 
                             )
                    ),
                    tabPanel("File Transformation",
                             fluidRow(
                                 tabBox(
                                     
                                     title = "",
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     id = "ta",side = "right", width = 200 , height = 500,
                                     tabPanel("jeux de données",
                                              DT::dataTableOutput("DataMsa")
                                     ),
                                     tabPanel("MSA Plot",
                                              numericInput("START", "START", "1"),
                                              numericInput("END", "END", "1000"),
                                              plotOutput("MsaPlot")      
                                     ),
                                     tabPanel("Histogramme",
                                              plotlyOutput("FastaHist")
                                     ),
                                     tabPanel("cir",
                                              plotOutput("cir")
                                              ),
                                     tabPanel("pairwise alignment",
                                             
                                              numericInput("nb1", "Select a sequence", 3 ),
                                              numericInput("nb2", "Selecet a sequence", 4 ),
                                              uiOutput("inc")
                                              
                                              )
                                 ),
                                 
                                 
                             )      
                             
                    )
                   
                    
                    
                )
            ),
            tabItem(tabName = "contact",
                    fluidRow(
                        box(
                            title = "Author",width = 4, background = "navy",
            
                          
                            #h6("If you have any feedback to give on the tool, please contact me at my email above.")
                            em(
                                span("Created by "),
                                a("Khaoula oueslati", href = "mailto:oueslatikhaoula2@gmail.com"),
                                span(", june 2020"),
                                br(), br()
                            )
                        )
                    )
                
            )
            
            
            
        )
        
    )
)
