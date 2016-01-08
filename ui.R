library(shiny)


shinyUI(fluidPage(
          navbarPage("Visualization Tool for Mixed Effects Model",
              tabPanel("Data",
                  sidebarLayout(
                      sidebarPanel( 
                              # input file parameters
                              fileInput("file","Please upload your file"),
                              helpText("max file size is 5MB"),
                              br(),
                              h5("Select the parameters below"),
                              checkboxInput(inputId='header', label='header', value=TRUE),
                              checkboxInput(inputId="stringsAsFactors", label="stringsAsFactors", value=FALSE),
                              radioButtons(inputId='sep',label='Separator', choices=c(Comma=',', Semicolon=';',Tab='\t',Space=''), selected=','),
                              br() 
                              ),
                          # how the data file looks
                          mainPanel(dataTableOutput("table"))
                           )
                          ),
  
               tabPanel("Plots",  
                    sidebarLayout(
                        sidebarPanel( 
                            # select the dependent variable from a list of column names
                            selectInput("dependent", label = "Select the dependent variable from the list below",""),
                            
                            # select fixed variables
                            checkboxGroupInput(inputId="fixed",label='Choose Fixed Variable', "", ""),
                            
                            # enter random variables separated by comma
                            textInput("random", label = "Please enter random variables separated by comma", value = ""),
                            
                            actionButton("plotButton", "Click to proceed")
                               ),
                       mainPanel(
                            # plots showing relationship between fixed and random variables
                            uiOutput('uiFixedRandomPlots'),
                            
                            #random slope density plots and correlation plots
                            uiOutput('uiSlopeCorrelationPlots')
                           )
                          ) 
                         ),
  
      
            tabPanel("Model",  
                # model diagnostic plots for fixed variables and random intercepts only     
                 uiOutput('plotModel'),
                
                #model diagnostics plots after including random slopes and correlation terms 
                 uiOutput('plot_ui3')
                  )
            )
          ))
  


