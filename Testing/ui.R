library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(mdsr)
library(plotly)
library(shinyjs)

dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Chi-Square Goodness-of-Fit Test and Simulation for Real Data",titleWidth=720),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  menuItem("App", tabName = "over", icon = icon("cubes"))
                  
                )),
              
              #Content within the tabs
              dashboardBody(
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css")
                ),
                
                tags$head(
                  tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: #1C2C5B;
                                  font-size: 30px; 

                                  }
                                  ")),
                
                
                tags$style(HTML("
                  .tabbable > .nav > li > a {background-color: #1C2C5B;  color:white}
                                .tabbable > .nav > li > a[data-value='Simulated p-values plot'] {background-color: #1C2C5B;  color:white}
                                .tabbable > .nav > li > a[data-value=' Comparsion to null distribution '] {background-color: #1C2C5B;  color:white}
                                .tabbable > .nav > li[class=active]    > a {background-color: grey; color:white}
                                "))),
                
                
                useShinyjs(),
                
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(4,
                                   
                                              
                                              textInput("names",h4(tags$strong("Level Names")),
                                                        ""),
                                              br(),
                                              br(),
                                              textInput("nulls",h4(tags$strong("Null Probabilities")),
                                                        ""),
                                              br(),
                                              br(),
                                              textInput("obs",h4(tags$strong("Observed Counts")),
                                                        ""),
                                              br(), 
                                   numericInput("sims",h4(tags$strong("Number of simulations from null model")),1,min=0,step=1),
                                   br(),
                                   actionButton("resample",h4(tags$div(tags$strong("Simulate Now"),style = "color:black"))),
                                   conditionalPanel(
                                     condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                                     actionButton("reset",h4(tags$div(tags$strong("Start Over"),style = "color:black")))
                                   )
                                              
                                   ),
                            
                            column(8,
                                   conditionalPanel(
                                     condition="input.resample == 0 || output.totalPrev == output.total",
                                     plotOutput("barGraphInitial"),
                                     p(textOutput("remarksInitial")),
                                     tags$head(tags$style("#remarksInitial{color: black ;
                                                           font-size: 17px;
                                                          }"
                                                          )),
                                     tableOutput("obsTable"),
                                     tags$head(tags$style("#obsTable{color: black ;
                                                           font-size: 16px;
                                                          }"
                                                          ))
                                   ),
                                   
                                   conditionalPanel(
                                     condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
                                     tabsetPanel(
                                                 tabPanel(h5(tags$div(tags$strong("Latest Simulation"),style = "color:white" )),
                                                          plotOutput("barGraphLatest"),
                                                          bsPopover("barGraphLatest", "", "This plot shows you the comparison of latest resample data verse actual data", placement = "left", options = list(container="body")),
                                                          p(textOutput("remarksLatest1")),
                                                          tags$head(tags$style("#remarksLatest1{color: black ;
                                                           font-size: 17px;
                                                                           }"
                                                          )),
                                                          tableOutput("summary1"),
                                                 tags$head(tags$style("#summary1{color: black ;
                                                           font-size: 16px;
                                                                      }"
                                                          ))), 
                                                 
                                                 tabPanel(h5(tags$div(tags$strong("Simulated p-values plot"),style = "color:white" )),
                                                          plotOutput("pvalueplot",height=400,width=630)),                                                                                                                                                                                                                                                                                     
                                                 
                                                 
                                                 tabPanel(h5(tags$div(tags$strong("
                                                   Comparsion to null distribution "), style = "color:white" )),
                                                          plotOutput("chisqCurve"),
                                                          br(),
                                                          
                                                          p(textOutput("remarksProb")),
                                                          tags$head(tags$style("#remarksProb{color: #1C2C5B ;
                                                           font-size: 18.5px;
                                                           font-style: italic;
                                                            }"
                                                          ))
                                                 ),
                                                 
                                                 id="myPanel"
                                     )
                                   )
                                   
                                   )
                                  
                                    
                                    
                            )
                          )
                  )
                          
                          )
                  )
              



               
            
    