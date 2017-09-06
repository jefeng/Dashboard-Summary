library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(shinyjs)
dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Chi-Square Goodness-fit-Test and Simulation ",titleWidth=550),
              
              #Sidebar
              dashboardSidebar(
                width = 221,
                sidebarMenu(
                  id = "tabs",
                  menuItem("Overview and Instruction", tabName = "over", icon = icon("github")),
                  menuItem("App Part I", tabName = "first", icon = icon("volume-up")),
                  menuItem("App Part II", tabName = "second", icon = icon("bullhorn"))
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
                            column(12,
                                   h3("About:"),
                                   h4("In this app you will explore Chi-Square Goodness-fit-Test with simulations.
                                        The test is applied when you have categorical variables from a population." 
                                     ),
                                   
                                   
                                   h4(tags$div("When an analyst attempts to fit a statistical model to observed data, he or she may wonder
                                  how well the model actually reflects the data. How close are the observed values to those which would be expected under the fitted model? 
                                  One statistical test that addresses this issue is the chi-square goodness of fit test. In general, the chi-square test statistic is of the form:"
                                       ),
                                      br(),
                               tags$img(src = "fit.jpg", width = "384px", height = "86px", style="display: block; margin-left: auto; margin-right: auto;"),
                                      br(),
                                              tags$div("If the computed test statistic is large, then the observed and expected 
                                               values are not close and the model is a poor fit to the data. "
                                      
                                               )),
                                   
                                   
                                   h3("Instructions for use:"),
                                   h4("1. Select one of the scenarios for the proportion in each category (Equal Probabilities or Different Probabilities)"),
                                   h4("2. Move the sliders to change the values of number of observations, number of categories and number of simulations"),
                                   
                                   h4(tags$div("3. A p-value is calculated and plotted for each simulation. You 
                                               can click a p-value on the plot to see the summary table for that 
                                               dataset")),
                                   h4("4. When there are more than 50 simulations, only a histogram of p-values is shown"),
                                   h4(tags$div("5. Click the ", tags$strong("link on the bottom-left "),
                                   "if you have your own data and null hypothesis to explore")), 
                                   br(),
                                   div(style = "text-align: center" ,bsButton("start", "Explore", icon("hand-o-right"), size = "large", style = "primary")),
                               
                               
                                   h3("Acknowledgement and Credit:"),
                                   h4("This app was developed and coded by Jinglin Feng. Special thanks to Alex Chen and Yuxin Zhang for help on some programming issues. ")
                               
                               
                                    
                            )
                          )
                  ),
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          fluidRow(
                            
                            
                            withMathJax(),
                            column(4,
                                   h3("Introduction:"),
                                   box(width ="10.5%",background = "navy",
                                      "You can either simulate example data or test if you have real data"),
                                   
                                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
                                   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
                              
                                   radioButtons("random", "Proportion in each category", choices = c("Equiprobable Null", "Different Null Probabilities")),
                                   
                                   sliderInput("n", "Sample Size:", min = 200, max = 2000, value = 1100 ,
                                               step = 1),
                                   bsPopover("n", "", "Number of Observations", place="right"),
                                   
                                   sliderInput("n2", "The number of Categories:", min = 2, max = 8, value = 5 ,
                                               step = 1) ,
                                   
                                   bsPopover("n2", "", "Number of Categories", place="right"),
                                   
                                   sliderInput("n3", "The number of Simulations:", min = 1, max = 1000, value = 5 ,
                                               step = 1),
                                   
                                   bsPopover("n3", "", "For the first 50 simulations, you will see a p-value scatterplot; For the number of simulations greater than 50, you will see a histogram of p-values.", place="right", options = list(container = "body")),
                                   
                                   div(style = "text-align: left" ,bsButton("test", tags$strong("Click here if you have real data to test"), icon("hand-o-right"), size = "large", style = "link")),
                                   
                                   
                                   conditionalPanel(
                                   
                                   condition="input.n3 <= 50",
                                   tags$img(src = "arrow.gif", width="15%", style="display: block; margin-left: 115px; margin-right: 4px;"),
                                   
                                   textOutput("hint"),
                                   tags$head(tags$style("#hint{color: #1C2C5B ;
                                                        font-size: 18px;
                                                        
                                                        
                                                        }"
                                                          ))
                                   )
                                   
                                   ),
                          
                                 h3("Table and Plot:"),
                                 column(7,align="center",
                                        
                                 conditionalPanel(condition = "input.random == 'Equiprobable Null'",
                                                  tableOutput("values2"),
                                                  bsPopover("values2","","An example of a summary table of population values", placement = "bottom", options = list(container = "body")),
                                   
                                       conditionalPanel(condition = "input.random == 'Equiprobable Null'",             
                                                  plotOutput("plot2", width='90%', click = "plot_click"),
                                                  bsPopover("plot2","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null ", place="right", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints"), 
                                                  bsPopover("plot_clickedpoints","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                  htmlOutput("text2", class="text-center")
                                                  
                          ))),
                          
                          column(7,align="center",
                                 
                                 conditionalPanel(condition = "input.random == 'Different Null Probabilities'",
                                                  tableOutput("values1"),
                                                  bsPopover("values1","","An example of a summary table of population values", placement = "bottom", options = list(container = "body")),
                                               
                                                  plotOutput("plot1", width="90%", click = "plot_click"),
                                                  bsPopover("plot1","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null )", place="right", options = list(container = "body")),
                                                 
                                                  tableOutput("plot_clickedpoints2"), 
                                                        bsPopover("plot_clickedpoints2","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                        htmlOutput("text1", class="text-center")))                 
                                       )
                          
                  
                  
                 
                                   
                                   
                            ),
                  tabItem(tabName = "second",
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(4,
                                   
                                   
                                   textInput("names",h4(tags$strong("Level Names")),
                                             ""),
                                   br(),
                                   
                                   textInput("nulls",h4(tags$strong("Null Probabilities")),
                                             ""),
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
                                                conditionalPanel(
                                                  
                                                  condition="input.sims <= 5",
                                                  p(textOutput("remarksProb2")),
                                                  tags$head(tags$style("#remarksProb2{color: #1C2C5B ;
                                                                       font-size: 18.5px;
                                                                       font-style: italic;
                                                                       }"
                                                          ))),
                                                
                                                conditionalPanel(
                                                  
                                                  condition="input.sims > 5",
                                                  p(textOutput("remarksProb")),
                                                  tags$head(tags$style("#remarksProb{color: #1C2C5B ;
                                                                       font-size: 18.5px;
                                                                       font-style: italic;
                                                                       }"
                                                          )))
                                                
                                                  ))),
                                   
                                   
                                   
                                   id="myPanel"
                                                  )
                                                  ))
                          )
                  )
)


                  
            
    