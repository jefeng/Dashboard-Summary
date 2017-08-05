library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Chi-Square Goodness-fit-Test and Simulation ",titleWidth=550),
              
              #Sidebar
              dashboardSidebar(
                width = 221,
                sidebarMenu(
                  menuItem("Overview and Instruction", tabName = "over", icon = icon("github")),
                  menuItem("App", tabName = "first", icon = icon("volume-up"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  
                  tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css")
                ),
                
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
                                   br(),
                                   
                                   h3("Instructions for use:"),
                                   h4("1. Select one of the scenarios in terms of proportion in each category"),
                                   h4("2. Move the sliders to change the values of number of observations, number of categories and number of simulations"),
                                   h4(tags$div("3. Click the ", tags$strong("link below ' The number of Simulations ' slider "),
                                   "if you have real data to test")),
                                   h4(tags$div("4. A p-value is calculated and plotted for each simulation. You 
                                               can click a p-value on the plot to see the summary table for that 
                                               dataset")),
                                   h4("5. When there are more than 50 simulations, only a histogram of p-values is shown"),
                                    
                                   br(),
                               
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
                                   
                                 
                                   h4(tags$div(a(href="http://shiny.science.psu.edu/jpf5265/Testing/", 
                                              tags$strong("Click here if you have real data to test ",style = "color:#1C2C5B")))),
                                   
                                   conditionalPanel(
                                   
                                   condition="input.n3 <= 50",
                                   tags$img(src = "arrow.gif", width="15%", style="display: block; margin-left: 115px; margin-right: 4px;"),
                                   
                                   textOutput("hint"),
                                   tags$head(tags$style("#hint{color: #1C2C5B ;
                                                        font-size: 18px;
                                                        
                                                        
                                                        }"
                                                          )))
                                   
                                   ),
                          column(7,
                                 h3("Table and Plot:"),
                                 conditionalPanel(condition = "input.random == 'Equiprobable Null'",
                                                  tableOutput("values2"),
                                                  bsPopover("values2","","An example of a summary table of population values", placement = "top", options = list(container = "body")),
                                                  plotOutput("plot2", width='90%', click = "plot_click"),
                                                  bsPopover("plot2","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null ", place="right", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints"), 
                                                  bsPopover("plot_clickedpoints","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                  htmlOutput("text2", class="text-center")
                                                  
                          )),
                          
                          column(7,
                                 
                                 conditionalPanel(condition = "input.random == 'Different Null Probabilities'",
                                                  tableOutput("values1"),
                                                  bsPopover("values1","","An example of a summary table of population values", placement = "top", options = list(container = "body")),
                                                  plotOutput("plot1", width="90%", click = "plot_click"),
                                                  bsPopover("plot1","","For the number of simulations less than or equal to 50, click a point on the scatterplot to see the table behind it; For the number of simulations greater than 50, you will see a histogram of p-values. The red line denotes the uniform density of p-values under the null )", place="right", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints2"), 
                                                  bsPopover("plot_clickedpoints2","","An example of a summary table of sample values", placement = "right", options = list(container = "body")),
                                                  htmlOutput("text1", class="text-center")
                                                  
                                                  
                                 )
                          
                          )
                          
                  
                  
                 
                                   
                                   
                            )
                          )
                  )
)
)
                  
            
    