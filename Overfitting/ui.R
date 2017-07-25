library(shiny)
library(shinydashboard)
library(shinyBS)
dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Overfitting",titleWidth=235),
              
              #Sidebar
              dashboardSidebar(
                width = 235,
                sidebarMenu(
                  menuItem("Overview and Instruction", tabName = "over", icon = icon(" fa-hourglass-half")),
                  menuItem("App", tabName = "first", icon = icon("rss"))
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
                                   h4("This app explores how you can become overconfident
                                      when you are choosing the best explanatory variable from many choices"),
                                   br(),
                                   
                                   h3("Understanding the overfitting effect:"),
                                   withMathJax(),
                                   h4(" 1. A reasearcher looks at many explanatory variables
                                      and picks the one that predicts Y the best"),
                                  
                                   
                                   h4(" 2. But if we draw another sample randomly from the 
                                      same model, it will not fit nearly as well"),
                                  
                                  br(),
                                   
                                   h3("Instructions for use:"),
                                   h4("1. Move the sliders to change the values of sample size, true population correlation 
                                      and the number of variables you are choosing from"),
                                   h4(tags$div("2. You need to ",
                                               
                                               tags$strong("first"), "click the ",
                                               tags$strong("plot button"),
                                               "and",
                                               tags$strong("then"), "click the ",
                                               tags$strong("validate button"))),
                                   
                                   h4(tags$div("3. If you want to generate a new plot with the same slider values, just click ",
                                               tags$strong("plot button"),
                                               "again")),
                                  
                                  br(),
                                  h3("Acknowledgement and Credit:"),
                                 h4(tags$div(tags$strong("shiny"),":", "Chang W, Cheng J, Allaire J, Xie Y and McPherson J (2017).", tags$i("shiny: Web Application
                                                                                                                                             Framework for R."), " R package version 1.0.3, ", tags$a(href="https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny",style = "color:blue"))),
                                  h4(tags$div(tags$strong("shinyBS"),":", "Bailey E (2015).", tags$i("shinyBS: Twitter Bootstrap Components for Shiny."),
                                              " R package version 0.61, ", tags$a(href="https://CRAN.R-project.org/package=shinyBS", "https://CRAN.R-project.org/package=shinyBS",style = "color:blue"))),
                                  h4(tags$div(tags$strong("shinydashboard"),":", "Chang W and Borges Ribeiro B (2017).", tags$i("shinydashboard: Create Dashboards with 'Shiny'."), 
                                              " R package version 0.6.1, ", tags$a(href="https://CRAN.R-project.org/package=shinydashboard", "https://CRAN.R-project.org/package=shinydashboard",style = "color:blue")))
                                  
                                   
                                   
                                   
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
                     "In the app you will calculate sample correlations (r) of all 
                     X variables with Y variable and pick out the X that has the highest r with Y.
                     You can then compare this r value with the r value associated with a randomly chosen X variable."),
                 
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                 tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
                 tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
                 
                 sliderInput("n", "Sample Size:", min = 1, max = 1000, value = 5 ,
                             step = 1),
                 bsPopover("n", "", "Number of Observations", place="right",options = list(container = "body")),
                 
                 sliderInput("p", "True Population Correlation:", min = -1, max = 1 , value = 0,
                             step = 0.01),
                 
                 bsPopover("p", "", "Move the slider to change true population correlation", place="right",options = list(container = "body")),
                 
                 sliderInput("k", "The Number of Variables:", min = 1, max = 100 , value = 3 ,
                             step = 1),
                 bsPopover("k", "", "Move the slider to change the number of explanatory variables you are choosing from", place="right",options = list(container = "body")),
                 
                 actionButton("plot", h5(tags$strong("Click to plot a new dataset"))), 
                 bsPopover("plot", "", "The density plot (in black) is the plot of residuals using the best picked X (The one that has the strongest correlation with Y). The scatterplot on the right shows the replationship between the best picked X and Y", place="right",options = list(container = "body")),
                 br(),
                 
                 conditionalPanel("input.plot != 0",
                 actionButton("validate", h5(tags$strong("Click here later to Validate"))))),
          bsPopover("validate", "", " Click to show density plot of residuals with a randomly picked X (in blue) ", place="right",options = list(container = "body")),
          
          column(8,
                 h3("Plot:"),
                 splitLayout(style="border: 1px solid sliver;",
                             cellWidths = c("50%", "50%"),
                             cellArgs = list(style = "padding: 6px"),
                             plotOutput("plott"),
                             plotOutput("scatter"),
                             bsPopover("plott", "","Black curve stays constant when you validate with a new X, but scale might change to fit two curves perfectly ", place="bottom",options = list(container = "body"))
                 )
                 
          ),
          column(7, offset=1, tableOutput("choose"),
                 bsPopover("choose", "", "Compared the sample best correlation and randomly chosen correlation (In absolute value terms)", place="bottom",options = list(container = "body")),
          
          
          conditionalPanel("input.validate != 0",
          
          h3("Challenge:"),
          
          
          h4("How does the overfitting effect depend on 
               sample sizes, true population correlation and 
             the number of variables you are choosing from ?")
          
          )
          
          
          )
          
          
          
          )
        )
                )
              )
                  )

