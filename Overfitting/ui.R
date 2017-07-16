library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Overfitting",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  menuItem("Overview and Instruction", tabName = "over", icon = icon(" fa-hourglass-half")),
                  menuItem("App", tabName = "first", icon = icon("rss"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3("About:"),
                                   h4("In this app you will explore overfitting. Overfitting occurs when a model is excessively complex, 
                                      such as having too many parameters relative to the number of observations."),
                                   
                                   h3("How the data are generated:"),
                                   withMathJax(),
                                   h4("Step 1: Pick a number of Y from N(0,1) "),
                                   
                                   h4(helpText("Step 2: pick a number of",
                                               HTML(paste("X", tags$sub(1), sep=''),
                                                    " from N( Y, 1/\\(\\rho^2\\)), where 
                                                    \\(\\rho\\) is 
                                                    the true population correlation"),style = "color:inherit")),
                                   
                                   h4(helpText("Step 3: Independently pick a number of",
                                               HTML(paste("X", tags$sub(2), sep=''),
                                                    " from N( Y, 1/\\(\\rho^2\\)), where 
                                                    \\(\\rho\\) is 
                                                    the true population correlation"),style = "color:inherit")),
                                   
                                   h4(tags$div("Step 4: Continue until you have K variables: ",
                                               
                                               HTML(paste("X", tags$sub(1), sep='')),
                                               "to", HTML(paste("X", tags$sub("k"), sep='')))),
                                   
                                   h4("Step 5: Repeat the above n times"),
                                  
                                  
                                   
                                   h3("Instruction:"),
                                   h4("1. Move the sliders to change the values of sample size, true population correlation 
                                      and the numbers of variables in the model"),
                                   h4(tags$div("2. You need to ",
                                               
                                               tags$strong("first"), "click the ",
                                               tags$strong("plot button"),
                                               "and",
                                               tags$strong("then"), "click the ",
                                               tags$strong("validate button"))),
                                   
                                   h4(tags$div("3. If you want to generate a new plot with the same slider values, just click ",
                                               tags$strong("plot button"),
                                               "again")),
                                   h4("4. Use the hover in the app to see further information"  )
                                   
                                   
                                   
                                   )
                                   )
),

#Define the content contained within part 1 ie. tabname "first"
tabItem(tabName = "first",
        fluidRow(
          
          
          withMathJax(),
          column(4,
                 h3("Introduction:"),
                 box(width =11,background = "black",
                     "In the app you will calculate the sample correlations (r) of all 
                     the X variables with the Y variable and pick out the X that has the highest r value.
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
                 bsPopover("k", "", "Move the slider to change the number of variables in the model", place="right",options = list(container = "body")),
                 
                 actionButton("plot", h5(tags$strong("Click here first to Plot"))), 
                 bsPopover("plot", "", "There will be a density curve (black line) and a scatterplot (with a red regression line) shown once you click this button. The density plot is the residual between Y and estimated Y with the best picked X (The one that has the strongest correlation with Y) in the model and the scatterplot is plotting the replationship between the best picked X and Y", place="right",options = list(container = "body")),
                 br(),
                 
                 actionButton("validate", h5(tags$strong("Click here later to Validate")))),
          bsPopover("validate", "", "There will be a new density curve (blue line) shown once you click this button. This new blue curve is the residual between Y and estimated Y with a randomly picked X in the model", place="right",options = list(container = "body")),
          
          column(8,
                 h3("Plot:"),
                 splitLayout(style="border: 1px solid sliver;",
                             cellWidths = c("50%", "50%"),
                             cellArgs = list(style = "padding: 6px"),
                             plotOutput("plott"),
                             plotOutput("scatter"),
                             bsPopover("plott", "","It seemed like the black curve changes when adding the blue curve, however, this is caused by the scale. In order to fit both the black and blue curves perfectly, the scale must be adjusted", place="bottom",options = list(container = "body"))
                 )
                 
          ),
          column(7, offset=1, tableOutput("choose"),
                 bsPopover("choose", "", "Compared the sample best correlation and randomly chosen correlation (In absolute value terms)", place="bottom",options = list(container = "body"))
                 
          )
          
          
          
          
          
          
          )
        )
                )
              )
                  )

