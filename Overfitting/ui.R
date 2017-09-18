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
                  id = "tabs",
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
                                   
                                   h4(tags$div("3. If you want to generate a new plot with the same slider values, just click the",
                                               tags$strong("plot button"),
                                               "again")),
                                  
                                  br(),
                                  div(style = "text-align: center" ,bsButton("start", "Explore",icon("hand-o-right"), size = "large", style = "primary")),
                                   
                                  h3("Acknowledgement and Credit:"),
                                  h4("This app was developed and coded by Jinglin Feng. Special thanks to Alex Chen for being my partner in this project.")
                                  
                                  
                                   
                                   
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
                     "A researcher is about to look at many explanatory 
                     variables and pick the one X that predicts Y the best. 
                     The sliders below allow you to set the number of explanatory 
                     variables, the sample size, and the population correlation between the 
                     explanatory variables and the response variable. Later, the 
                     researcher will run a validation study with new, independent
                     observations for X."),
                 
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                 tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
                 tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
                 
                 sliderInput("n", "Sample Size:", min = 2, max = 50, value = 5 ,
                             step = 1),
                 bsPopover("n", "", "Number of Observations", place="right",options = list(container = "body")),
                 
                 sliderInput("p", "True Population Correlation:", min = -1, max = 1 , value = 0,
                             step = 0.01),
                 
                 bsPopover("p", "", "Move the slider to change true population correlation", place="right",options = list(container = "body")),
                 
                 sliderInput("k", "The Number of Variables:", min = 1, max = 100 , value = 100 ,
                             step = 1),
                 bsPopover("k", "", "Move the slider to change the number of explanatory variables you are choosing from", place="right",options = list(container = "body")),
                 
                 actionButton("plot", h5(tags$strong("Click to plot a new dataset"))), 
                 bsPopover("plot", "", "The scatterplot on the left shows the relationship between the best picked X and Y.  The box plot on the right summarizes the distribution of the residuals when you predict Y from the best picked X.", place="right",options = list(container = "body")),
                 br(),
                 br(),
                 conditionalPanel("input.plot != 0",
                 actionButton("validate", h5(tags$strong("Click here later to Validate"))))),
          bsPopover("validate", "", "Click to show a scatterplot of Y versus X for the new data used to validate the relationship and a box plot of the distribution of residuals from a validation experiment with new observations of X and Y.", place="right",options = list(container = "body")),
          
          h3("Plot:"),
          column(4,   
                 align="center",
                 plotOutput("scatter", height=360),
                 tableOutput("choose"),
                 bsPopover("choose", "", "Compare the sample best Correlation to the validation set correlation (in absolute terms). ", place="bottom",options = list(container = "body")),
                 plotOutput("scatter2",height = 360)
                 
          ),
         
          column(4,
                 br(),
                 br(),
                 br(),
                 plotOutput("plott",height=450),
                 
                 br(),
                 conditionalPanel("input.validate != 0",
                                  
                                  
                                  h3("Challenge:"),
                                  
                                  
                                  h4("How does the overfitting effect depend on 
                                     sample sizes, true population correlation and 
                                     the number of variables you are choosing from ?")
                                  
                                  ))
           
          
          )
          
          
          
          )
        )
                )
)

              
                  

