library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(mdsr)
library(plotly)

dashboardPage(skin="black",
              
              #Title
              dashboardHeader(title="Simpson's paradox",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  menuItem("Overview and Instruction", tabName = "over", icon = icon("superpowers")),
                  menuItem("App", tabName = "first", icon = icon("usb"))
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
                                   h4("In this app you will explore Simpson's paradox.
                                      Simpson's paradox is a phenomenon in which a trend appears in different 
                                      groups of data but disappears or reverses when these groups are combined."
                                      ),
                                       
                                   h3("Background:"),
                                   h4(tags$div("In 1973, the University of California-Berkeley was sued for sex discrimination. 
                                               The numbers looked pretty incriminating: the graduate schools had just 
                                               accepted 44% of male applicants but only 35% of female applicants. 
                                               When researchers looked at the evidence, though, they uncovered something surprising:"
                                       ),
                                      br(),
                               tags$img(src = "paradox.jpg", width = "476px", height = "48px", style="display: block; margin-left: auto; margin-right: auto;"),
                                      br(),
                                              tags$div("So Simpson's Paradox is defined as: "),
                                                br(),  
                                              tags$div("An observed association between two 
                                                                   variables can",
                                                       tags$strong(" change or even reverse direction"),  
                                                              "when there is",
                                                       tags$strong("another variable that interacts strongly"),
                                                   "with both variables.")
                                      
                                               ),
                                   
                                   
                                   h3("Instruction:"),
                                   h4("1.Data come from 'SAT_2010' file from Package 'mdsr' embedded in R.  
                                      12 states are selected and assigned into 2 groups ( High and Low ) based on the
                                      percent of students taking SAT "),
                                   h4(tags$div("2. The national level of SAT participation rate is 27%, so ",
                                               tags$strong("'High'"), "group is States that have SAT Participation Rate",
                                      
                                            tags$strong("greater than 27%; "),tags$strong("'Low'"), 
                                            "group is States that have SAT Participation Rate",
                                            tags$strong("less than 27%"))),
                               
                                   h4("3. You must click the button first to see the plot of original/actual paradox effect"),
                                   h4("4. Move the slider to see the change from actual paradox effect to no paradox effect"),
                                   h4("5. Use the hover in the app to see further information.")
                                 
                                  
                                    
                                    
                            )
                          )
                  ),
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          fluidRow(
                            
                            
                            
                            
                          column(6,offset = 1/2,
                                 h3("Plot:"),
                                 column(2, offset=1, actionButton("newplot2", h5(tags$strong("Click here first to show the plot")))),
                                 bsPopover("newplot2", "", "This plot shows you the original (actual) paradox effect", place="right",options = list(container = "body")),
                                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                                 br(),
                                 br(),
                                 br(),
                                 
                                sliderInput("integer", label = div(style='width:420px;', 
                                                                            div(style='float:left; width:50%', 'No Paradox Effect'), 
                                                                            div(style='float:right;width:50%', 'Actual Paradox Effect')), 
                                                                min = 0, max = 1, value = 1, width = '340px'),
                                 bsPopover("integer", "", "Move the slider to see how the Simpson Paradox effect changes. Default as actual paradox effect, which is the true SAT participation rates in the dataset. No paradox effect is the case that when all states have equal SAT participation rates", place="right", options = list(container = "body")),
                          
                                 
                          
                          conditionalPanel("input.newplot2 != 0",
                                                  plotlyOutput("plot2"),
                                           bsPopover("plot2", "", "Hover the point to see more details", placement = "right"),
                                           img(src="jinglin.jpg", width="80%")
                                           )
                          
                                
                            ),
                          column(4,offset=1,
                                 h3("Introduction:"),
                                 box(width ="10.5%",background = "navy",
                                     "This dataset is about the SAT results by state in 2010. There 
                                     are 12 selected states with different teachers salaries, SAT 
                                     scores and SAT participation rates. The variable 'salary' is 
                                     the average teachers salaries in US dollar; The variable 'total' 
                                     is the state average SAT score; The variable 'sat pct' is 
                                     the percent of students taking SAT in that state."),
                                 
                                 #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                                 tableOutput("SAT1"),
                                 
                                 h4(tags$div(
                                   tags$strong("Low Group"),
                                   ": States have SAT Participation Rate", 
                                   tags$strong("less than 27%" ),
                                   "(National Level)")),
                                 
                                 h4(tags$div(
                                   tags$strong("High Group"),
                                   ": States have SAT Participation Rate", 
                                   tags$strong("greater than 27%" ),
                                   "(National Level)"))
                                 
                                 
                                 )
                          
                          )
                  )
              )
)
)


               
            
    