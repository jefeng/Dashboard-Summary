library(shiny)
library(shinydashboard)

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Multiple Testing Caution",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("Part 1", tabName = "first", icon = icon("pencil-square"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3("About"),
                                   
                                   h4(tags$div(tags$strong("Cairo"),":", "Urbanek S and Horner J (2015).", tags$i("R graphics device using cairo graphics library for creating
                                          high-quality bitmap (PNG, JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and Win32) output ."), "
                                               R package version 1.5-9, ", tags$a(href="https://CRAN.R-project.org/package=Cairo", "https://CRAN.R-project.org/package=Cairo",style = "color:blue"))),
                                   
                                   
                                   
                                   h4(tags$div(tags$strong("DAAG"),":", "Maindonald JH and Braun WJ (2015).", tags$i("DAAG: Data Analysis and Graphics Data and Functions."), "
                                               R package version 1.22, ", tags$a(href="https://CRAN.R-project.org/package=DAAG", "https://CRAN.R-project.org/package=DAAG",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("discrimARTs"),":", "Rowland JM, Qualls C and Gunning C (2013).", tags$i("discrimARTs: Discrimination of Alternative Reproductive
                                 Tactics (ARTs)."), "
                                               R package version 0.2, ", tags$a(href="https://CRAN.R-project.org/package=discrimARTs", "https://CRAN.R-project.org/package=discrimARTs",style = "color:blue"))),
                                   
                                   
                                   h4(tags$div(tags$strong("ggplot2"),":", "Wickham H (2016).", tags$i("ggplot2: Elegant Graphics for Data Analysis."), "Springer-Verlag New
                                   York. ISBN 978-3-319-24277-4, ", tags$a(href="http://ggplot2.org", "http://ggplot2.org",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("grid"),":", "R Core Team (2016).", tags$i("A Language and Environment for Statistical Computing."), "
                                               R Foundation for
                                   Statistical Computing, Vienna, Austria, ", tags$a(href=" https://www.R-project.org", " https://www.R-project.org",style = "color:blue"))),
                                   

                                   h4(tags$div(tags$strong("markdown"),":", "Allaire J, Horner J, Marti V and Porte N (2015).", tags$i("markdown: 'Markdown' Rendering for R."), "
                                               R package version 0.7.7, ", tags$a(href="https://CRAN.R-project.org/package=markdown", "https://CRAN.R-project.org/package=markdown",style = "color:blue"))),
                                   
                                   
                                   h4(tags$div(tags$strong("MASS"),":", "Venables WN and Ripley BD (2002).", tags$i("Modern Applied Statistics with S, Fourth edition."), "
                                               Springer, New
                                   York. ISBN 0-387-95457-0, ", tags$a(href="http://www.stats.ox.ac.uk/pub/MASS4", "http://www.stats.ox.ac.uk/pub/MASS4",style = "color:blue"))),
                                   
                                   
                                   h4(tags$div(tags$strong("mdsr"),":", "Baumer B, Horton N and Kaplan D (2016).", tags$i("devtools: mdsr: Complement to 'Modern Data Science with
                                                                                                                          R'."), "R package version 0.1.3, ", tags$a(href="https://CRAN.R-project.org/package=mdsr", "https://CRAN.R-project.org/package=mdsr",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("plotly"),":", "Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2017).", tags$i("plotly: Create Interactive Web Graphics via 'plotly.js'."), "
                                               R package version 4.6.0, ", tags$a(href="https://CRAN.R-project.org/package=plotly", "https://CRAN.R-project.org/package=plotly",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("plotrix"),":", "Lemon, J. (2006).", "Plotrix: a package in the red light district of R", tags$i("R-News, 6(4)."),
                                               "8-12.",style = "color:black")),
                                   
                                   
                                   h4(tags$div(tags$strong("png"),":", "Urbanek S (2013).", tags$i("png: Read and write PNG images."), "
                                                R package version 0.1-7, ", tags$a(href="https://CRAN.R-project.org/package=png", "https://CRAN.R-project.org/package=png",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("RColorBrewer"),":", "Neuwirth E (2014).", tags$i("RColorBrewer: ColorBrewer Palettes."), "
                                                R package version 1.1-2, ", tags$a(href="https://CRAN.R-project.org/package=RColorBrewer", "https://CRAN.R-project.org/package=RColorBrewer",style = "color:blue"))),
                                  
                                   h4(tags$div(tags$strong("Rlab"),":", "Boos DD and Nychka D (2012).", tags$i("Rlab: Functions and Datasets Required for ST370 class."), "
                                                R package version 2.15.1, ", tags$a(href="https://CRAN.R-project.org/package=Rlab", "https://CRAN.R-project.org/package=Rlab",style = "color:blue"))),
                                   
                                   
                                   h4(tags$div(tags$strong("shinyDND"),":", "Hoffer A (2016).", tags$i("shinyDND: Shiny Drag-n-Drop."), "
                                                R package version 0.1.0, ", tags$a(href="https://CRAN.R-project.org/package=shinyDND", "https://CRAN.R-project.org/package=shinyDND",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("treemap"),":", "Tennekes M (2017).", tags$i("treemap: Treemap Visualization."), "
                                                R package version 2.4-2, ", tags$a(href="https://CRAN.R-project.org/package=treemap", "https://CRAN.R-project.org/package=treemap",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("V8"),":", "Ooms J (2017).", tags$i("V8: Embedded JavaScript Engine for R."), "
                                                R package version 1.5, ", tags$a(href="https://CRAN.R-project.org/package=V8", "https://CRAN.R-project.org/package=V8",style = "color:blue"))),
                                   
                                   
                                   h4(tags$div(tags$strong("shiny"),":", "Chang W, Cheng J, Allaire J, Xie Y and McPherson J (2017).", tags$i("shiny: Web Application Framework for R."), " R package version 1.0.3, ", tags$a(href="https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("shinyBS"),":", "Bailey E (2015).", tags$i("shinyBS: Twitter Bootstrap Components for Shiny."),
                                               " R package version 0.61, ", tags$a(href="https://CRAN.R-project.org/package=shinyBS", "https://CRAN.R-project.org/package=shinyBS",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("shinydashboard"),":", "Chang W and Borges Ribeiro B (2017).", tags$i("shinydashboard: Create Dashboards with 'Shiny'."), 
                                               " R package version 0.6.1, ", tags$a(href="https://CRAN.R-project.org/package=shinydashboard", "https://CRAN.R-project.org/package=shinydashboard",style = "color:blue"))),
                                   
                                   h4(tags$div(tags$strong("shinyjs"),":", "Attali D (2016).", tags$i("shinyjs: Easily Improve the User Experience of Your Shiny Apps in
                                                                                                      Seconds."), " R package version 0.9, ", tags$a(href="https://CRAN.R-project.org/package=shinyjs", "https://CRAN.R-project.org/package=shinyjs",style = "color:blue")))
                                   
                                   
                                   
                            )
                          )
                  )
                  
                )
              )
)