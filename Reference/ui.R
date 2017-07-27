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
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=Cairo",tags$strong("Cairo"),style = "color:#1C2C5B"),":", "Urbanek S and Horner J (2015).", tags$i("R graphics device using cairo graphics library for creating
                                                                                                                                                                                                  high-quality bitmap (PNG, JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and Win32) output ."), "
                                               R package version 1.5-9 ")),
                                   
                                   
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=DAAG",tags$strong("DAAG"),style = "color:#1C2C5B"),":", "Maindonald JH and Braun WJ (2015).", tags$i("DAAG: Data Analysis and Graphics Data and Functions."), "
                                               R package version 1.22 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=data.table",tags$strong("data.table"),style = "color:#1C2C5B"),":", "Dowle M, Srinivasan A, Short T, Saporta SLwcfR and Antonyan E (2015).", tags$i("data.table:
                                                                                                                                                                                                                                                   Extension of Data.frame."), "
                                               R package version 1.9.6 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=discrimARTs", tags$strong("discrimARTs"),style = "color:#1C2C5B"),":", "Rowland JM, Qualls C and Gunning C (2013).", tags$i("discrimARTs: Discrimination of Alternative Reproductive
                                                                                                                                                                                                                           Tactics (ARTs)."), "
                                               R package version 0.2 ")),
                                   
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=DT",tags$strong("DT"),style = "color:#1C2C5B"),":", "Xie Y (2016) .", tags$i("DT: A Wrapper of the JavaScript Library 'DataTables'."), "
                                               R package version 0.2 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=dtplyr",tags$strong("dtplyr"), style = "color:#1C2C5B"),":", "Hadley Wickham (2017).", tags$i("dtplyr: Data Table Back-End for 'dplyr'."), "
                                               R package version 0.0.2 ")),
                                   
                                   h4(tags$div(tags$a(href="http://ggplot2.org",tags$strong("ggplot2"),style = "color:#1C2C5B"),":", "Wickham H (2016).", tags$i("ggplot2: Elegant Graphics for Data Analysis."), "Springer-Verlag New
                                               York. ISBN 978-3-319-24277-4")),
                                   
                                   h4(tags$div(tags$a(href=" https://www.R-project.org",tags$strong("grid"),style = "color:#1C2C5B"),":", "R Core Team (2016).", tags$i("A Language and Environment for Statistical Computing."), "
                                               R Foundation for
                                               Statistical Computing, Vienna, Austria")), 
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=Hmisc",tags$strong("Hmisc"), style = "color:#1C2C5B"),":", "Frank E {Harrell Jr} and with contributions from Charles Dupont and many others (2017).", tags$i("Hmisc: Harrell Miscellaneous"), "
                                               R package version 4.0-3 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=markdown",tags$strong("markdown"),style = "color:#1C2C5B"),":", "Allaire J, Horner J, Marti V and Porte N (2015).", tags$i("markdown: 'Markdown' Rendering for R."), "
                                               R package version 0.7.7")), 
                                   
                                   
                                   h4(tags$div(tags$a(href="http://www.stats.ox.ac.uk/pub/MASS4",tags$strong("MASS"), style = "color:#1C2C5B"),":", "Venables WN and Ripley BD (2002).", tags$i("Modern Applied Statistics with S, Fourth edition."), "
                                               Springer, New
                                               York. ISBN 0-387-95457-0")),  
                                   
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=mdsr",tags$strong("mdsr"),style = "color:#1C2C5B"),":", "Baumer B, Horton N and Kaplan D (2016).", tags$i("devtools: mdsr: Complement to 'Modern Data Science with
                                                                                                                                                                                                         R'."), "R package version 0.1.3")),  
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=mosaic",tags$strong("mosaic"),style = "color:#1C2C5B"),":", "Pruim R, Kaplan DT and Horton NJ (2016).", tags$i("mosaic: Project MOSAIC Statistics and
                                                                                                                                                                                                              Mathematics Teaching Utilities."), "
                                               R package version 0.14.4")), 
                                   
                                   h4(tags$div(tags$a(tags$strong("plotly"), href = "https://CRAN.R-project.org/package=plotly",style = "color:#1C2C5B"),":", "Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2017).", tags$i("plotly: Create Interactive Web Graphics via 'plotly.js'."), "
                                               R package version 4.6.0")),
                                   
                                   h4(tags$div(tags$a(tags$strong("plotrix"), href = "https://cran.r-project.org/web/packages/plotrix/index.html", style = "color:#1C2C5B"),":", "Lemon, J. (2006).", "Plotrix: a package in the red light district of R",
                                               tags$i("R-News, 6(4),"), "8-12")),
                                   
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=png", tags$strong("png"), style = "color:#1C2C5B"),":", "Urbanek S (2013).", tags$i("png: Read and write PNG images."), "
                                               R package version 0.1-7")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=RColorBrewer", tags$strong("RColorBrewer") ,style = "color:#1C2C5B"),":", "Neuwirth E (2014).", tags$i("RColorBrewer: ColorBrewer Palettes."), "
                                               R package version 1.1-2 ")),
                                   
                                   h4(tags$div(tags$a(href="http://www.jstatsoft.org/v21/i12/",tags$strong("reshape2"),style = "color:#1C2C5B"),":", "Wickham H (2007).","Reshaping Data with the reshape Package.", tags$i("Journal of Statistical
                                                                                                                                                                                                                            Software, 21(12),"), "
                                               1-20")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=Rlab", tags$strong("Rlab"),style = "color:#1C2C5B"),":", "Boos DD and Nychka D (2012).", tags$i("Rlab: Functions and Datasets Required for ST370 class."), "
                                               R package version 2.15.1")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=scales", tags$strong("scales"),style = "color:#1C2C5B"),":", "Hadley Wickham (2016).", tags$i("scales: Scale Functions for Visualization."), "
                                               R package version 0.4.1")),
                                   
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=shiny", tags$strong("shiny"),style = "color:#1C2C5B"),":", "Chang W, Cheng J, Allaire J, Xie Y and McPherson J (2017).", 
                                               tags$i("shiny: Web Application Framework for R."), " R package version 1.0.3 ")), 
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=shinyBS", tags$strong("shinyBS"), style = "color:#1C2C5B"),":", "Bailey E (2015).", tags$i("shinyBS: Twitter Bootstrap Components for Shiny."),
                                               " R package version 0.61 ")), 
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=shinydashboard", tags$strong("shinydashboard"),style = "color:#1C2C5B"),":", "Chang W and Borges Ribeiro B (2017).", tags$i("shinydashboard: Create Dashboards with 'Shiny'."), 
                                               " R package version 0.6.1 ")), 
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=shinyDND", tags$strong("shinyDND"),style = "color:#1C2C5B"),":", "Hoffer A (2016).", tags$i("shinyDND: Shiny Drag-n-Drop."), "
                                               R package version 0.1.0 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=shinyjs",tags$strong("shinyjs"),  style = "color:#1C2C5B"),":", "Attali D (2016).", tags$i("shinyjs: Easily Improve the User Experience of Your Shiny Apps in
                                                                                                                                                                                          Seconds."), 
                                               " R package version 0.9")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=taRifx",tags$strong("taRifx"),style = "color:#1C2C5B"),":", "Friedman AB (2014).", tags$i("taRifx: Collection of utility and convenience functions."), "
                                               R package version 1.0.6")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=treemap",tags$strong("treemap"),style = "color:#1C2C5B"),":", "Tennekes M (2017).", tags$i("treemap: Treemap Visualization."), "
                                               R package version 2.4-2")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=truncnorm",tags$strong("truncnorm"), style = "color:#1C2C5B"),":", "Heike Trautmann and Detlef Steuer and Olaf Mersmann and Björn Bornkamp (2014).", tags$i("truncnorm: Truncated normal distribution"), "
                                               R package version 1.0-7 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=V8",tags$strong("V8"), style = "color:#1C2C5B"),":", "Ooms J (2017).", tags$i("V8: Embedded JavaScript Engine for R."), "
                                               R package version 1.5 ")),
                                   
                                   h4(tags$div(tags$a(href="https://CRAN.R-project.org/package=xlsx",tags$strong("xlsx"), style = "color:#1C2C5B"),":", "Adrian A. Dragulescu (2014).", tags$i("xlsx: Read, write, format Excel 2007 and Excel 97/2000/XP/2003 files "), "
                                               R package version 0.5.7 "))
                                   
                                   
                                   
                                   
                                   
                                   )
                                   )
                                   )
                  
                                   )
                                   )
                                   )