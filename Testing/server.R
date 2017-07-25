library(shiny)
library(shinyBS)

source("chisqplot.R")

shinyServer(function(input, output,session) {
  simLimit <- 10000
  
  
  numberSims <- 0
  chisqSims <- numeric()
  latestSim <- NULL
  fullSim <-character()
  
  
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one
  
  namesInput <- reactive({
    unlist(strsplit(input$names,split=","))  
  })
  
  nullsInput <- reactive({
    probs <- as.numeric(unlist(strsplit(input$nulls,split=",")))
    probs
  })
  
  obsInput <- reactive({
    observed <- as.numeric(unlist(strsplit(input$obs,split=",")))
    observed 
  })
  
  
  
  goodNames <- reactive({
    names <- namesInput()
    goodNames <- TRUE
    if (length(names) <= 1) goodNames <- FALSE
    if (any(is.na(names))) goodNames <- FALSE
    if (!goodNames) disable("resample")
    goodNames     
  })
  
  
  goodNulls <- reactive({
    nulls <- nullsInput()
    goodNulls <- TRUE
    if (length(nulls) <= 1) goodNulls <- FALSE
    if (any(is.na(nulls))) goodNulls <- FALSE
    if (!any(is.na(nulls)) && any(nulls <= 0)) goodNulls <- FALSE
    if (!goodNulls) disable("resample")
    goodNulls     
  })
  
  goodNulls2<-
    reactive({
      nulls <- nullsInput()
      goodNulls <- TRUE
      if (sum(nulls)!=1) goodNulls<-FALSE
      if (!goodNulls) disable("resample")
      goodNulls     
    })
  
  goodObs <- reactive({
    obs <- obsInput()
    goodObs <- TRUE
    if (length(obs) <= 1) goodObs <- FALSE
    if (any(is.na(obs))) goodObs <- FALSE
    if (any(obs < 0)) goodObs <- FALSE
    if (!goodObs) disable("resample")
    goodObs     
  })
  
  
  
  obschisqInput <- reactive({
    nulls <- nullsInput()/sum(nullsInput())
    totalCounts <- sum(obsInput())
    expected <- nulls*totalCounts
    sum(obsInput()^2/expected)-totalCounts
  })
  
  simsUpdate <- reactive({
    if (input$resample > 0) {
      nullProbs <- isolate(nullsInput()/sum(nullsInput()))
      totalCounts <- isolate(sum(obsInput()))
      expCounts <- nullProbs*totalCounts
      reps <- min(simLimit,isolate(input$sims))
      newSims <- rmultinom(n=reps,size=totalCounts,prob=nullProbs)
      chisqNew <- colSums(newSims^2/expCounts)-totalCounts
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[,reps]
      numberSims <<- numberSims + reps
      total <<- total+reps
      
      
      hide(id="setup",anim=T,animType="slide")
      
      if (total - totalPrev == 1) {
        updateTabsetPanel(session,"myPanel",selected="Latest Simulation")
      }
      
      varLevels <- isolate(namesInput())
      namesList <- rep(varLevels,times=latestSim)
      fullSim <<- sample(namesList,size=totalCounts,replace=FALSE)
      list(numberSims,latestSim)
    }
  })
  
  
  
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latestSim <<- NULL
    
    show(id="setup",anim=T,animType="slide")
    
    return(totalPrev)
  })
  
  
  
  dfInput <- reactive({
    length(obsInput())-1
  })
  
  
  xmaxInput <- reactive({
    
    qchisq(0.999,df=dfInput())
  })
  
  
  output$totalPrev <- reactive({
    simsReset()
  })
  
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)
  
  output$total <- reactive({
    simsUpdate() #for dependency
    total
  })
  
  
  # needed for the conditional panels to work
  outputOptions(output, 'total', suspendWhenHidden=FALSE)
  
  output$barGraphInitial <- renderPlot({
    if (goodNames()) enable("resample") else disable("resample")
    validate(
      need(goodNames(),"Enter level names for each category, seperated by commas ( e.g. A,B,C,D )"
      ))
    
    
    if (goodNulls()) enable("resample") else disable("resample")
    validate(
      need(goodNulls(),"Enter your null probabilities as decimals seperated by commas. They should all be positive numbers")
    )
    
    if (goodNulls2()) enable("resample") else disable("resample")
    validate(
      need(goodNulls2(),"Your probabilities should be added up to 1.")
    )
    
    
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    
    # Names and Null Check
    lengthCheck <- length(names) == length(nulls)
    if (lengthCheck) enable("resample") else disable("resample")
    validate(
      need(lengthCheck,
           "Make sure that you enter the exact same number of level names and null probabilities")
    )
    
    
    if (goodObs()) enable("resample") else disable("resample")
    validate(
      need(goodObs(),"Enter at least two observed counts seperated by commas. All counts must be non-negative numbers")
    )
    
    
    lengthCheck2 <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    if (lengthCheck2) enable("resample") else disable("resample")
    validate(
      need(lengthCheck2,
           "Make sure that you enter the exact same number of level names, null probabilities and observed counts")
    )
    
    
    
    observed <- obsInput()
    expected <- nulls*sum(observed)
    tab <- rbind(observed,expected)
    rownames(tab) <-c("Observed","Expected")
    colnames(tab) <- names
    barplot(tab,beside=T,col=c("dark green","yellow"),
            main="Bargraph of Observed and Expected Counts",xlab="",ylab="Counts",
            legend.text=TRUE)
  })
  
  output$remarksInitial <- renderText({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames() 
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
      
    )
    
    
    
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    p.value <- pchisq(chisq, length(nulls)-1, lower.tail=FALSE) 
    rounded2<- round(p.value,3)
    paste("Observed Chi-Square Statistic =  ",as.character(rounded1),sep="\n",",",
          "P-value =  ",as.character(rounded2) )
  })
  
  output$obsTable <- renderTable({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs() && goodNulls2()) && goodNames()
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
    )
    
    
    
    expected <- nulls*sum(observed)
    contribs <- (observed-expected)^2/expected
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     cont=round(contribs,2)
    )
    names(df)[4] <- c("Contribution to Chi-Square")
    df
  }, align="c"
  )
  
  output$remarksLatest1 <- renderText({
    input$resample
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square statistic =  ",as.character(rounded1),sep="\n",",",
          "Latest resampled chi-square statistics = ",as.character(abs(rounded2)))
  })
  
  
  output$barGraphLatest <- renderPlot({
    input$resample
    if (length(chisqSims) > 0) {
      totalCounts <- isolate(sum(obsInput()))
      nulls <- isolate(nullsInput()/sum(nullsInput()))
      expected <- totalCounts*nulls
      tab <- rbind(obsInput(),expected,latestSim)
      rownames(tab) <-c("Observed","Expected","Resampled")
      colnames(tab) <- isolate(namesInput())
      barplot(tab,beside=T,col=c("dark green","yellow","grey"),
              main="Bargraph of Observed, Expected, and Latest Resample",xlab="",
              ylab="Counts",
              legend.text=TRUE)
    }
    
  })
  
  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500,from=0,to=xmaxInput(),bw=band,adjust = 3)
  })
  
  
  output$summary1 <- renderTable({
    input$resample
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    expected <- nulls*sum(observed)
    
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     Resampled=latestSim
    )
    df
  }, align = "c"
  
  )
  
  
  
  output$pvalueplot <-
    renderPlot({
      input$resample
      nulls <- nullsInput()/sum(nullsInput())
      observed <- obsInput()
      chisq <- obschisqInput()
      obs <- isolate(obschisqInput())
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- pchisq(chisqSims, length(observed)-1, lower.tail=FALSE)
      hist(p.value,breaks=10,main="P-value Distribution Histogram from the simulations", xlab="p-value", xlim=c(0,1),font.lab=2)
      
    })  
  
  output$chisqCurve <- renderPlot({
   
    sim<-input$sims
    
    if(sim<=5){
      obs <- obschisqInput()
      degFreedom <- dfInput()
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
      abline(v=0)
    }
    
    else{
      obs <- obschisqInput()
      degFreedom <- dfInput()
      
      chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
      abline(v=0)
      lines(chisqDensities(),col="#D95F02",lwd=3)
      
    }
    
    
    
  })
  
  output$remarksProb <- renderText({
    obs <- obschisqInput()
    paste0("The orange curve approximates the true probability distribution of the chi-square statistic based on simulations.", 
           " The black curve shows the large sample chi-square density.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the probability of each outcome is under Null probabilities (i.e. the p-value ).")
  })
  
})