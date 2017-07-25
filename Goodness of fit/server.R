library(shiny)
library(ggplot2)
function(input, output) {
  
  #For Random
  firstdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    mytable<-list(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      mytable[i]<-list(x)
    }
    mytable
  })
  
  #For Same
  firstdata2<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    mytable<-list(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      mytable[i]<-list(x)
    }
    mytable
  })
  
  # For Random
  plotdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    pp=numeric(0)
    xx<-firstdata()
    for(i in 1:ss){
      x<-unlist(xx[i])
      gen<-runif(input$n2)
      trueProp = gen/sum(gen)
      total=table(x)
      expected=trueProp*input$n
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    if (ss<=50) {
      index=seq(1,length(pp))
      data=data.frame(index,pp)
    }
    else
    {
      data=data.frame(pp)
    }
    list(x = data, y = gen)
  })
  
  # For Same
  plotdata2<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    v$click1 <-NULL
    pp=numeric(0)
    xx<-firstdata2()
    for(i in 1:ss){
      x<-unlist(xx[i])
      nulls=1/(1:nn)
      total=table(x)
      expected=nulls*total
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    if (ss<=50) {
      index=seq(1,length(pp))
      data=data.frame(index,pp)
    }
    else
    {
      data=data.frame(pp)
    }
    data
  })
  
  #click_event
  v <- reactiveValues(click1 = NULL)
  
  observeEvent(input$plot_click, {
    v$click1 <- input$plot_click
  })
  
  
  
  # For Random
  output$plot1 <- renderPlot({
    ss= input$n3
    nn= input$n2
    d<-plotdata()$x
    if (ss<=50)
    {
      plot(d$index,d$pp,xlab = "Simulation Number",
           ylab = "P-value", main="P-value Distribution from the simulations", pch = 20,col="#1C2C5B", cex=2.5,font.lab=2 )
      if (!is.null(v$click1$x))
        points(x=round(v$click1$x),y=d$pp[round(v$click1$x)],col="#FF4500",pch = 20, cex=3)
      
    }
    else {
      par(xpd=F)
      hist(d$pp,breaks=5,main="P-value Distribution from the simulations", xlab="P-value",font.lab=2 )
      abline(h = ss/5, col = "red")}
  })
  
  # For Same
  
  output$plot2 <- renderPlot({
    ss= input$n3
    nn= input$n2
    d<-plotdata2()
    if (ss<=50)
    {
      plot(d$index,d$pp,xlab = "Simulation Number",
           ylab = "P-value", main="P-value Distribution from the simulations", pch = 20,col="#1C2C5B", cex=2.5,font.lab=2 )
      if (!is.null(v$click1$x))
        points(x=round(v$click1$x),y=d$pp[round(v$click1$x)],col="#FF4500",pch = 20, cex=3)
      
    }
    else {par(xpd=F)
      hist(d$pp,breaks=5,main="P-value Distribution from the simulations", xlab="P-value",font.lab=2 )
      abline(h = ss/5, col = "red")}
  })
  
  
  # For Random
  clickedpoints1<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata()  
    
    data<-plotdata()$x
    if (is.null(v$click1)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    x1<-unlist(mytable[i])
    # gen<-runif(input$n2)
    trueProp = plotdata()$y/sum(plotdata()$y)
    total=table(x1)
    expected=trueProp*input$n
    xx=cbind(paste0(LETTERS[1:nn]),table(x1),round(expected,2),
             round(table(x1)/sum(table(x1)),2), 
             round(round(expected,2)/sum(round(expected,2)),3))
    
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(table(x1)),sum(round(rep(num_of_samples/nn,nn),2)),"1","1")
    xx
    
    
  })
  
  # For Same
  clickedpoints2<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata2()  
    
    data<-plotdata2()
    if (is.null(v$click1)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    x1<-unlist(mytable[i])
    
    xx=cbind(paste0(LETTERS[1:nn]),table(x1),round(rep(num_of_samples/nn,nn),2),
             round(table(x1)/sum(table(x1)),2), 
             round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3))
    
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Observed Value","Expected Value", "Observed Proportion", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(table(x1)),sum(round(rep(num_of_samples/nn,nn),2)),"1", "1")
    xx
  })
  
  # For Random
  clickedpoints21<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata()  
    
    data<-plotdata()$x
    if (is.null(v$click1)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    paste("P-value =  ",as.character(pvalue) )
    
  })
  
  # For Same
  clickedpoints22<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    ss= input$n3
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata2()  
    
    data<-plotdata2()
    if (is.null(v$click1)||ss>50)
      return()
    i<-round(v$click1$x)
    pvalue<-round(data$pp[round(v$click1$x)],3)
    
    paste("P-value =  ",as.character(pvalue) )
    
  })
  
  # For Same
  output$plot_clickedpoints<-renderTable({
    clickedpoints2()},
    align="c"
  )
  
  # For Random
  output$plot_clickedpoints2<-renderTable({
    clickedpoints1()},
    align="c"
  )
  
  # For Random
  output$text1<- renderText({
    clickedpoints21()
    
  })
  
  # For Same
  output$text2<- renderText({
    clickedpoints22()
    
  })
  
  # For Random
  sliderValues <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x1 <- sample(1:nn,num_of_samples,replace=T)
    gen<-runif(input$n2)
    trueProp = gen/sum(gen)
    total=table(x1)
    expected=trueProp*input$n
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),round(expected,2), round(round(expected,2)/sum(round(expected,2)),3)) 
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Expected Value", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(round(expected,2)),"1")
    xx
    
  })
  
  # For Same
  
  sliderValues2 <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x1 <- sample(1:nn,num_of_samples,replace=T)
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),round(rep(num_of_samples/nn,nn),2), round(round(rep(num_of_samples/nn,nn),2)/sum(round(rep(num_of_samples/nn,nn),2)),3)) 
    xx=as.data.frame(xx,stringsAsFactors=FALSE)
    colnames(xx)=c("Categories","Expected Value", "Expected Proportion")
    xx[nrow(xx)+1,] <- c("Total", sum(round(rep(num_of_samples/nn,nn),2)),"1")
    xx
  })
  # For Random
  output$values1 <- renderTable({
    sliderValues()},
    align="c"
  )
  
  # For Same
  
  output$values2 <- renderTable({
    sliderValues2()},
    align="c"
  )
}