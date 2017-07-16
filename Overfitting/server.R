library(shiny)
library(shinythemes)
shinyServer(function(input, output,session) {
  
  plotdata<-reactive({
    n=input$n
    p=input$p
    k=input$k
    xmat=matrix(0,n,k)
    
    if(input$plot>0){
      y1<-rnorm(n,0,1)
      if(p!=0){
        x1=rnorm(n,y1,1/abs(p))
        x2=rnorm(n,y1,1/abs(p))
      }else{
        x1=rnorm(n,0,1 )
        x2=rnorm(n,0,1 )
      }
      if(p!=0){
        for(i in 1:k)
          xmat[,i]=rnorm(n,y1,1/abs(p))
      }
      else{
        for(i in 1:k)
          xmat[,i]=rnorm(n,0,1)
      }
      
      R2=0
      for(i in 1:k)
        R2[i]=cor( y1,xmat[,i])
      
      data<-list(R2,y1,xmat)
    }
    data
  })
  
  bluey<-reactive({
    n=input$n
    if(input$validate>0)
      y<-rnorm(n,0,1)
    y
  })
  
  plot2<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2)) #best
    
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    
    d2<-density(y2-mm$fitted.values) # Pick the best X
    plot(range(d2$x), range(d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main="")
    lines(d2, col="black",lwd=2)
  })
  
  plot1<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2))
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    
    d2<-density(y2-mm$fitted.values) # Pick the best X
    
    y1blue<-bluey()
    kkblue=which.max(abs(R2))
    xmatblue=array(unlist(mydata[3]),dim=c(n,k)) 
    mm2blue<-lm(y1blue~xmatblue[, k-kkblue+1]) # Randomly Chosen X
    d1<-density(y1blue-mm2blue$fitted.values, adjust=2) # Randomly Chosen X)
    plot(range(d1$x,d2$x), range(d1$y,d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main="")
    lines(d2, col="black",lwd=2)
    lines(d1, col="blue",lwd=2)
  })
  
  scatterplot<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    
    kk=which.max(abs(R2))
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    plot(xmat[,kk], y2, xlab="Best Chosen X", ylab="Y")
    abline(mm,col="red")
  })
  
  
  observeEvent(input$plot,output$plott<-plot2)
  observeEvent(input$plot,output$scatter<-scatterplot)
  observeEvent(input$plot,output$choose<-value1)
  
  observeEvent(input$validate,output$plott<-plot1)
  observeEvent(input$validate,output$choose<-value2)
  
  value11<-reactive({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2)) #best
    best<-cor(y2,xmat[, kk])
    xx=as.data.frame(best)
    colnames(xx)=c("Sample Best Correlation")
    xx
  })
  value1<-renderTable({
    value11()},
    align="c"
  )
  value22<-reactive({
    n=input$n
    k=input$k
    best<-value11()
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    kkblue<-which.max(abs(R2))
    xmatblue=array(unlist(mydata[3]),dim=c(n,k))
    y1blue<-bluey()
    random<-cor(y1blue,xmatblue[, k-kkblue+1])
    xx=cbind(best,random)
    xx=as.data.frame(xx)
    colnames(xx)=c("Sample Best Correlation","Sample Randomly Chosen Correlation")
    xx
  })
  value2<-renderTable({
    value22()},
    align="c"
  )
  
})