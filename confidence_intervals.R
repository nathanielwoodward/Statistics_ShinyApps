library(shiny)
library(shinyjs)

ui <- fluidPage(
  titlePanel("Confidence Intervals"),
  
  sidebarLayout(
    sidebarPanel(
   useShinyjs(),
   div(id="form",
  sliderInput("samp","Sample Size (N):",
              min=5,max=100,value=10
              ),
  sliderInput("mu","Population Mean (\u03bc):",
              min=-30,max=30,value=30),
  actionButton("run",
               "New Samples"),
  div(HTML("<hr>")),
  sliderInput("conf","Confidence Level (1 - \u03b1):",
              min=0.5,max=0.99,step=0.01,
              value=0.95
              ),
  sliderInput("iter","Plot Samples:", value=100, animate=T,min = 0,max=100,step=1)
  
),
actionButton("resetAll",
             "Reset All"),

),
  mainPanel(
  plotOutput("distPlot")
  )
)
)


server <- function(input, output) {
  
  library(ggplot2)

  observeEvent(input$resetAll, {
    reset("form")
  })
  
  size=100
  
  newdata <-reactive({
    input$run
    isolate({
    dat <- matrix(input$mu+rnorm(size*input$samp), nrow=input$samp)
    dat <- data.frame(mean=apply(dat,2,mean),sd=apply(dat,2,sd))
    dat$x <- 1:size
    dat$n <- input$samp
    dat$mu <- input$mu
    })
    return(dat)
  })


  output$distPlot <- renderPlot({
    
    input$run
    
    iter <- input$iter
    newdata <- newdata()
    newdata$se <- newdata$sd/sqrt(newdata$n)
    newdata$ymin <- newdata$mean-qt(1-(1-input$conf)/2,newdata$n-1)*newdata$se
    newdata$ymax <- newdata$mean+qt(1-(1-input$conf)/2,newdata$n-1)*newdata$se
    
    ggplot(newdata[1:iter,], aes(x,mean))+
      geom_hline(aes(yintercept=mu), lty=2)+
      geom_point(aes(color=ifelse(ymin>mu | ymax < mu, "red","black")))+
      geom_errorbar(aes(ymin=ymin, ymax=ymax,
                        color=ifelse(ymin>mu | ymax < mu, "red","black")))+
      xlim(0,max(30,input$iter+.5))+ylim(min(newdata$mu)-5*max(newdata$se),max(newdata$mu)+5*max(newdata$se))+
      scale_color_manual(values=c("black","red"))+
      theme_light(base_size=14)+
      theme(legend.position = "none")+ylab(expression(paste(bar(x)," \u00B1 ", t[crit], " \u00D7 ","SE")))+xlab("Sample")+
      ggtitle(paste("Proportion of",paste(100*input$conf,"%",sep=""),"CIs containing \u03bc =",newdata$mu,"after",iter,"trials:",round(mean(newdata$ymin[1:iter]<newdata$mu[1] & newdata$ymax[1:iter] > newdata$mu[1]),3)))
    
    #ggplot(mean_sd, aes(iter,mean))+geom_point()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
