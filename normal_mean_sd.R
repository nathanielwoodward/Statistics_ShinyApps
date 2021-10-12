ui <- fluidPage(
titlePanel("The Normal Distribution"),
  
 sidebarLayout(
    # Sidebar to demonstrate various slider options ----
  sidebarPanel(
  #div(HTML("Set Mean, Standard Deviation<br><hr>")),
  sliderInput("mu1", "Mean:", value=0, min = -5, max = 5,step=0.25),
  checkboxInput("showmu", "Show mean (red)", FALSE),
  div(HTML("<hr>")),
  sliderInput("sd1","Standard Deviation",value=1,min=0,max=3,step=0.1),
  checkboxInput("showsd", "Show ± 1 sd (blue)", FALSE)
),
mainPanel(
  plotOutput("distplot"),
  )
)
)

server <- function(input, output) {
  
  library(ggplot2); library(dplyr)
  output$distplot <- renderPlot({
  
  # base R version  
  #.xlow <- round(input$mu1 - 5*input$sd1,2)
  #.xupp <- round(input$mu1 + 5*input$sd1,2)
  #.x <- seq(.xlow,.xupp,.01)
  #.y <- dnorm(.x,mean=input$mu1,sd=input$sd1)
  #plot(.x,.y,type="l",xlim=c(.xlow,.xupp),axes=FALSE)
  #axis(1,pos=0)
    
  plot<- ggplot()+stat_function(fun=dnorm, args=list(mean=input$mu1, sd=input$sd1),size=1)+
    scale_x_continuous(lim=c(-5,5),breaks=seq(-5,5,1))+scale_y_continuous(lim=c(0,ifelse(input$sd1>.8,.5,dnorm(input$mu1,input$mu1,input$sd1)+.02)))+
    xlab("")+ylab("")+theme_light(base_size=15)
  
  if(input$showmu & !input$showsd){
    plot+geom_segment(aes(x=input$mu1,xend=input$mu1,y=0,yend=dnorm(input$mu1,input$mu1,input$sd1)),color="red",lty=2)
  }
  else if(!input$showmu & input$showsd){
    plot+geom_segment(aes(x=input$mu1+input$sd1,xend=input$mu1+input$sd1,y=0,yend=dnorm(input$mu1+input$sd1,input$mu1,input$sd1)),color="blue",lty=2)+
         geom_segment(aes(x=input$mu1-input$sd1,xend=input$mu1-input$sd1,y=0,yend=dnorm(input$mu1-input$sd1,input$mu1,input$sd1)),color="blue",lty=2)
  }
  else if(input$showmu & input$showsd){
    plot+geom_segment(aes(x=input$mu1,xend=input$mu1,y=0,yend=dnorm(input$mu1,input$mu1,input$sd1)),color="red",lty=2)+
      geom_segment(aes(x=input$mu1+input$sd1,xend=input$mu1+input$sd1,y=0,yend=dnorm(input$mu1+input$sd1,input$mu1,input$sd1)),color="blue",lty=2)+
      geom_segment(aes(x=input$mu1-input$sd1,xend=input$mu1-input$sd1,y=0,yend=dnorm(input$mu1-input$sd1,input$mu1,input$sd1)),color="blue",lty=2)
  }
  else{plot}
                    })



}
shinyApp(ui, server)

