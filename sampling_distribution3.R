library(shiny)
library(animation)
library(e1071)
library(shinyjs)
library(matrixStats)
ui <- fluidPage(
  titlePanel(
    "Sampling Distributions"
  ),
  br(),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      shinyjs::useShinyjs(),
      div(
        id = "form",
        selectInput("select","Population Distribution", 
                    choices = list("Normal" = "norm", "Uniform/Flat" = "unif", "Right Skew" = "right", "Left Skew"= "left"), 
                    selected = "norm"),
        sliderInput("mean1","Population Mean (\u03bc):",
                    value=0,min=-50,max=50,step=1),
        sliderInput("sd1","Population Standard Deviation (\u03c3):",
                     value=1,
                     min=1,max=20,step=0.5),
        sliderInput("n1","Sample Size (N):",
                    value=25,
                    min=0,max=100,step=1),
        sliderInput("iter1","Iterations:",
                    value=0,min=0,max=5000,step=10,animate=T),
        checkboxInput("empsamp", "Show theoretical sampling distribution", value = FALSE, width = NULL),
        checkboxInput("scale", "Zoom in on sampling distribution", value = FALSE, width = NULL),
        #h5(verbatimTextOutput("mean1")),	
        actionButton("resetMean", "Reset"),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        )
      ),
    ),
    mainPanel( tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
                 plotOutput(outputId="popdistn",height=300),
                 plotOutput(outputId="sampdistn",height=300)

    )
  )
)

server <- function(input, output, session) {
  library(shinyjs)
  library(ggplot2)
  
  rv <- reactiveValues(xlims=NULL,count=0)

  
observeEvent(input$resetMean, {
    reset("form")
  })

observeEvent(input$select, {
  
  dataInput<-NULL

  if(input$select=="right") {
    updateSliderInput(session, "mean1", value = 2, min = 1, max=10, step=.5)
    updateSliderInput(session, "sd1", value = 1, min = 1, max=5, step=.25)
    updateSliderInput(session, "n1", value = 25, min = 1, max=1000, step=1)
  }
  
  else if(input$select=="left"){
    updateSliderInput(session, "mean1", value = .8, min = .7,max=.9,step=.01)
    updateSliderInput(session, "sd1", value = .12, min = 0,max=.2,step=.01)
    updateSliderInput(session, "n1", value = 25, min = 1, max=1000, step=1)
    mean1<-NULL
    sd1<-NULL
  }
  
  else if(input$select %in% c("norm","unif")){
    updateSliderInput(session, "mean1", value = 0, min = -50,max=50,step=.5)
    updateSliderInput(session, "sd1", value = 1, min = 0,max=20,step=.5)
    updateSliderInput(session, "n1", value = 25, min = 1, max=100, step=1)
  }
 # else if(input$select %in% c("bimod")){
#    updateSliderInput(session, "mean1", value = 0, min = -50,max=50,step=.5)
#    updateSliderInput(session, "sd1", value = 1, min = 0,max=20,step=.5)
#    updateSliderInput(session, "n1", value = 25, min = 1, max=100, step=1)
#  }
  
})


dataInput <- reactive({
  
  select <- input$select
  n1 <- input$n1
  mean1<-input$mean1
  sd1<-input$sd1
  
  if(select=="norm") means <- replicate(5000,{mean(rnorm(n1, mean1, sd1))})
  else if(select=="unif") means <- replicate(5000,{mean(runif(n1, mean1-sd1*sqrt(3), mean1+sd1*sqrt(3) ))})
  else if(select=="right") if(mean1<=0)return(NULL) else means <- replicate(5000,{mean(rlnorm(n1, log((mean1^2)/(sqrt(mean1^2+sd1^2))), sqrt(log(1+(sd1^2/mean1^2))) ))})
  else if(select=="left") if(sd1^2>mean1*(1-mean1))return(NULL)else means <- replicate(5000,{mean(rbeta(n1, mean1*(((mean1*(1-mean1))/sd1^2)-1), (1-mean1)*(((mean1*(1-mean1))/sd1^2)-1)))})
  #else if(select=="bimod") means <- replicate(5000,{
  #  x<-(runif(n1)>.5)
  #  mean_1 <- 0
  #  sd_1 <-1
  #  mean2 <- 2*mean1-mean_1
  #  sd2 <- sqrt(2*sd1^2-1*sd_1^2-.5*(mean_1-mean1)^2)
  #  if(sd2>0) mean(c(rnorm(n1,mean_1,sd_1)[x],rnorm(n1, mean2, sd2)[!x]))
  #  })
  
  return(list(means=means))
  })


  output$popdistn <- renderPlot(height=300, {
    
    mean1<-input$mean1
    sd1<-input$sd1

    if(input$select=="norm"){
    p1 <- ggplot()+stat_function(geom="area",fun=dnorm, args=list(mean=mean1, sd=sd1), fill="gray50")+
      xlim(mean1-3*sd1,mean1+3*sd1)+theme_light()
    }
    else if(input$select=="unif"){
    p1 <-  ggplot()+stat_function(geom="area",fun=dunif, args=list(min= mean1-sd1*sqrt(3), max= mean1+sd1*sqrt(3)), fill="gray50")+
        theme_light()+xlim(mean1-sd1*sqrt(3),mean1+sd1*sqrt(3))
    }
    else if(input$select=="right"){
      
     m1=log((mean1^2)/(sqrt(mean1^2+sd1^2)))
     s1=sqrt(log(1+(sd1^2/mean1^2)))
      
    p1<-  ggplot()+stat_function(geom="area",fun=function(x)suppressWarnings(dlnorm(x,m1, s1)), fill="gray50")+
        theme_light() +xlim(.01,suppressWarnings(qlnorm(.99,m1,s1)))
    }
  
    else if(input$select=="left"){
      
      #updateSliderInput(session, "sd1", min = 0,max=input$mean1*(1-input$mean1),step=.05)
      
      a=mean1*(((mean1*(1-mean1))/sd1^2)-1)
      b=(1-mean1)*(((mean1*(1-mean1))/sd1^2)-1)
      
    p1<-  ggplot()+stat_function(geom="area",fun=function(x)suppressWarnings(dbeta(x,shape1=a ,shape2=b)), fill="gray50")+
        theme_light()
    }
    
    #else if(input$select=="bimod"){
      
      #updateSliderInput(session, "sd1", min = 0,max=input$mean1*(1-input$mean1),step=.05)
      
    #  mean_1 <- 0
    #  sd_1 <-1
      
    #  mean2<-2*mean1-mean_1
    #  sd2<-sqrt(2*sd1^2-1*sd_1^2-.5*(mean_1-mean1)^2)
      
    #p1<-  ggplot()+stat_function(geom="area",fun=function(x).5*dnorm(x, mean=mean_1,sd=sd_1)+.5*dnorm(x,mean2, sd2), fill="gray50")+
     #   theme_light()+xlim(qnorm(.005,0,1),qnorm(.995,mean2,sd2))
    #}
    
    rv$xlims <- ggplot_build(p1)$layout$panel_params[[1]]$x.range
    
    p1<-p1+annotate("text",x=-Inf,y=Inf, hjust="left", vjust="top", label=paste0("\n   μ = ",mean1,"\n","   σ = ",sd1), size=5)+
      ylab("")+ggtitle("Population Distribution")
    
    suppressWarnings(print(p1))

  })
  
  
  output$sampdistn <- renderPlot(height=300,{
    
    m=log((mean1^2)/(sqrt(mean1^2+sd1^2)))
    s=sqrt(log(1+(sd1^2/mean1^2)))
    
    a=mean1*(((mean1*(1-mean1))/sd1^2)-1)
    b=(1-mean1)*(((mean1*(1-mean1))/sd1^2)-1)
    
   p2<-ggplot()+geom_histogram(aes(x=dataInput()$means[1:5000]), color="gray50",bins=100)
    
   p<-ggplot()+geom_histogram(aes(x=dataInput()$means[0:input$iter1]), color="gray50",bins=100)
   
   if(input$scale){    
     if(input$select=="norm") p<-p #+xlim(input$mean1-3*input$sd1, input$mean1+3*input$sd1)
     else if(input$select=="unif") p<-p #+xlim(input$mean1-input$sd1*sqrt(3), input$mean1+input$sd1*sqrt(3))
     else if(input$select=="right") p<-p #+xlim(0,qlnorm(.999,m,s))
     else if(input$select=="left") p<-p #+xlim(0,1)
     else if (input$select=="bimod") p<-p
   }
   else {p<-p+xlim(rv$xlims); p2<-p2+xlim(rv$xlims)}
   
   p<- p+ylim(0,suppressWarnings(max(ggplot_build(p2)$data[[1]]$count))*1.1)+
      theme_light()+ggtitle(paste0("\nSampling Distribution of the Mean (N = ",input$n1,")"))+ylab("")+xlab(bquote(bar(x)))
   if(input$iter1>0){
   p<-p+annotate("text",x=-Inf,y=Inf, hjust="left", vjust="top",
               label=paste0("\n   mean = ",suppressWarnings(round(mean(dataInput()$means[0:input$iter1]),2)),"\n",
                            "   sd = ",round(sd(dataInput()$means[0:input$iter1]),3)), size=5)}
    

    if(input$empsamp){
      bw = suppressWarnings(diff(range(ggplot_build(p)$data[[1]]$xmax,na.rm=T))/nrow(ggplot_build(p)$data[[1]]))
      n_obs = sum(!is.na(dataInput()$means[0:input$iter1]))
      
     p<- p+ stat_function(geom="line", fun=function(x)dnorm(x,mean=input$mean1,sd=input$sd1/sqrt(input$n1))*n_obs*bw,size=1,color="blue")+
       ylab("")+xlab(bquote(bar(x)))
       
    }
   
   suppressWarnings(print(p))
                                  
                                      
    
  }
  )

  output$mean1 <- renderPrint({
    cat("Population Distribution","\n")
    cat(paste("mean = ",round(input$mean1,2),sep=""),"\n")
  #  cat(paste("median = ",round(dataInput()$zm,2),sep=""),"\n")
    cat(paste("standard deviation = ",round(input$sd1,2),sep=""),"\n")
    cat(NULL,"\n")
    cat("Sampling Distribution","\n")
    cat(paste("mean of means = ",round(mean(dataInput()$means,2)),sep=""),"\n")
    cat(paste("sd of means = ",round(sd(dataInput()$means),4),sep=""),"\n")
    cat(NULL,"\n")
   # cat("Sampling Distribution (n=25)","\n")
  #  cat(paste("mean of means = ",round(dataInput()$m25,2),sep=""),"\n")
  #  cat(paste("sd of means = ",round(dataInput()$sd25,2),sep=""),"\n")
    #		 if(input$desc1[1]=="skew1" | input$desc1[2]=="skew1")cat(paste("skew = ",round(dataInput()$sk1,2),sep=""),"\n")
    #    if(any(input$desc1=="skew1") )cat(paste("skew = ",
    #                                            round(dataInput()$sk1,2),sep=""),"\n")
    #   if(any(input$desc1=="kurt1"))cat(paste("kurt = ",
    #                                          round(dataInput()$kurt2,2),sep=""),"\n")
  }
  )
}

shinyApp(ui, server)
