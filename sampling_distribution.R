library(shiny)

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
        sliderInput("mean1","Population Mean (\u03bc):",
                    value=0,min=-50,max=50,step=1),
        sliderInput("sd1","Population Standard Deviation (\u03c3):",
                     value=1,
                     min=0,max=20,step=0.5),
        div(HTML("<hr>")),
        sliderInput("n1","Sample Size (N):",
                    value=25,
                    min=1,max=100,step=1),
        sliderInput("iter1","Iterations:",
                    value=0,min=0,max=5000,step=10,animate=T),
        checkboxInput("empsamp", "Show theoretical sampling distribution", value = FALSE, width = NULL),
        #checkboxInput("scale", "Zoom in on sampling distribution", value = FALSE, width = NULL),
        #h5(verbatimTextOutput("mean1")),	
        actionButton("resetMean", "Reset"),
      ),
    ),
    mainPanel(
                 plotOutput(outputId="popdistn",height=300),
                 plotOutput(outputId="sampdistn",height=300)

    )
  )
)

server <- function(input, output, session) {
  library(shinyjs)
  library(ggplot2)
  
  rv <- reactiveValues(xlims=NULL,xbreaks=NULL)

observeEvent(input$resetMean, {
    reset("form")
  })



dataInput <- reactive({
  
  select <- input$select
  n1 <- input$n1
  mean1<-input$mean1
  sd1<-input$sd1
  
  means <- replicate(5000,{mean(rnorm(n1, mean1, sd1))})

  
  return(list(means=means) )
  
})


  output$popdistn <- renderPlot(height=300, {
    
    mean1<-input$mean1
    sd1<-input$sd1

    p1 <- ggplot()+stat_function(geom="area",fun=dnorm, args=list(mean=mean1, sd=sd1), fill="gray50")+
      xlim(mean1-3.5*sd1,mean1+3.5*sd1)+theme_light()+ggtitle("Population Distribution")+
      annotate("text",x=-Inf,y=Inf, hjust="left", vjust="top", label=paste0("\n   μ = ",mean1,"\n","   σ = ",sd1), size=5)+
      ylab("")
    
    #rv$xlims <- ggplot_build(p1)$layout$panel_params[[1]]$x.range
    return(p1)

  })
  
  
  output$sampdistn <- renderPlot(height=300,{

    mean1<-input$mean1
    sd1<-input$sd1
    
    p2<-ggplot()+geom_histogram(aes(x=dataInput()$means[0:5000]), color="gray50",bins=100)+xlim(mean1-3.5*sd1,mean1+3.5*sd1)
    
    p<-ggplot()+geom_histogram(aes(x= dataInput()$means[0:input$iter1]), color="gray50",bins=100)+xlim(mean1-3.5*sd1,mean1+3.5*sd1)+
      theme_light()+ggtitle(paste0("\nSampling Distribution of the Mean (N = ",input$n1,")"))+ylim(0,suppressWarnings(max(ggplot_build(p2)$data[[1]]$count))*1.1)+
    xlab(bquote(bar(x)))
    
    if(input$iter1>0){
      p<-p+annotate("text",x=-Inf,y=Inf, hjust="left", vjust="top",
               label=paste0("\n   mean = ",round(mean(dataInput()$means[0:input$iter1]),2),"\n",
                            "   sd = ",round(sd(dataInput()$means[0:input$iter1]),3)), size=5)+ylab("")
      suppressWarnings(print(p))
      }
    else suppressWarnings(print(p))
    
    if(input$empsamp){
      
    #label1 <- parse(text=paste(parse(text=paste0("μ[bar(x)] == ", round(mean1, 2))),"\n",parse(text=paste0("μ[bar(x)] == ", round(mean1, 2))))  
    #binwidth<- ggplot_build(p)$data[[1]]$xmax - ggplot_build(p)$data[[1]]$xmin
      bw = suppressWarnings(diff(range(ggplot_build(p)$data[[1]]$xmax,na.rm=T))/nrow(ggplot_build(p)$data[[1]]))
      n_obs = sum(!is.na(dataInput()$means[0:input$iter1]))
      
      p<- p+ stat_function(geom="line", fun=function(x)dnorm(x,mean=input$mean1,sd=input$sd1/sqrt(input$n1))*n_obs*bw,size=1,color="blue")+
        ylab("")+xlab(bquote(bar(x)))
       
    }
    
      if(FALSE) suppressWarnings(print(p))
      else suppressWarnings(print(p))
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
