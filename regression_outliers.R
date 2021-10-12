library(shiny)


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Outlier Impact"),

    # Sidebar with a slider input for number of obs
    sidebarLayout(

        sidebarPanel(
          sliderInput("samp","Sample Size (N):",
                      min=5,max=50,value=15),
          sliderInput("slope","Slope:",
                      min=-4,max=4,value=1, step=.5),
          sliderInput("intercept","Intercept:",
                      min=0,max=50,value=30, step=.5),
          div(HTML("<hr>")),
          sliderInput("xcoord","Outlier X-Coordinate:",
                      min=-100,max=100,value=0, step=.5),
          sliderInput("ycoord","Outlier Y-Coordinate:",
                      min=-100,max=100,value=0, step=.5),
          actionButton("reset", "Reset plotting area"),
          div(HTML("<br>")),
          actionButton("outlier", "Reset outlier"),

	#checkboxInput("choose", "Click plot to add outlier", FALSE)
	),

        # Show a plot of the generated regression
        mainPanel(
           plotOutput("distPlot", click="plot_click"),
	         htmlOutput("table"),
           tags$head(tags$style(HTML("p1 {margin-left: 15%}"))),
           HTML("<br><i><p1>Note. * p < .05, ** p < .01, *** p < .001</i></p1>")
        )

        )
    )


# Define server logic
server <- function(input, output, session) {
  
  rv <- reactiveValues(clickx=NULL, clicky=NULL, lastx=NULL, lasty=NULL)
  
  dataInput <- reactive({
  
    nobs <- input$samp
    tol <- 1e-3
    x <- 1:nobs
    
    continue <- TRUE
    while(continue) {
      y <- cbind(1,x) %*% c(input$intercept, input$slope) + rnorm(nobs,sd=5)
      if (sum((coef(lm(y ~ x)) - c(input$intercept, input$slope))^2) < tol) continue <- FALSE
    }
    
    return(list(x=x, y=y))
    
      #### CODE FOR CORRELATIONS, ANALYTIC
      #nobs <- input$samp
      #rho <- input$slope
      
      #y<-rnorm(nobs)
      #x<-1:nobs
      #y<-scale(y)
      #fit<- lm(x ~ y)
      #y.perp <- residuals(fit)
      #x<-rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
      #x<-scale(x)
  
      #return(list(x=x,y=y))
    
    })
  
  observeEvent(input$reset, {
    rv$lastx <- input$xcoord
    rv$lasty <- input$ycoord
  })
  
  observeEvent(input$outlier, {
    updateSliderInput(session,"xcoord", value = mean(dataInput()$x))
    updateSliderInput(session,"ycoord", value = mean(dataInput()$y))
    rv$clickx <- mean(dataInput()$x)
    rv$clicky <- mean(dataInput()$y)
  })
  
  observeEvent(input$samp,{
    updateSliderInput(session,"xcoord", value = mean(dataInput()$x))
    updateSliderInput(session,"ycoord", value = mean(dataInput()$y))
    rv$clickx <- mean(dataInput()$x)
    rv$clicky <- mean(dataInput()$y)
  })
  
  observeEvent(input$plot_click, {
  
    rv$clickx <- input$plot_click$x
    rv$clicky <-input$plot_click$y
    
    updateSliderInput(session,"xcoord", value = rv$clickx)
    updateSliderInput(session,"ycoord", value = rv$clicky)
  })
  
  observeEvent(input$xcoord, ignoreInit = TRUE, {
    rv$clickx <- input$xcoord
    rv$lastx <- c(rv$lastx, input$xcoord)
 #   rv$clicky <- input$ycoord
  })
  
  observeEvent(input$ycoord, ignoreInit = TRUE, {
 #   rv$clickx <- input$xcoord
     rv$clicky <- input$ycoord
     rv$lasty <- c(rv$lasty, input$ycoord)
  })
  
#  observeEvent(input$choose, {if (input$choose) shinyjs::disable(id=c("ycoord","xcoord"))  
#                else shinyjs::enable(id=c("ycoord","xcoord"))
#    })
  

  output$distPlot <- renderPlot({
    
    x<-dataInput()$x
    y<-dataInput()$y
    
    outx<-rv$clickx
    outy<-rv$clicky
      
    x1<-as.numeric(c(x, outx))
    y1<-as.numeric(c(y, outy))
    
    mod1<-lm(y~x)
    mod2<-lm(y1~x1)
    
    plot(x1,y1,xlab = "x",ylab="y", xlim=c(min(x, min(rv$lastx)), max(x, max(rv$lastx))), 
                                    ylim=c(min(y, min(rv$lasty)), max(y, max(rv$lasty))),
         main="Click the plot or use X and Y sliders to place/move outlier")
    
    points(outx, outy,col="red",pch=16)
    abline(mod1, col="blue")
    abline(mod2, col="red", lty=2)
  })
  
output$table <- renderUI({
  
    x<-dataInput()$x
    y<-dataInput()$y
    
    outx<-rv$clickx
    outy<-rv$clicky
    
    x1<-as.numeric(c(x, outx))
    y1<-as.numeric(c(y, outy))
    mod1<-lm(y~x)
    mod2<-lm(y1~x1)
    
    sign1 <- ifelse(coef(mod1)[2]<0,"-","+")
    sign2 <- ifelse(coef(mod2)[2]<0,"-","+")
    
    
    original_reg<-paste("Equation: y =",round(coef(mod1)[1],1), sign1,
                        abs(round(coef(mod1)[2],1)),"* x")
    
    original_cor<-paste0("Correlation: ", 
                        ifelse(abs(cor(x,y)-0)>.025,round(cor(x,y),2),0), ifelse(cor.test(x,y)$p.value<.001,"***",
                                                                 ifelse(cor.test(x,y)$p.value<.01,"**",
                                                                 ifelse(cor.test(x,y)$p.value<.05,"*",""))))
    
    #original_p<-paste("P-value:", round(cor.test(x,y)$p.value,3))
    
    outlier_reg<-paste("Equation: y =",round(coef(mod2)[1],1), sign2,
                       abs(round(coef(mod2)[2],1)),"* x")
    outlier_cor<-paste0("Correlation: ",ifelse(abs(cor(x1,y1)-0)>.025,round(cor(x1,y1),2),0),ifelse(cor.test(x1,y1)$p.value<.001,"***",
                                                                  ifelse(cor.test(x1,y1)$p.value<.01,"**",
                                                                         ifelse(cor.test(x1,y1)$p.value<.05,"*",""))))
    
    #outlier_p<-paste("P-value:", round(cor.test(x1,y1)$p.value,3))
    
    #HTML(paste(eq1, cor1, sep="<br>"))
    
    html.table <- tags$table(style = "border: 0px solid black; padding: 5%; width: 100%;margin-left:15%; margin-right:15%;",
                             tags$tr(
                               tags$th(style = "color: #0000ff","Original Data"),
                               tags$th(style = "color: #ff0000","With Outlier")
                               
                             ),
                             tags$tr(
                               tags$td(style = "color: #0000ff",original_reg),
                               tags$td(style = "color: #ff0000",outlier_reg)
                             ),
                             tags$tr(
                               tags$td(style = "color: #0000ff",original_cor),
                               tags$td(style = "color: #ff0000",outlier_cor)
                             )
    )

})


}


# Complete app with UI and server components
shinyApp(ui, server)




