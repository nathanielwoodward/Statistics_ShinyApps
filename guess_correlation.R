library(shiny)
library(shinyjs)

ui <- fluidPage(

br(),
 sidebarLayout(

    # Sidebar panel for inputs ----

sidebarPanel(
br(),
  shinyjs::useShinyjs(),
   div(
      id = "form",
	numericInput("cor1","Input Guess:",
	value=NA,
	min=-1,max=1,step=0.01),
	actionButton("gues1","Submit Guess"),
	div(HTML("<hr>")),
  actionButton("round1", "Next Round"),
	actionButton("reset1","Reset All"),
	div(HTML("<hr>"))),
	textOutput("out2")
),
    mainPanel(
# tabsetPanel(
#tabPanel("Scatterplot",      
plotOutput(outputId="init1"),
h5(verbatimTextOutput("out1"))
)
)
)


server <- function(input, output, session) {
library(shinyjs)
library(MASS)
library(beepr)

rv <- reactiveValues(x=NULL,y=NULL,z=NULL,a=NULL, score=0,count=0)


observeEvent(input$reset1, {
  reset("form")
  show("gues1")
  rv$score <- 0
  rv$count <- 0
  output$init1 <- renderPlot({
    
    .xc1 <- seq(-1,1,by=0.01)
    
    .xc2 <- sample(.xc1,1)
    
    .Sig1 <- matrix(c(1,.xc2,.xc2,1),2,2)
    .xdist <- mvrnorm(n=100,mu=c(0,0),Sigma=.Sig1)
    .xcor <- cor(.xdist)[1,2]
    plot(.xdist,xlab="",ylab="", main="Guess the Correlation!", pch=19)
    rv$x <- .xcor
    rv$y <- .xdist
    rv$a <- 0
})

})
   
  
observeEvent(input$round1, {
    reset("form")
    show("gues1")

output$init1 <- renderPlot({

      .xc1 <- seq(-1,1,by=0.01)

      .xc2 <- sample(.xc1,1)

      .Sig1 <- matrix(c(1,.xc2,.xc2,1),2,2)
      .xdist <- mvrnorm(n=100,mu=c(0,0),Sigma=.Sig1)
      .xcor <- cor(.xdist)[1,2]
      plot(.xdist,xlab="",ylab="", main="Guess the Correlation!", pch=19)
      rv$x <- .xcor
      rv$y <- .xdist
      rv$a <- 0

	    })

})


     observeEvent(input$gues1, {
      rv$z <- input$cor1
      rv$a <- 1
      rv$count <- rv$count+1
      rv$score[rv$count] <- abs(rv$x-rv$z)
      #reset("form")  
      hide('gues1')
      })
      


output$init1 <- renderPlot({

      .xc1 <- seq(-1,1,by=0.01)

      .xc2 <- sample(.xc1,1)

      .Sig1 <- matrix(c(1,.xc2,.xc2,1),2,2)
      .xdist <- mvrnorm(n=100,mu=c(0,0),Sigma=.Sig1)
      .xcor <- cor(.xdist)[1,2]
      plot(.xdist,xlab="",ylab="", main="Guess the Correlation!",pch=19)
      rv$x <- .xcor
      rv$y <- .xdist
      rv$a <- 0
      
	    })



    output$out1 <- renderPrint({

       if(rv$a==1){
 if(abs(rv$x-rv$z)<=0.05) beep(3)
 else if(abs(rv$x-rv$z)<=0.15) beep(1)
 cat("Your Guess:", rv$z,"\n")
 cat("Actual Correlation: ",round(rv$x,3),"\n \n")
 
 cat("Your guess was", round(abs(rv$x-rv$z),3), "away! \n")
 cat("Average error after", rv$count, "games:", round(mean(rv$score),3))
	    } else {
	    cat("\n")
rv$a<-0
}
		 })

    
    output$out2 <- renderPrint({
      if(rv$count>0){
        cat("Avg error after", rv$count, "games: \n", round(mean(rv$score),3))
      } else {
        cat("\n")
      }
    })

}












shinyApp(ui, server)
