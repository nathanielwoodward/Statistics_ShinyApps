library(shiny)

sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
    tags$form(class = "well", ...),
    out
  )
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(



# App title ----
  titlePanel("Visualizing Distributions"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel2(fluid=FALSE,

    #Input Data Distribution
    
	radioButtons(inputId="dist1",label="Distribution:",
	       choices=list("Normal" = "Normal", 
	       "Left-Skewed" = "Left",
	       "Right-Skewed" = "Right",
	       "Uniform" = "Uniform")),


      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 25,
                  value = 10),
		  

	    checkboxInput("points", "Show raw data", FALSE),
	    checkboxInput("box", "Overlay boxplot", FALSE),
	    checkboxInput("mean", "Show mean", FALSE),
	
        actionButton("resetMean", "Reset Data"),
	h5(verbatimTextOutput("mean1")),
      downloadButton("down", "Download the file"),
      



    ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "distPlot")
      #plotOutput(outputId = "altPlot")
      )
   
)
)


# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
  
  
  rv <- reactiveValues(x=NULL,y=NULL)
  
  
  
  
  dataInput <- reactive({
    
    
    
    observeEvent(input$resetMean, {
      
      session$reload()
      return()
      
    })
    
    observeEvent(input$dist1, {
      
      rv$x <- dataInput()$u
      rv$y <- dataInput()$x
      
    })
    
    
    
    
    out1 <- function(x) {
      bq1 <- quantile(x,prob=0.25)
      bq3 <- quantile(x,prob=0.75)
      iqr1 <- bq3 - bq1
      low1 <- bq1 - 1.5*iqr1
      upp1 <- bq3 + 1.5*iqr1
      .w <- x>upp1 | x<low1
      return(.w)
    }
    
    
    g <- function() {
      x <- rnorm(n=75)
      .y <- out1(x)
      rv$x <- 1:length(x)
      rv$y <- x
      zzz <- list(x=x,mn1=mean(x),med1=median(x),sd1=sd(x),
                  bq1=quantile(x,prob=0.25),bq3=quantile(x,prob=0.75),
                  iqr1=quantile(x,prob=0.75)-quantile(x,prob=0.25),w=.y,
                  u=1:length(x))
      
      return(zzz)
    }
    h <- function() {
      x <- rexp(n=75)
      .y <- out1(x)
      rv$x <- 1:length(x)
      rv$y <- x
      
      zzz <- list(x=x,mn1=mean(x),med1=median(x),sd1=sd(x),
                  bq1=quantile(x,prob=0.25),bq3=quantile(x,prob=0.75),
                  iqr1=quantile(x,prob=0.75)-quantile(x,prob=0.25),w=.y,
                  u=1:length(x))
      
      return(zzz)
    }
    k <- function() {
      x <- runif(n=75)
      .y <- out1(x)
      rv$x <- 1:length(x)
      rv$y <- x
      
      zzz <- list(x=x,mn1=mean(x),med1=median(x),sd1=sd(x),
                  bq1=quantile(x,prob=0.25),bq3=quantile(x,prob=0.75),
                  iqr1=quantile(x,prob=0.75)-quantile(x,prob=0.25),w=.y,
                  u=1:length(x))
      
      return(zzz)
    }
    m <- function() {
      x <- rbeta(n=75,2,0.5,ncp=2)
      .y <- out1(x)
      rv$x <- 1:length(x)
      rv$y <- x
      
      zzz <- list(x=x,mn1=mean(x),med1=median(x),sd1=sd(x),
                  bq1=quantile(x,prob=0.25),bq3=quantile(x,prob=0.75),
                  iqr1=quantile(x,prob=0.75)-quantile(x,prob=0.25),w=.y,
                  u=1:length(x))
      
      return(zzz)
    }
    
    
    switch(input$dist1,
           Normal=g(),
           Left=m(),
           Right=h(),
           Uniform=k())
    
  })
  
  
  
  output$distPlot <- renderPlot({
    
    
    
    
    if(is.null(rv$y)) {
      x <- dataInput()$x
    } else {
      x <- rv$y
    }
    
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    iqr2 <- quantile(x,prob=0.75) - quantile(x,prob=0.25)
    low2 <- quantile(x,prob=0.25) - 1.5*iqr2
    upp2 <- quantile(x,prob=0.75) + 1.5*iqr2
    lb2 <- length(bins)
    clr2 <- rep("lightblue",length=lb2)
    for(i in 1:lb2) {
      if(bins[i] <= low2 | bins[i] >= upp2)clr2[i] <- "red"
    }
    
    
    
    par(fig=c(0,1,0,1), new=TRUE)
    hist(x, breaks = bins, border = "white",
         xlab = " ",
         main = "Histogram of random numbers",col=clr2)
    if(input$mean)abline(v=mean(x),col="red",lty=2)
    if(input$points)rug(x)
    if(input$box){
      par(fig=c(0,1,0,.5), new=TRUE)
    boxplot(x, horizontal=T,axes=F)}
    
  })
  
  output$down <- downloadHandler(
    
    
    filename <- function(){
      paste("plot-", substring(Sys.time(),1,10),"-",
            substring(Sys.time(),12,13),"-",substring(Sys.time(),15,16),
            "-",substring(Sys.time(),18,19),
            ".png", sep="")
      
    },
    content <- function(file){   
      png(file)
      if(is.null(rv$y)) {
        x <- dataInput()$x
      } else {
        x <- rv$y
      }
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      iqr2 <- quantile(x,prob=0.75) - quantile(x,prob=0.25)
      low2 <- quantile(x,prob=0.25) - 1.5*iqr2
      upp2 <- quantile(x,prob=0.75) + 1.5*iqr2
      lb2 <- length(bins)
      clr2 <- rep("lightblue",length=lb2)
      for(i in 1:lb2) {
        if(bins[i] <= low2 | bins[i] >= upp2)clr2[i] <- "red"
      }
      
      
      
      par(fig=c(0,1,0,1), new=TRUE)
      hist(x, breaks = bins, border = "white",
           xlab = " ",
           main = "Histogram of random numbers",col=clr2)
      if(input$mean)abline(v=mean(x),col="red",lty=2)
      if(input$points)rug(x)
      if(input$box){
        par(fig=c(0,1,0,.5), new=TRUE)
        boxplot(x, horizontal=T,axes=F)}
      
      dev.off()
    })
  
  output$mean1 <- renderPrint({
    #   		 cat(paste("mean = ",round(dataInput()$mn1,3),sep=""),"\n")
    cat(paste("Mean = ",round(mean(rv$y),3),sep=""),"\n")
    cat(paste("Median = ",round(median(rv$y),3),sep=""),"\n")
    cat(paste("SD = ",round(sd(rv$y),3),sep=""),"\n")
    iqr1 <- quantile(rv$y,prob=0.75) - quantile(rv$y,prob=0.25)
    cat(paste("IQR = ",round(iqr1,3),sep=""))
  })
  output$altPlot <- renderPlot({				 
    boxplot(rv$y,horizontal=TRUE)
    if(is.null(rv$y)) {
      x <- dataInput()$x
    } else {
      x <- rv$y
    }
    
    
    
  })
  
  
  #  rv <- reactiveValues(x=NULL,y=NULL)
  #    x = dataInput()$u,
  #   y = dataInput()$x
  #  )
  #print(rv$x)
  
  
  grid <- reactive({
    
    data.frame(x = seq(min(rv$x), max(rv$x), length = 10))
    
  })
  
  model <- reactive({
    
    
    d <- data.frame(x = rv$x, y = rv$y)
    lm(y ~ x, d)
    
  })
  
  
  
#  output$p <- renderPlotly({
#    #    rv$x <- dataInput()$u
#    #    rv$y <- dataInput()$x
#    # creates a list of circle shapes from x/y data
#    circles <- map2(rv$y, rv$x, 
#                    ~list(
#                      type = "circle",
#                      # anchor circles at (mpg, wt)
 #                     xanchor = .x,
#                      yanchor = .y,
#                      # give each circle a 2 pixel diameter
#                      x0 = -4, x1 = 4,
#                      y0 = -4, y1 = 4,
#                      xsizemode = "pixel", 
#                      ysizemode = "pixel",
#                      # other visual properties
#                      fillcolor = "blue",
#                      line = list(color = "transparent")
#                    )
#    )
#    #    print(summary(model()))
#    
#    # plot the shapes and fitted line
 #   plot_ly() %>%
#      #    add_lines(x = grid()$x, y = predict(model(), grid()), color = I("red")) %>%
#      layout(shapes = circles) %>%
#      layout(yaxis=list(showticklabels=FALSE)) %>%
#      
#     config(edits = list(shapePosition = TRUE))
#  })
#  
# output$summary <- renderPrint({a
#    summary(model())
#  })
#  
#  # update x/y reactive values in response to changes in shape anchors
#  observe({
#    ed <- event_data("plotly_relayout")
#    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
#    if (length(shape_anchors) != 2) return()
#    row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
#    pts <- as.numeric(shape_anchors)
#    
#    rv$y[row_index] <- pts[1]
#    rv$x[row_index] <- pts[2]
#  })
  
  
  
  
}

shinyApp(ui,server)
