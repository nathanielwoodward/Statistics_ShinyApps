ui <- fluidPage(

  # App title ----
  titlePanel("Tail Probability of the Standard Normal Distribution"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      selectInput(inputId = "prob1",
                  label = "Choose Type of Control:",
		  c("Adjust Probability" = "ap1",
		  "Adjust X value (Z)" = "xval1")),
      selectInput(inputId = "tail",label = "Which Tail of distribution",		  c("one tail upper" = "tail1u",
            "one tail lower" = "tail1l",
	    "two tail" = "tail2")),
		  
 radioButtons(inputId="stype", "Choose Slider or Input number:",
               choices=c("Slider" = "slide",
                 "Number entry box" = "numen"),
		 selected = "slide"),
		 
	    

    conditionalPanel(
    condition = "input.stype == 'slide' && input.prob1 == 'ap1'",
     
	sliderInput("alpha", "Probability:",
		value=0.05,
	min=0.00,
	max=0.50)
	),

    conditionalPanel(
    condition = "input.stype == 'numen' && input.prob1 == 'ap1'",
    numericInput("numb3","Probability:",
    		value=0.05,
	min=0.00,
	max=0.50)
),

    conditionalPanel(
    condition = "input.stype == 'numen' && input.prob1 == 'xval1'",
    numericInput("numb2","Enter X Axis Quantile",
    		value=1.65,
	min=0.00,
	max=4.30)
),
    conditionalPanel( 	      
    condition = "input.stype == 'slide' && input.prob1 == 'xval1'",
     
	sliderInput("beta", "Probability:",
		value=1.65,
	min=0.00,
	max=4.30)
	),
     downloadButton("down1", "Download as png"),
     downloadButton("down2", "Download as pdf"),

),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Normal Plot
      plotOutput(outputId = "distPlot")

    )
  )
)

server <- function(input, output) {


  output$distPlot <- renderPlot({
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  xe1 <- "prob(Z"
		  xe2 <- "or Z"
		  xe3 <- ")"

		  
		  if(input$prob1=="ap1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
			if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zzz <- .z1
			ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$alpha))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha)
			zz[x >= -4.4 & x<=.z1] <- y[x >= -4.4 & x<=.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$alpha))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha/2,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zz[x>= -4.4 & x<=-.z1] <- y[x>=-4.4 & x<=-.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2)  >= .(.z1) ~ .(xe3) == .(input$alpha))						
			}
			}
		  if(input$prob1=="ap1" & input$stype=="numen") {		
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

 if(input$tail == "tail1u") {
   		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3,lower.tail=FALSE)
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3)

		  zz[x<=.z1 & x >= -4.4] <- y[x<=.z1 & x >= -4.4]
		  ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3/2,lower=FALSE)

		  zz[x<=-.z1 & x >= -4.4] <- y[x<=-.z1 & x >= -4.4]
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- -.z1
		  }


		  }
		  
		  if(input$prob1=="xval1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) >= .(input$beta) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(-input$beta)

			zz[x >= -4.4 & x<=-input$beta] <- y[x >= -4.4 & x<=-input$beta]
			zzz <- -input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zz[x>= -4.4 & x<=-input$beta] <- y[x>=-4.4 & x<=-input$beta]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe2)  >= .(input$beta) ~ .(xe3) == .(.z1))						
			}
		}
			

		  if(input$prob1=="xval1" & input$stype=="numen") {

		  
  		  x <- seq(-4.4,4.4,.01)

		  y <- dnorm(x)

if(input$tail == "tail1u") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			print("bad")
			print(.z1)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) >= .(input$numb2) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2)
			zz[x >= -4.4 & x<=-input$numb2] <- y[x >= -4.4 & x<=-input$numb2]
			zzz <- -input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zz[x>= -4.4 & x<=-input$numb2] <- y[x>=-4.4 & x<=-input$numb2]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe2)  >= .(input$numb2) ~ .(xe3) == .(.z1))						
			}
}


		  
		  plot(x,y,type="l",axes=FALSE,xlab="Z Values",ylab="Density")
		  title(ex1)
		  axis(1,pos=0)
		  axis(2,pos=-4.4)
		  lines(x,zz,type="h",col="grey")
		  abline(v=0,col="black")

		  if(input$tail=="tail1u")abline(v=zzz,
			col="red")
		  if(input$tail == "tail1l")abline(v=zzz,col="red")
		  if(input$tail == "tail2") {
		  		abline(v=zzz,col="red")
				abline(v=-zzz,col="red")
				}
		  


output$down1 <- downloadHandler(
     filename <- function(){
      paste("plot-", substring(Sys.time(),1,10),"-",
      substring(Sys.time(),12,13),"-",substring(Sys.time(),15,16),
      "-",substring(Sys.time(),18,19),
      ".png", sep="")
     
     },
content <- function(file){   
         png(file)


  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  xe1 <- "prob(Z"
		  xe2 <- "or Z"
		  xe3 <- ")"
		  print("being")
		  print(input$prob1)
		  print(input$stype)
		  print(input$numb2)
		  
		  if(input$prob1=="ap1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
			if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zzz <- .z1
			ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$alpha))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha)
			zz[x >= -4.4 & x<=.z1] <- y[x >= -4.4 & x<=.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$alpha))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha/2,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zz[x>= -4.4 & x<=-.z1] <- y[x>=-4.4 & x<=-.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2)  >= .(.z1) ~ .(xe3) == .(input$alpha))						
			}
			}
		  if(input$prob1=="ap1" & input$stype=="numen") {		
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

 if(input$tail == "tail1u") {
   		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3,lower.tail=FALSE)
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3)

		  zz[x<=.z1 & x >= -4.4] <- y[x<=.z1 & x >= -4.4]
		  ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3/2,lower=FALSE)

		  zz[x<=-.z1 & x >= -4.4] <- y[x<=-.z1 & x >= -4.4]
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- -.z1
		  }


		  }
		  
		  if(input$prob1=="xval1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) >= .(input$beta) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(-input$beta)

			zz[x >= -4.4 & x<=-input$beta] <- y[x >= -4.4 & x<=-input$beta]
			zzz <- -input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zz[x>= -4.4 & x<=-input$beta] <- y[x>=-4.4 & x<=-input$beta]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe2)  >= .(input$beta) ~ .(xe3) == .(.z1))						
			}
		}
			

		  if(input$prob1=="xval1" & input$stype=="numen") {
		  print("stuff")
		  
  		  x <- seq(-4.4,4.4,.01)
		  print("test")
		  y <- dnorm(x)
		  print(input$numb2)
if(input$tail == "tail1u") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			print("bad")
			print(.z1)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) >= .(input$numb2) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2)
			zz[x >= -4.4 & x<=-input$numb2] <- y[x >= -4.4 & x<=-input$numb2]
			zzz <- -input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zz[x>= -4.4 & x<=-input$numb2] <- y[x>=-4.4 & x<=-input$numb2]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe2)  >= .(input$numb2) ~ .(xe3) == .(.z1))						
			}
}


		  
		  plot(x,y,type="l",axes=FALSE,xlab="Z Values",ylab="Density")
		  title(ex1)
		  axis(1,pos=0)
		  axis(2,pos=-4.4)
		  lines(x,zz,type="h",col="grey")
		  abline(v=0,col="black")

		  if(input$tail=="tail1u")abline(v=zzz,
			col="red")
		  if(input$tail == "tail1l")abline(v=zzz,col="red")
		  if(input$tail == "tail2") {
		  		abline(v=zzz,col="red")
				abline(v=-zzz,col="red")
				}
dev.off()
}
)


output$down2 <- downloadHandler(
     filename <- function(){
      paste("plot-", substring(Sys.time(),1,10),"-",
      substring(Sys.time(),12,13),"-",substring(Sys.time(),15,16),
      "-",substring(Sys.time(),18,19),
      ".pdf", sep="")
     
     },
content <- function(file){   
         pdf(file)


  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  xe1 <- "prob(Z"
		  xe2 <- "or Z"
		  xe3 <- ")"
		  print("being")
		  print(input$prob1)
		  print(input$stype)
		  print(input$numb2)
		  
		  if(input$prob1=="ap1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
			if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zzz <- .z1
			ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$alpha))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha)
			zz[x >= -4.4 & x<=.z1] <- y[x >= -4.4 & x<=.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$alpha))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- qnorm(input$alpha/2,lower.tail=FALSE)
			zz[x>=.z1 & x<=4.4] <- y[x>=.z1 & x <= 4.4]
			zz[x>= -4.4 & x<=-.z1] <- y[x>=-4.4 & x<=-.z1]
			zzz <- .z1
			ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2)  >= .(.z1) ~ .(xe3) == .(input$alpha))						
			}
			}
		  if(input$prob1=="ap1" & input$stype=="numen") {		
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

 if(input$tail == "tail1u") {
   		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3,lower.tail=FALSE)
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
		  
		  zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3)

		  zz[x<=.z1 & x >= -4.4] <- y[x<=.z1 & x >= -4.4]
		  ex1 <- bquote(.(xe1) <= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- .z1
		  }
		  if(input$tail=="tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

zz <- rep(NA,length=length(x))
		  .z1 <- qnorm(input$numb3/2,lower=FALSE)

		  zz[x<=-.z1 & x >= -4.4] <- y[x<=-.z1 & x >= -4.4]
		  zz[x>=.z1 & x <= 4.4] <- y[x>=.z1 & x <= 4.4]
		  ex1 <- bquote(.(xe1) <= -.(.z1) ~ .(xe2) >= .(.z1) ~ .(xe3) == .(input$numb3))
		  zzz <- -.z1
		  }


		  }
		  
		  if(input$prob1=="xval1" & input$stype=="slide") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

if(input$tail == "tail1u") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) >= .(input$beta) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(-input$beta)

			zz[x >= -4.4 & x<=-input$beta] <- y[x >= -4.4 & x<=-input$beta]
			zzz <- -input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$beta,lower.tail=FALSE)
			zz[x>=input$beta & x<=4.4] <- y[x>=input$beta & x <= 4.4]
			zz[x>= -4.4 & x<=-input$beta] <- y[x>=-4.4 & x<=-input$beta]
			zzz <- input$beta
			ex1 <- bquote(.(xe1) <= -.(input$beta) ~ .(xe2)  >= .(input$beta) ~ .(xe3) == .(.z1))						
			}
		}
			

		  if(input$prob1=="xval1" & input$stype=="numen") {
		  print("stuff")
		  
  		  x <- seq(-4.4,4.4,.01)
		  print("test")
		  y <- dnorm(x)
		  print(input$numb2)
if(input$tail == "tail1u") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)

			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			print("bad")
			print(.z1)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) >= .(input$numb2) ~ .(xe3) == .(.z1))
			
			}
			if(input$tail == "tail1l") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2)
			zz[x >= -4.4 & x<=-input$numb2] <- y[x >= -4.4 & x<=-input$numb2]
			zzz <- -input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe3) == .(.z1))			
			}
			if(input$tail == "tail2") {
  		  x <- seq(-4.4,4.4,.01)
		  y <- dnorm(x)
			
			zz <- rep(NA,length=length(x))
			.z1 <- pnorm(input$numb2,lower.tail=FALSE)
			zz[x>=input$numb2 & x<=4.4] <- y[x>=input$numb2 & x <= 4.4]
			zz[x>= -4.4 & x<=-input$numb2] <- y[x>=-4.4 & x<=-input$numb2]
			zzz <- input$numb2
			ex1 <- bquote(.(xe1) <= -.(input$numb2) ~ .(xe2)  >= .(input$numb2) ~ .(xe3) == .(.z1))						
			}
}


		  
		  plot(x,y,type="l",axes=FALSE,xlab="Z Values",ylab="Density")
		  title(ex1)
		  axis(1,pos=0)
		  axis(2,pos=-4.4)
		  lines(x,zz,type="h",col="grey")
		  abline(v=0,col="black")

		  if(input$tail=="tail1u")abline(v=zzz,
			col="red")
		  if(input$tail == "tail1l")abline(v=zzz,col="red")
		  if(input$tail == "tail2") {
		  		abline(v=zzz,col="red")
				abline(v=-zzz,col="red")
				}
dev.off()
}
)





				
}
)



}


shinyApp(ui,server)
