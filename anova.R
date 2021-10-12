ui <- pageWithSidebar(
  headerPanel('ANOVA'),
  sidebarPanel(
     sliderInput("mean1","Mean of Group A",
     value=100,min=50,max=150,step=1),
     sliderInput("mean2","Mean of Group B",
     value=100,min=50,max=150,step=1),
     sliderInput("mean3","Mean of Group C",
     value=100,min=50,max=150,step=1),
     sliderInput("n","Group Sample Size (n)",
                 value=25,min=2,max=200,step=1),
     sliderInput("bsd1","Common Standard Deviation",
     value=20,min=1,max=100,step=1),
     checkboxInput("showse","Show SE bars",FALSE)     
  ),
  mainPanel(
    plotOutput('plot1'),
    div(HTML("<hr>")),
    tableOutput('aov1a')
  )
)



server <- function(input, output, session) {
  
library(ggplot2)
  
 selectedData <- reactive({
  dat <- data.frame(
    means=c(rep(input$mean1,input$n),rep(input$mean2,input$n),rep(input$mean3,input$n)),
    Response=c(input$mean1+input$bsd1*scale(rnorm(input$n)), 
               input$mean2+input$bsd1*scale(rnorm(input$n)),
               input$mean3+input$bsd1*scale(rnorm(input$n))),
    Group=c(rep("A",input$n),
            rep("B",input$n),
            rep("C",input$n))
  )
  return(dat)
  })

  output$plot1 <- renderPlot({
    
  dat <- selectedData()
  dat$Group<-factor(dat$Group,levels=c("C","B","A"))
  
  if(input$showse) {
  ggplot(dat, aes(Group,Response))+geom_boxplot()+geom_point()+
    geom_point(stat="summary",fun=mean,aes(color=Group),size=6, shape=18)+
    geom_errorbar(stat="summary",fun.data=mean_se,aes(color=Group),size=1,width=.5)+
    coord_flip()+
    theme_light(base_size=14)+
    ylim(min(0,dat$Response),max(200,dat$Response))+
    scale_color_discrete(breaks=c("A","B","C"))
  }
  else{
    ggplot(dat, aes(Group,Response))+geom_boxplot()+geom_point()+
      geom_point(stat="summary",fun=mean,aes(color=Group),size=6, shape=18)+
      #geom_errorbar(stat="summary",aes(color=Group),size=1,width=.5)+
      coord_flip()+
      theme_light(base_size=14)+
      ylim(min(0,dat$Response),max(200,dat$Response))+
      scale_color_discrete(breaks=c("A","B","C"))
  }  
  })
  
output$aov1a <- renderTable({
  dat <- selectedData()
  df<-as.data.frame(anova(lm(Response~Group, data=dat)))
  df<-rbind(df,colSums(df))
  rownames(df)<-c("Group", "Error","Total")
  df[3,3]<-NA
  df
}, rownames=T, width="90%",na=" ")
}

shinyApp(ui,server)
