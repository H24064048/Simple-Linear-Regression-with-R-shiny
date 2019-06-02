library(shiny)

ui <- fluidPage(
  
  titlePanel("Simple Linear Regression"),
  sidebarLayout(
    sidebarPanel(
      h4("Try to find values for the slope and intercept that minimize the residual error from the linear model."),
      h4("Color will change at minimum SSE (b0=-17.5,b1=4)"),
      br(),
      sliderInput("Int",
                  "Intercept",
                  min = -20,
                  max = 20,
                  value = 0,step=2.5),
      sliderInput("Slp",
                  "Slope",
                  min = 0,
                  max = 5,
                  value = 2.5,step=0.5),
      br(),
      checkboxInput(inputId = "summary_show",
                    label = strong("Show summary(lm(y ~ x))"),
                    value = FALSE)
    ),
    
    mainPanel(
      plotOutput("regPlot",height = "500px",width = "700px"),
      plotOutput("linePlot",height = "200px",width = "750px"),
      plotOutput(outputId = "HistPlot", height = "300px",width = "750px"),
      conditionalPanel(
        condition = "summary_show == TRUE",
        textOutput("titlesum"),
        tags$head(tags$style("#titlesum{color: black;
                                 font-size: 18px;
                                 font-style: italic;
                                 }"
        )),
        verbatimTextOutput("summary")
        
        )
      )
   
    )
  
)

server <- function(input, output) {
  

  
# Scatter Plot with Regression
  
  output$regPlot <- renderPlot({
      x = cars$speed
      y = cars$dist
      plot(x,y,axes=FALSE,pch=20,cex=2,cex.lab=1.4)
      title("Linear model dist~speed",cex.main=1.5)
      axis(side=1,at=seq(5,25,5),las=0,lwd=1.5,cex.axis=1.3)
      axis(side=2,at=seq(20,120,20),las=0,lwd=1.5,cex.axis=1.3)
      abline(a=input$Int, b=input$Slp,lwd=2)
      legend("topleft",paste("y = ",input$Int," + ",input$Slp,"* x"),bty="n",lty = 1)
      model <- lm(y ~ x)
      # calculate residuals and predicted values
      res <- signif(residuals(model), 5)
      pre <- predict(model) # plot distances between points and the regression line
      segments(x, y, x, pre,lwd=2, c = ifelse(input$Int==-17.5 & input$Slp==4 ,'forestgreen','firebrick3') )
  })

  

  
  

# Sum of Squares of Residuals (SSE)
  
  output$linePlot <- renderPlot({
    
    #Calculation
    X <- cars$speed
    Y <- cars$dist
    pred <- input$Slp*X + input$Int 
    rsd <- Y-pred
    sse <- sum(rsd^2)
    #Min SSE
    minpred <- 3.9324*X -17.5791 
    SSE <- sum((Y-minpred)^2)                 #11353.52

    
    #Plot
    x = 0:60000
    y = numeric(60001)
    plot(x,y,ylim=c(0,1),type='n',axes = FALSE,ylab="",xlab="")
    title("Sum of Squares of residuals  (SSE)",cex.main=1.5)
    axis(side=1,at=seq(0,60000,10000),las=0,lwd=1.8,cex.axis=1.3)
    points(sse,0.2,pch=1,cex=3,col=ifelse(input$Int==-17.5 & input$Slp==4 ,'forestgreen','firebrick3'))
    points(SSE,0.2,pch=4,cex=3)  
 })  
  

  
  
#  BoxPlot : Distribution of Residuals 
  
  output$HistPlot <- renderPlot({
    
    #data
    x=cars$speed
    y=cars$dist
    pred <- input$Slp*x + input$Int
    c=pred-y
    
    #plot
    hist(c,xlim = c(-120,120),ylim = c(0,0.03),prob=TRUE,axes = FALSE,xlab = "",ylab = "",col="gray",main="Distribution of Residuals",cex.main=1.5)
    rug(c)
    axis(1,at=seq(-120,120,20))
    curve(dnorm(x, mean=0, sd=(231.7045)^(1/2)), add=TRUE,lwd=2, col=ifelse(input$Int==-17.5 & input$Slp==4 ,'forestgreen','firebrick3'))       #lm's c's var=231.7045    

  })
  

  
    
#  Summary
  output$titlesum <- renderText({
    if (input$summary_show) {
      print("Linear Model Summary")
    }
  })
  
  output$summary <- renderPrint({
    if(input$summary_show){
      fit <- lm(cars$dist ~ cars$speed)
      summary(fit)
      }
  })  
  
  

    

}

shinyApp(ui = ui, server = server)






