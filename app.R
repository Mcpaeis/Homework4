library(shiny)
library(shinythemes)

library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(gtable)

###server
server <- function(input, output, session) {
  # return a list of UI elements
  output$my_output_UI <- renderUI({
    
    list(
      h4(style = "color:blue;", "Select Test"),
      selectInput(inputId = "testSelected", label="", choices = selections)
    )
  })
  
  # initial selections
  selections <- c("Select", "T-test", "Chi-square Test", "Anova Test", "Proportions Test")
  
  ##gather inputs
  valToCalculate <- reactive({paste(input$val_to_calculate)})
  n <- reactive({paste({input$sample_size})})
  delta <- reactive({paste(input$delta)})
  std <- reactive({paste(input$std_deviation)})
  powe <- reactive({paste(input$power)})
  alpha <- reactive({paste(input$alpha)})
  sliderVals <- reactive({paste(input$alpha_range)})
  
  ##listen to event change
  observe({
    #set the default values
    n <- ifelse(n()=="", 100, n())
    delta <- ifelse(delta()=="", 0.2, delta())
    std <- ifelse(std()=="", 1, std())
    powe <- ifelse(powe()=="", 0.5, powe())
    alpha <- ifelse(alpha()=="", 0.05, alpha())
    if(valToCalculate()=="Power"){
      #perform power calculations
      results <- power.t.test(as.integer(n), as.double(delta), sd=as.double(std), sig.level = as.double(alpha), power=NULL)
      #results <- power.t.test(n = 6, 0.2, sd = 0.1, sig.level =0.05, power=NULL)
      #construct a dataframe
      power <- results$power
      n <- results$n
      delta <- results$delta
      sd <- results$sd
      sig.level <- results$sig.level
      #convert to dataframe
      df <- data.frame(n, power, delta, sd, sig.level)
      
      #gather the data for the table
      dat <- reactive({
        df
      })
      #render tablke
      output$mytable <- renderTable({
        dat()
      })
    }else if(valToCalculate()=="Sample Size"){
      #perform sample size calculations
      results <- power.t.test(n=NULL, as.double(delta), sd=as.double(std), sig.level = as.double(alpha), power=as.double(powe))
      #results <- power.t.test(n = 6, 0.2, sd = 0.1, sig.level =0.05, power=NULL)
      #construct a dataframe
      power <- results$power
      n <- results$n
      delta <- results$delta
      sd <- results$sd
      sig.level <- results$sig.level
      #convert to dataframe
      df <- data.frame(n, power, delta, sd, sig.level)
      
      #gather the data for the table
      dat <- reactive({
        df
      })
      #render tablke
      output$mytable <- renderTable({
        dat()
      })
    }else if(valToCalculate()=="Delta"){
      #perform delta calculations
      results <- power.t.test(n, NULL, sd=as.double(std), sig.level = as.double(alpha), power=as.double(powe))
      #results <- power.t.test(n = 6, 0.2, sd = 0.1, sig.level =0.05, power=NULL)
      #construct a dataframe
      power <- results$power
      n <- results$n
      delta <- results$delta
      sd <- results$sd
      sig.level <- results$sig.level
      #convert to dataframe
      df <- data.frame(n, power, delta, sd, sig.level)
      
      #gather the data for the table
      dat <- reactive({
        df
      })
      #render tablke
      output$mytable <- renderTable({
        dat()
      })
    }
    
    ###plot the graph for the t test
    plt <- reactive(
      {
        nvals <- seq(2, n, length.out=200)
        powvals <- sapply(nvals, function(x) power.t.test(n=x, delta=delta)$power)
        plot(nvals, powvals, xlab="n", ylab="power", main="Power Curve for t-test", lwd=2, col="red", type="l")
      }
    )
    output$power_curve <- renderPlot({
      plt()
    })
    
    ###varying delta
    pltVarying <- reactive(
      {
        nvals <- seq(2, n, length.out=200)
        minD <- as.double(sliderVals()[1])
        maxD <- as.double(sliderVals()[2])
        deltas <- c(minD)
        i = 2
        while (minD < maxD) {
          minD <- minD + 0.1
          minD <- ifelse(minD > maxD, maxD, minD)
          deltas[i] <- minD
          i <- i +1
        }
        plot(nvals, seq(0,1, length.out=length(nvals)), xlab="n", ylab="power",
             main="Power Curve for\nt-test with varying delta", type="n")
        for (i in 1:length(deltas)) {
          powvals <- sapply(nvals, function (x) power.t.test(n=x, delta=deltas[i])$power)
          lines(nvals, powvals, lwd=2, col=i)
        }
        legend("topleft", lwd=2, col=1:length(deltas), legend=deltas)
      }
    )
    ##tie to a render event
    output$varying_delta <- renderPlot({
      pltVarying()
    })
    
    print(as.double(sliderVals()[1]))
    
  })
}

#### user interface

ui <- fluidPage(
  theme=shinytheme("cosmo"),
  
  titlePanel("Power Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #layer to output selection
      uiOutput("my_output_UI"),
      textInput("sample_size", "Enter Sample Size"),
      textInput("delta", "Enter Delta"),
      textInput("std_deviation", "Enter SD"),
      #sliderInput(inputId = "alpha", label = "Alpha", min = 0,max = 1, value =0.05),
      selectInput(inputId = "alpha", label="Sig. Level", choices = c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)),
      textInput("power", "Enter Power"),
      selectInput(inputId = "alternative", label = "Alternative", choices = c("One sided", "Two sided")),
      #radio button to indicate which to calculate
      radioButtons(inputId = "val_to_calculate", label="Value to Calculate", inline = TRUE, choices = c("Power", "Sample Size", "Delta"))
      
    ), #endsidebarpanel
    
    mainPanel(
      #output results righ here..
      uiOutput("test_results"),
      textOutput("text"),
      tabsetPanel(
        tabPanel("Results", tableOutput("mytable"),
                 plotOutput("power_curve")
        ),
        tabPanel("Varying Delta", plotOutput("varying_delta"), 
                 sliderInput(inputId = "alpha_range", label = "Delta", min = 0, max = 1, value = c(0.05, 1)))
        
      )
      
    )#end mainpanel
    #choices for test sides
    #alt <- c("One sided", "Two sided")
  )# end sidebarlayout
)

shinyApp(ui = ui, server = server)
