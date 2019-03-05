library(shiny)
library(shinythemes)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(gtable)

###server
server <- function(input, output, session) {
  
  ##gather inputs
  specialRange <- reactive({paste(input$special_range)})
  sliderVals <- reactive({paste(input$year_range)})
  
  ##listen to event change
  observe({
    #set the default values
    
    if(specialRange()=="All Years"){
      minY <- 1980
      maxY <- 2013
    }else if(specialRange()=="After 1990"){
      minY <- 1990
      maxY <- 2013
    }else if(specialRange()=="Range"){
      minY <- as.integer(sliderVals()[1])
      maxY <- as.integer(sliderVals()[2])
      print(minY)
    }
    
    ###plot the graph
    plt <- reactive(
      {
        dat <- who %>%
          gather(new_sp_m014:newrel_f65, key="key", value = "cases", na.rm = TRUE) %>%
          
          mutate(key = stringr::str_replace(key, "newrel", "new_rel"))%>%
          
          separate(key, c("new", "type", "sex"), sep="_")%>%
          
          separate(sex, c("sex", "age"), sep=1, convert = TRUE)%>%
          
          select(-new, -iso2, -iso3) 
        
        dat %>%
          #group by country, year and sex
          group_by(country, year, sex) %>%
          #add the filter
          filter(year>=minY & year <=maxY) %>%
          #create a new variable to cross reference country and sex
          unite(country_sex, country, sex, remove = FALSE) %>%
          #add plot
          ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
          geom_line()
      }
    )
    output$p_plot <- renderPlot({
      plt()
    })
    
    
  })
}

#### user interface

ui <- fluidPage(
  theme=shinytheme("cosmo"),
  
  titlePanel("Homework 3 Problem 4"),
  sidebarLayout(
    
    sidebarPanel(
      tags$blockquote("This illustrates the variation of the number of case of Tuberclosis(TB) observed from 1980-2013 grouped by sex."),
      tags$blockquote("You can observe the distribution of cases of TB by specifying a range using the options below."),
      
      radioButtons(inputId = "special_range", label="Quick View", inline = TRUE, choices = c("All Years", "After 1990", "Range")),
      sliderInput(inputId = "year_range", label = "Year Range(First Select Range Above)", min = 1980, max = 2013, value =c(1980, 2013), step=1, sep = "")
      
    ), #endsidebarpanel
    
    mainPanel(
      #output results
      tabsetPanel(
        tabPanel("Results", 
                 plotOutput("p_plot")
                 
        )
        
      )
      
    )#end mainpanel
    #choices for test sides
    #alt <- c("One sided", "Two sided")
  )# end sidebarlayout
)

shinyApp(ui = ui, server = server)