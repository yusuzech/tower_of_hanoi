library(shiny)
library(stringr)
library(R6)
library(RColorBrewer)
library(purrr)
library(tibble)
library(dplyr)
library(ggplot2)
source("hanoi.R")
ui <- fluidPage(
  sidebarLayout(
      sidebarPanel(
          fluidRow(
              selectInput(
                  inputId = "hanoi_n",
                  label = "Number Disks",
                  choices = 1:10,
                  selected = 3
              )
          ),
          fluidRow(
              column(
                  width = 6,
                  offset = 6,
                  actionButton("nextStep","Next",width = "100%")
              )
          )
      ),
      mainPanel(
          plotOutput("graph"),
          uiOutput("hint")
      )
          
  )
)

server <- function(input, output, session) {
    hanoi <- reactiveVal()
    reactiveCounter <- reactiveVal(1)
    observeEvent(input$hanoi_n,{
        hanoi(toa$new(as.integer(input$hanoi_n)))
    })
    
    observeEvent(input$nextStep,{
      hanoi()$advance()
      newVal <- reactiveCounter() + 1
      reactiveCounter(newVal)
    })
    
    observe({
        req(reactiveCounter())
        #update hints
        output$hint <- renderUI({
            HTML(
                glue::glue(
                    "<span>Step {hanoi()$current_step - 1} of {length(hanoi()$solution)}</span>",
                    "<br>",
                    "<span>Next Step:{hanoi()$solution[hanoi()$current_step]}</span>"
                )
            )
        })
        
        #update graph
        output$graph <- renderPlot(hanoi()$snapeshot())
    })
    
    
    
    
}

shinyApp(ui, server)