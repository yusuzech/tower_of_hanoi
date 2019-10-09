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
    hanoi_result <- reactiveVal()
    step_count <- reactiveVal()
    observeEvent(input$hanoi_n,{
        hanoi(toa$new(as.integer(input$hanoi_n)))
        hanoi_result(hanoi()$steps())
        step_count(1)
    })
    
    observeEvent(input$nextStep,{
      if(step_count() <= length(hanoi_result())){
        instruction <- hanoi_result()[step_count()]
        from <- stringr::str_extract(instruction,"(?<=from ).(?= to)")
        to <- stringr::str_extract(instruction,"(?<=to ).$")
        hanoi()$piece_move_to(from,to)
        step_count_new <- step_count() + 1
        step_count(step_count_new)
      }
        
    })
    
    observe({
        req(step_count())
        #update hints
        output$hint <- renderUI({
            HTML(
                glue::glue(
                    "<span>Step {step_count() - 1} of {length(hanoi_result())}</span>",
                    "<br>",
                    "<span>Next Step:{hanoi_result()[step_count()]}</span>"
                )
            )
        })
        
        #update graph
        output$graph <- renderPlot(hanoi()$snapeshot())
    })
    
    
    
    
}

shinyApp(ui, server)