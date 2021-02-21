library(shiny)
library(tidyverse)
library(rjson)
library(lubridate)
library(DT)

ui <- fluidPage(
  titlePanel("Ark Holdings Statistics"),
  
  sidebarLayout(
    sidebarPanel(width = 2, 
      selectInput(inputId = "fund_select",
                  label = "Select a fund",
                  choices = c("ARKK", "ARKW", "ARKQ", "ARKG", "ARKF")),
      dateRangeInput(inputId = "date_range",
                     label = "Select a range of dates",
                     start = "2021-01-01",
                     end = "2021-01-06")
    ),
    
    mainPanel(dataTableOutput("test"))
  )
)

server <- function(input, output) {
  dates <- reactive(seq(from = input$date_range[1], to = input$date_range[2], by = "days"))
  files <- reactive(paste0("https://arkfunds.io/api/v1/etf/holdings?symbol=ARKK&date=", dates()))
  
  ark_holdings_data <- reactive(
    lapply(files(), function(x) fromJSON(file = x)[["holdings"]] %>% map(bind_rows)) %>%
      set_names(dates()) %>% unlist(recursive = FALSE) %>% bind_rows(.id = "date") %>%
      mutate(date = ymd(str_sub(date, start = 1, end = 10)))
  )
  
  output$test <- renderDataTable({
    datatable(ark_holdings_data())
  })
}

shinyApp(ui = ui, server = server)
