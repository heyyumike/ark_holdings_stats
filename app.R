library(shiny)
library(tidyverse)
library(rjson)
library(lubridate)
library(DT)
library(shinycssloaders)
library(plotly)

ui <- fluidPage(
  titlePanel("Ark Holdings Statistics"),
  
  sidebarLayout(
    sidebarPanel(width = 2, 
      selectInput(inputId = "fund_select",
                  label = "Select a fund",
                  choices = c("ARKK", "ARKW", "ARKQ", "ARKG", "ARKF")),
      dateRangeInput(inputId = "date_range",
                     label = "Select a range of dates")
    ),
    
    mainPanel(plotlyOutput("test") %>% withSpinner(color = "red"))
  )
)

server <- function(input, output) {
  dates <- reactive(
    if (input$date_range[1] <= input$date_range[2]) {
      return(seq(from = input$date_range[1], to = input$date_range[2], by = "days"))
    }
  )
  files <- reactive(paste0("https://arkfunds.io/api/v1/etf/holdings?symbol=", input$fund_select, "&date=", dates()))
  
  ark_holdings_data <- reactive(
    if (input$date_range[1] <= input$date_range[2]) {
      if (length(dates()) > 31) {
        return(data.frame())
      } else {
        lapply(files(), function(x) fromJSON(file = x)[["holdings"]] %>% map(bind_rows)) %>%
          set_names(dates()) %>% unlist(recursive = FALSE) %>% bind_rows(.id = "date")
      }
    } else {
      return(data.frame())
    }
  )
  
  output$test <- renderPlotly({
    validate(
      need(length(ark_holdings_data()) != 0, 
           "Please make sure to follow these criteria:\n1) Valid date range \n2) No weekends/holidays \n3) Keep range of dates selection to <= 31 days")
    )
    
    p <- ark_holdings_data() %>% mutate(date = ymd(str_sub(date, start = 1, end = 10))) %>% filter(weight_rank <= 10) %>%
      ggplot(aes(x = date, y = weight, color = ticker, group = ticker)) + geom_point() + geom_line()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
