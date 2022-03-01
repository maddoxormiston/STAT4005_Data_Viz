library(tidyverse)
library(here)
library(lubridate)
weather_df <- read_delim(here("data/canton_ny_weather_data.txt"))

weather_df <- weather_df %>% mutate(month = month(datetime), 
                                        year = as.factor(year(datetime)), 
                                        day = day(datetime), 
                                        day_of_year = yday(datetime)) %>% 
  select(month, year, day, day_of_year, everything())

year_choices <- c("2019", "2020", "2021", "2022")

temp_choices <- names(weather_df)[c(7:12)]

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("yearchoice", label = "Choose a year", choices = year_choices), 
      selectInput("temp", label = "Select a temperature stat", choices = temp_choices)
    ), 
    mainPanel(plotOutput("lineplot"))
  )
)

server <- function(input, output, session) {
  weather_year <- reactive({
    weather_df %>% filter(year == input$yearchoice)
  })
  
  output$lineplot <- renderPlot(
    ggplot(data = weather_year(), aes(x = day_of_year, y = .data[[input$temp]])) + 
      geom_line()
  )
}

shinyApp(ui, server)
