library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)  

ui <- fluidPage(
  navbarPage("F1 Lap Duration Viewer",
             tabPanel("Plot",
                      fluidRow(
                        column(3,
                               selectInput("year", "Select Year:", choices = 2021:2025, selected = 2024)
                        ),
                        column(3,
                               uiOutput("circuit_ui")
                        ),
                        column(3,
                               uiOutput("session_ui")
                        ),
                        column(3,
                               uiOutput("driver_ui")
                        )
                      ),
                      actionButton("load", "Load Lap Data", class = "btn-primary", style = "margin: 10px;"),
                      verbatimTextOutput("status"),
                      plotlyOutput("lapPlot")  
             )
  )
)

server <- function(input, output, session) {
  
  
  session_data <- reactive({
    url <- paste0("https://api.openf1.org/v1/sessions?year=", input$year)
    response <- GET(url)
    fromJSON(content(response, "text"))
  })
  
 
  output$circuit_ui <- renderUI({
    data <- session_data()
    circuits <- sort(unique(data$circuit_short_name))
    selectInput("circuit", "Select Circuit:", choices = circuits)
  })
  
  
  output$session_ui <- renderUI({
    data <- session_data()
    filtered <- data[data$circuit_short_name == input$circuit,]
    session_types <- sort(unique(filtered$session_name))
    selectInput("session_type", "Select Session:", choices = session_types)
  })
  
  
  output$driver_ui <- renderUI({
    req(input$circuit, input$session_type)
    data <- session_data()
    selected <- data[data$circuit_short_name == input$circuit &
                       data$session_name == input$session_type, ]
    if (nrow(selected) == 0) return()
    
    session_key <- selected$session_key[1]
    driver_url <- paste0("https://api.openf1.org/v1/drivers?session_key=", session_key)
    
    driver_response <- GET(driver_url)
    driver_data <- fromJSON(content(driver_response, "text"))
    
    if (nrow(driver_data) == 0) return()
    
    drivers <- driver_data %>%
      distinct(driver_number, last_name) %>%
      arrange(last_name)
    
    driver_choices <- setNames(as.character(drivers$driver_number), drivers$last_name)
    
    selectInput("driver_number", "Select Driver(s):", 
                choices = driver_choices, multiple = TRUE)
  })
  
  
  lap_data <- eventReactive(input$load, {
    output$status <- renderText("Fetching data...")
    
    data <- session_data()
    selected <- data[data$circuit_short_name == input$circuit &
                       data$session_name == input$session_type, ]
    if (nrow(selected) == 0 || is.null(input$driver_number)) {
      output$status <- renderText("No session or drivers selected.")
      return(NULL)
    }
    
    session_key <- selected$session_key[1]
    all_laps <- list()
    
    for (driver in input$driver_number) {
      lap_url <- paste0("https://api.openf1.org/v1/laps?session_key=", 
                        session_key, "&driver_number=", driver)
      lap_response <- GET(lap_url)
      lap_df <- fromJSON(content(lap_response, "text"))
      
      if (nrow(lap_df) > 0) {
        lap_df$driver_number <- driver
        all_laps[[driver]] <- lap_df
      }
    }
    
    combined_laps <- bind_rows(all_laps)
    
    if (nrow(combined_laps) == 0) {
      output$status <- renderText("No lap data found for selected drivers.")
      return(NULL)
    }
    
    output$status <- renderText("Data loaded successfully.")
    return(combined_laps)
  })
  
 
  output$lapPlot <- renderPlotly({
    data <- lap_data()
    if (is.null(data)) return()
    
    data <- data %>%
      mutate(
        lap_duration_sec = as.numeric(lap_duration),
        lap_number = as.numeric(lap_number),
        driver_number = as.factor(driver_number)
      ) %>%
      filter(!is.na(lap_duration_sec))
    
    p <- ggplot(data, aes(x = lap_number, y = lap_duration_sec, 
                          group = driver_number,color = driver_number,
                          text = paste("Driver:", driver_number,
                                       "<br>Lap:", lap_number, 
                                       "<br>Duration:", round(lap_duration_sec, 2), "s"))) +
      geom_line(size=0.7,alpha=0.6) +
     geom_point(size=1.1) +
      labs(
        title = "Lap Duration Comparison",
        x = "Lap Number",
        y = "Lap Duration (seconds)",
        color = "Driver"
      ) +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 16, face = "bold"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p, tooltip = "text")  
  })
}

shinyApp(ui, server)
