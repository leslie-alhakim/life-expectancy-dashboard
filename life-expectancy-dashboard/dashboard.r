# Leslie Alhakim  
# P1 -- Part 4: Build a Dashboard

# Load libraries
library(ggplot2)
library(paletteer)
library(gapminder)

data <- gapminder %>% filter(`year` > 1950)
names(data)

# Shiny Dashboard
library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Interacting with Gapminder Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("My Visuals", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            width = 4, title = "Controls", status = "primary", solidHeader = TRUE,
            selectInput("continent", "Continent",
                        choices = levels(data$continent),
                        selected = levels(data$continent),
                        multiple = TRUE,
                        selectize = FALSE),
            sliderInput("year", "Year range",
                        min = min(data$year), max = max(data$year),
                        value = c(min(data$year), max(data$year)),
                        step = 5,
                        sep = "")
          ),
          box(width = 8, 
              title = "Scatter Plot: GDP per Capita v Life Expectancy",
              status = "info", solidHeader = TRUE,
              plotOutput("scatter_plot", height = 350)
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Line Chart: Life Expectancy Over Time",
              status = "info", solidHeader = TRUE,
              plotOutput("line_plot", height = 350)))
      )
    )
  )
)

server <- function(input, output){
  filtered_data <- reactive({
    req(input$continent, input$year)
    data %>%
      filter(
        continent %in% input$continent,
        dplyr::between(year, input$year[1], input$year[2])
      )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = gdpPercap, y = lifeExp, colour = continent)) +
      geom_point(alpha = 0.7) +
      labs(
        colour = "Continent",
        x = "GDP per Capita",
        y = "Life Expectancy",
        title = "GDP per Capita vs Life Expectancy"
      ) +
      theme_minimal() +
      scale_colour_paletteer_d("rcartocolor::Antique")
  })

  output$line_plot <- renderPlot({
    filtered_data() %>%
      group_by(continent, year) %>%
      summarise(mean_lifeExp = mean(lifeExp), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_lifeExp, colour = continent)) +
      geom_line(linewidth = 1) +
      labs(
        colour = "Continent",
        x = "Year",
        y = "Average Life Expectancy",
        title = "Life Expectancy Over Time"
      ) +
      theme_minimal() +
      scale_colour_paletteer_d("rcartocolor::Antique")
  })
}
  
  
shinyApp(ui, server) 
  