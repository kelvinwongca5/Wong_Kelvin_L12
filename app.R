# Import Libraries
library(shiny)
library(sf)
library(maps)
library(mapproj)
library(tools)
library(tidyverse)
library(statebins)
library(rsconnect)


# Read county data
counties <- sf::read_sf('data/county_data.shp')%>%
  mutate(state = str_to_title(state))

state_list <- unique(counties$state)

ui <- fluidPage(
  titlePanel(tags$h1("County Demographic Map by State", style = "font-weight: normal;")),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
               information from the 2010 US Census."),
      
      selectInput("state",
                  label = "Choose a state to display",
                  choices = state_list),
      selected = "California",
      
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Percent White",
                              "Percent Black",
                              "Percent Hispanic",
                              "Percent Asian"),
                  selected = "Percent White"),
      
      
    ),
    mainPanel(plotOutput("map")
    )
  ))


# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    state_data <- counties %>% filter(state == input$state)
    
    data <- switch(input$var, 
                   "Percent White" = state_data$white,
                   "Percent Black" = state_data$black,
                   "Percent Hispanic" = state_data$hispanic,
                   "Percent Asian" = state_data$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    ggplot(state_data) +
      geom_sf(aes(fill = data)) +
      scale_fill_gradient(low = "white", 
                          high = color, 
                          limits = c(0,100)) +
      theme_void() +
      labs(
        title = input$state, 
        fill = legend
      ) +
      theme(plot.title = element_text(size = 24,
                                      hjust = 0.5))
    
  })
}


# Run app ----
shinyApp(ui = ui, server = server)