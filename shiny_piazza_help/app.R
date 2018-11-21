library(tidyverse)
library(dplyr)
library(shiny)
library(fs)
library(reshape2)


table <- read_rds("all_congress.rds") %>% 
  select(rep_win, rep_adv, accuracy, district) %>% 
  distinct(district, rep_win, rep_adv, accuracy) 




# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample Demographic and Poll Accuracy"),
  
  # Sidebar with a select input
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "category",
                  label = "select X axis",
                  choices =c(`Polled Republican Advantage` = "rep_adv",
                             `Actual Republican Advantage` = "rep_win"))),
    
    
    # Show a plot
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    table <- table %>%
      filter(input == input$category)
    
    table %>%
      ggplot(aes(x = input$category, y = accuracy)) +
      # create scatterplot
      geom_point() +
      # add title, subtitle
      labs(title = case_when(input$category == "rep_adv" ~ "Rep_adv",
                             input$category == "rep_win" ~ "Rep_win"),
           x = case_when(input$category == "rep_adv" ~ "Rep_adv",
                         input$category == "rep_win" ~ "rep_win"),
           
           y = "Poll Accuracy")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
