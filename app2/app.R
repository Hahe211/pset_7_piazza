library(tidyverse)
library(dplyr)
library(shiny)
library(fs)
library(reshape2)

# 
# table <- read_rds("all_congress.rds") %>% 
#   select(rep_win, rep_adv, accuracy, district) %>% 
#   distinct(district, state, rep_win, rep_adv, accuracy) %>% 
#   select(-district)

table <- read_rds("all_congress.rds") %>% 
  mutate(race_eth = fct_collapse(race_eth,
                             "White" = "White",
                              "Black" = "Black", 
                              "Asian" = "Asian", 
                               "Other" = "Other",
                             "Hispanic" = "Hispanic"
                            
  )) 


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "Accuracy vs. Win by Race/Ethnicity"),
      numericInput("size", "Point size", 1, 1),
       checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("colour", "Point colour",
                   choices = c("blue", "red", "green", "black")),
      # Add a race_eth dropdown selector
      selectInput("race_eth", "race_eth",
                  choices = levels(table$race_eth),
                  multiple = TRUE,
                  selected = "White")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the table dataset by the chosen race_eths
    table <- subset(table,
                   race_eth %in% input$race_eth)
    
    
    p <- ggplot(table, aes(rep_adv, accuracy)) +
      geom_point(size = input$size, col = input$colour) +
      #scale_x_log10() +
      ggtitle(input$title)
    
     if (input$fit) {
       p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)