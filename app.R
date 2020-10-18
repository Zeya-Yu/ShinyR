library(readxl)
library(tidyverse)
library(plyr)
library(stringr)
library(shiny)
library(shinydashboard)

'%not_in%' <- Negate('%in%')

schools <- read_excel("schools.xlsx", na = 'N/A')
schools <- as_tibble(schools)

## ranking
# delete '='
clean_rank_num <- function(str) {str_remove(str, '=$')}
schools[] <- lapply(schools, clean_rank_num)

# convert '401-450' to '401'
convert_rank <- function(ss) {
  for (i in 1:length(ss)) {
    if (! is.na(ss[i]) & str_detect(ss[i], '-')) {
      ss[i] <- str_remove(ss[i], '-.*')
    }
  }
  return(as.numeric(ss))
}
schools <- cbind(schools[2:25], lapply(schools[c(1, 26:30)], convert_rank))
countries <- c("United Kingdom", "Australia", "Germany")
rankings <- c("rank", "Art & Humanities Ranking", "Engineering&Tech Ranking",
              "Life Sciences & Medicine Ranking", "Natural Sciences Ranking",
              "Social Sciences & Management Ranking")
display <- list("Grade Requirement" = 1, "Rankings" = 2, "Expenses" = 3, "Doucments&Others" = 4)
GR <- c("gaokao_requirement", "gaokao_grade", "gaokao_grade_other",
        "GPA", "TOEFL", "IELTS","Duolingo","language_other")
Expenses <- c("tuitions","tuitions_other", "expenses_other",
              "application_fee", "scholarship_info","scholarship")
DO <- c("PS", "RL", "document_other", "DDL1", "enrollment1")

### create shinyR ###
ui <- fluidPage(
  titlePanel("Your dream school starts from here"),
  
  sidebarLayout(
    #define the sidebar
    sidebarPanel(
      selectInput(inputId = "country", label = "Choose a country:", choices = countries),
      selectInput(inputId = "rankings", label = "Choose the field:", choices = rankings),
      sliderInput(inputId = "rank", label = "rank:", 
                  min = 1, max = 1000, value = c(1,250)),
      checkboxGroupInput(inputId = "display", label = "Display:",
                         choices = display, selected = c(1,2,3,4)),
      hr(),
      actionButton(inputId = "search", label = "search")
    ),
    
    mainPanel(
      dataTableOutput(outputId = "data")
      
    )
  )
)

server <- function(input, output, session){
  search <- eventReactive(
    input$search, {
      print("Filter your dream schools...")
      df <- schools %>% 
        arrange(schools[,input$rankings]) %>%
        filter(country == input$country,
               schools[,input$rankings] >= input$rank[1],
               schools[,input$rankings] <= input$rank[2])
      
      if(1 %not_in% input$display){df <- df %>% select(-GR)}
      if(2 %not_in% input$display){df <- df %>% select(-rankings)}
      if(3 %not_in% input$display){df <- df %>% select(-Expenses)}
      if(4 %not_in% input$display){df <- df %>% select(-DO)}
      
      df
    }
  )
  
  
  
  
  output$data <- renderDataTable(
    search()
  )
}

shinyApp(ui, server)
