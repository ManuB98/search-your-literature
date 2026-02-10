library(shiny)
library(arrow)
library(dplyr)
library(tidytext)
library(ggplot2)
library(reactable)
library(stringr)  # Added this to fix the str_detect error
library(forcats)  # Added for better plot ordering

vault_path <- "/Users/manuelbruns/Coding/search-your-literature/raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Thematic Literature Overview"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("target_file", "Select Source File:", choices = NULL, multiple = TRUE),
      sliderInput("top_n", "Words per Topic:", 5, 20, 10),
      hr(),
      helpText("This view identifies the most unique words for each PDF using TF-IDF.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Thematic Chart", plotOutput("topic_plot", height = "700px")),
        tabPanel("Data Browser", reactableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load data once
  raw_data <- reactive({
    if(!file.exists(vault_path)) return(NULL)
    read_parquet(vault_path)
  })

  # Update UI choices based on available files
  observe({
    req(raw_data())
    files <- unique(raw_data()$source_file)
    updateSelectInput(session, "target_file", 
                      choices = files,
                      selected = head(files, 4))
  })

  # Compute TF-IDF (The "Thematic" Logic)
  thematic_stats <- reactive({
    req(raw_data(), input$target_file)
    
    # Custom stop words to clean academic noise
    stop_vec <- c(stop_words$word, "page", "id", "fig", "table", "et", "al", "section")
    
    raw_data() %>%
      filter(source_file %in% input$target_file) %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_vec) %>%
      filter(str_detect(word, "[a-z]")) %>% # str_detect now found!
      count(source_file, word) %>%
      bind_tf_idf(word, source_file, n) %>%
      group_by(source_file) %>%
      slice_max(tf_idf, n = input$top_n, with_ties = FALSE) %>%
      ungroup()
  })

  # Render static plot
  output$topic_plot <- renderPlot({
    req(thematic_stats())
    
    ggplot(thematic_stats(), aes(tf_idf, reorder_within(word, tf_idf, source_file), fill = source_file)) +
      geom_col(show.legend = FALSE) +
      scale_y_reordered() +
      facet_wrap(~source_file, scales = "free", ncol = 2) +
      labs(x = "Thematic Importance (TF-IDF)", y = NULL, 
           title = "Top Keywords by Document") +
      theme_minimal(base_size = 15) +
      theme(panel.spacing = unit(2, "lines")) # Add some breathing room between charts
  })

  # Render fast table
  output$table <- renderReactable({
    req(raw_data())
    reactable(
      raw_data() %>% select(source_file, page, links_to, text),
      searchable = TRUE,
      compact = TRUE,
      columns = list(
        text = colDef(minWidth = 300),
        source_file = colDef(maxWidth = 150)
      )
    )
  })
}

shinyApp(ui, server)