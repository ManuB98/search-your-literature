library(shiny)
library(arrow)
library(dplyr)
library(stringr)

# Path to your parquet file
vault_path <- "raw-literature/literature_vault.parquet"

# --- DATA PRE-PROCESSING (Run once before app starts) ---
if (file.exists(vault_path)) {
  temp_df <- read_parquet(vault_path)
  
  # Ensure columns exist
  if (!"tags" %in% names(temp_df)) temp_df$tags <- ""
  if (!"row_id" %in% names(temp_df)) temp_df$row_id <- seq_len(nrow(temp_df))
  
  # Inject ID into text field if it hasn't been done yet
  # Using a more robust check to prevent double injection
  needs_id <- !grepl("^\\[ID:", temp_df$text)
  if (any(needs_id)) {
    temp_df$text[needs_id] <- paste0("[ID: ", temp_df$row_id[needs_id], "] ", temp_df$text[needs_id])
    write_parquet(temp_df, vault_path)
  }
  rm(temp_df) # Clean up memory
}

ui <- fluidPage(
  titlePanel("Literature Vault: Search & Tagging"),
  tags$head(
    tags$style(HTML("
      /* UI Styling */
      .para-card { border: 1px solid #ddd; padding: 20px; margin-bottom: 30px; background-color: #fff; border-radius: 8px; }
      .para-text { font-size: 18px; line-height: 1.6; color: #2c3e50; margin-bottom: 15px; }
      .meta-info { font-size: 12px; color: #7f8c8d; font-family: monospace; margin-bottom: 10px; }
      
      @media print {
        @page { margin: 1.5cm; }
        .para-card { 
          display: block !important;
          page-break-before: always !important; 
          break-before: page !important;
          min-height: 95vh !important; 
          border: none !important;
          box-shadow: none !important;
        }
        .para-card::after {
          content: '\\00a0';
          display: block;
          page-break-before: always !important;
          break-before: page !important;
        }
        .btn, .nav-tabs, hr, .titlePanel, #result_summary, .tag-input-row { display: none !important; }
        .para-text { font-size: 16pt !important; color: black !important; line-height: 1.5; }
      }
    "))
  ),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Search & Filter",
      fluidRow(
        column(4, offset = 4, style = "margin-top: 50px;",
          wellPanel(
            textInput("query", "Search in Text:", placeholder = "e.g. AI.*black box"),
            uiOutput("tag_filter_ui"),
            checkboxInput("exclude_tag", "Exclude selected tags instead", value = FALSE),
            actionButton("search_btn", "Apply Filters & View Results", class = "btn-primary btn-lg", width = "100%")
          )
        )
      )
    ),
    tabPanel("Literature Results",
      fluidRow(
        column(10, offset = 1,
          div(style = "margin-top: 20px;",
            uiOutput("result_summary"),
            hr(),
            uiOutput("paragraphs_ui")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load data into reactiveValues AFTER the pre-processing check above
  val <- reactiveValues(db = {
    if(!file.exists(vault_path)) data.frame() else read_parquet(vault_path)
  })
  
  current_view <- reactiveVal(data.frame()) 
  
  output$tag_filter_ui <- renderUI({
    req(val$db)
    available_tags <- val$db$tags %>% str_split(",\\s*") %>% unlist() %>% 
      unique() %>% setdiff(c("", NA)) %>% sort()
    selectizeInput("filter_tag", "Filter by Tag(s):", choices = available_tags, multiple = TRUE)
  })

  observeEvent(input$search_btn, {
    res <- val$db
    if(nchar(input$query) > 0) {
      res <- res %>% filter(str_detect(text, regex(input$query, ignore_case = TRUE)))
    }
    if(!is.null(input$filter_tag) && length(input$filter_tag) > 0) {
      tag_pattern <- paste(input$filter_tag, collapse = "|")
      if (input$exclude_tag) res <- res %>% filter(!str_detect(tags, tag_pattern))
      else res <- res %>% filter(str_detect(tags, tag_pattern))
    }
    current_view(res)
    updateTabsetPanel(session, "main_tabs", selected = "Literature Results")
  })

  output$paragraphs_ui <- renderUI({
    data <- current_view()
    if (nrow(data) == 0) return(p("No results found."))
    
    display_data <- head(data, 50)

    lapply(1:nrow(display_data), function(i) {
      this_id <- display_data$row_id[i]
      div(class = "para-card",
          div(class = "meta-info", paste0(display_data$source_file[i], " | Page ", display_data$page[i])),
          div(class = "para-text", display_data$text[i]),
          div(class = "tag-input-row",
              fluidRow(
                column(8, textInput(paste0("tag_", this_id), "Tags:", value = display_data$tags[i])),
                column(4, style = "margin-top: 25px;", actionButton(paste0("save_", this_id), "Save", class = "btn-success", width = "100%"))
              )
          )
      )
    })
  })

  # Handle Saving
  observe({
    data <- head(current_view(), 50)
    if(nrow(data) == 0) return()
    
    lapply(data$row_id, function(id) {
      observeEvent(input[[paste0("save_", id)]], {
        row_idx <- which(val$db$row_id == id)
        val$db$tags[row_idx] <<- input[[paste0("tag_", id)]]
        write_parquet(val$db, vault_path)
        showNotification(paste("ID", id, "saved."))
      }, ignoreInit = TRUE)
    })
  })

  output$result_summary <- renderUI({
    tags$h4(paste("Showing", nrow(head(current_view(), 50)), "of", nrow(current_view()), "results"))
  })
}

shinyApp(ui, server)