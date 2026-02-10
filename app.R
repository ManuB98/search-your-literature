library(shiny)
library(arrow)
library(dplyr)
library(stringr)

# Path to your parquet file
vault_path <- "raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Literature Vault: Search & Tagging"),
  tags$head(
    tags$style(HTML("
      /* UI Styling for the Browser */
      .para-card { border: 1px solid #ddd; padding: 20px; margin-bottom: 30px; background-color: #fff; border-radius: 8px; box-shadow: 2px 2px 5px rgba(0,0,0,0.05); }
      .para-text { font-size: 18px; line-height: 1.6; color: #2c3e50; margin-bottom: 15px; }
      .meta-info { font-size: 12px; color: #7f8c8d; font-family: monospace; margin-bottom: 10px; }
      
      /* PRINT LOGIC */
      @media print {
        @page { 
          margin: 1.5cm; 
        }

        /* 1. Force every card to start on its own page and occupy the full height */
        .para-card { 
          display: block !important;
          page-break-before: always !important; 
          break-before: page !important;
          border: none !important;
          box-shadow: none !important;
          /* Occupies the full page so even short chunks don't share space */
          min-height: 95vh !important; 
          margin: 0 !important;
          padding: 0 !important;
        }

        /* 2. Force the second (empty) page after every card */
        .para-card::after {
          content: '\\00a0'; /* Non-breaking space */
          display: block;
          page-break-before: always !important;
          break-before: page !important;
          visibility: hidden;
        }

        /* 3. Hide all app controls and buttons */
        .btn, .sidebar, #main_tabs > li, .nav-tabs, hr, .filter-status, 
        .control-label, .titlePanel, #result_summary, .tag-input-row, .shiny-notification-wrapper { 
          display: none !important; 
        }
        
        /* 4. Formatting for the printed text */
        .para-text { 
          font-size: 16pt !important; 
          color: black !important; 
          line-height: 1.5;
          page-break-inside: avoid !important;
        }
        .meta-info { font-size: 10pt !important; border-bottom: 1px solid #eee; padding-bottom: 5px; }
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
  
  # 1. DATA LOADING & ID INJECTION
  val <- reactiveValues(db = {
    if(!file.exists(vault_path)) return(data.frame())
    d <- read_parquet(vault_path)
    
    # Ensure columns exist
    if (!"tags" %in% names(d)) d$tags <- ""
    if (!"row_id" %in% names(d)) d$row_id <- seq_len(nrow(d))
    
    # Inject ID into text field if it hasn't been done yet
    d$text <- ifelse(grepl("^\\[ID:", d$text), 
                     d$text, 
                     paste0("[ID: ", d$row_id, "] ", d$text))
    
    write_parquet(d, vault_path)
    d
  })
  
  current_view <- reactiveVal(data.frame()) 
  
  output$tag_filter_ui <- renderUI({
    available_tags <- val$db$tags %>% str_split(",\\s*") %>% unlist() %>% 
      unique() %>% setdiff(c("", NA)) %>% sort()
    selectizeInput("filter_tag", "Filter by Tag(s):", choices = available_tags, multiple = TRUE)
  })

  # 2. FILTER LOGIC
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
  }, ignoreNULL = FALSE)

  # 3. UI RENDERING
  output$paragraphs_ui <- renderUI({
    data <- current_view()
    if (is.null(data) || nrow(data) == 0) return(p("No results found. Please use the search tab."))
    
    # Limit display to 50 for performance
    display_data <- head(data, 50)

    lapply(1:nrow(display_data), function(i) {
      this_id <- display_data$row_id[i]
      
      div(class = "para-card",
          div(class = "meta-info", paste0(display_data$source_file[i], " | Page ", display_data$page[i])),
          div(class = "para-text", display_data$text[i]),
          # Tag Input Row: Visible in App for editing, hidden on paper
          div(class = "tag-input-row",
              fluidRow(
                column(8, textInput(paste0("tag_", this_id), "Edit Tags:", value = display_data$tags[i])),
                column(4, style = "margin-top: 25px;", actionButton(paste0("save_", this_id), "Save", class = "btn-success", width = "100%"))
              )
          )
      )
    })
  })

  # 4. SAVE LOGIC
  observe({
    data <- head(current_view(), 50)
    if(nrow(data) == 0) return()
    
    lapply(data$row_id, function(id) {
      observeEvent(input[[paste0("save_", id)]], {
        row_idx <- which(val$db$row_id == id)
        val$db$tags[row_idx] <<- input[[paste0("tag_", id)]]
        write_parquet(val$db, vault_path)
        showNotification(paste("ID", id, "tags updated."), type = "message")
      }, ignoreInit = TRUE)
    })
  })

  output$result_summary <- renderUI({
    data <- current_view()
    if(nrow(data) > 0) tags$h4(paste("Showing top 50 of", nrow(data), "results found"))
  })
}

shinyApp(ui, server)