library(shiny)
library(arrow)
library(dplyr)
library(stringr)

vault_path <- "raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Literature Vault: Search & Tagging"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Text Search
      textInput("query", "Search in Text:", placeholder = "e.g. decoding|phonics"),
      
      helpText("Boolean Tips:"),
      tags$ul(style = "font-size: 11px; color: #555;",
        tags$li(tags$b("|"), " for OR"),
        tags$li(tags$b(".*"), " for AND")
      ),
      hr(),
      
      # 2. Multi-Tag Filter Logic
      uiOutput("tag_filter_ui"),
      checkboxInput("exclude_tag", "Exclude selected tags instead", value = FALSE),
      
      actionButton("search_btn", "Apply Filters", class = "btn-primary", width = "100%"),
      hr(),
      
      # Active Filters Info Box
      uiOutput("active_filters_box"),
      hr(),
      
      uiOutput("result_count"),
      helpText("Note: Rendering is capped at the top 50 results for speed.")
    ),
    
    mainPanel(
      tags$head(
        tags$style(HTML("
          .para-card { border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; background-color: #fff; border-radius: 8px; box-shadow: 2px 2px 5px rgba(0,0,0,0.05); }
          .para-text { font-size: 18px; line-height: 1.6; color: #2c3e50; margin-bottom: 15px; }
          .tag-input-area { background: #f1f1f1; padding: 10px; border-radius: 5px; border-top: 1px solid #ccc; }
          .meta-info { font-size: 12px; color: #7f8c8d; font-family: monospace; }
          .filter-status { padding: 10px; border-radius: 5px; font-size: 12px; line-height: 1.4; }
          .status-active { background-color: #e7f3fe; border: 1px solid #b6d4fe; color: #084298; }
          .status-exclude { background-color: #f8d7da; border: 1px solid #f5c2c7; color: #842029; }
        "))
      ),
      uiOutput("paragraphs_ui")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. LOAD DATA ONCE INTO RAM
  val <- reactiveValues(db = {
    if(!file.exists(vault_path)) return(data.frame())
    showNotification("Loading 190k rows into memory...", type = "message", duration = NULL, id = "load_msg")
    d <- read_parquet(vault_path)
    removeNotification("load_msg")
    
    if (!"tags" %in% names(d)) d$tags <- ""
    d <- d %>% mutate(row_id = row_number()) 
    d
  })
  
  current_view <- reactiveVal(data.frame())
  
  # Generate Multi-Select Tag Choices
  output$tag_filter_ui <- renderUI({
    available_tags <- val$db$tags %>% 
      str_split(",\\s*") %>% unlist() %>% 
      unique() %>% setdiff(c("", NA)) %>% sort()
    
    selectizeInput("filter_tag", "Filter by Tag(s):", 
                   choices = available_tags, 
                   multiple = TRUE, 
                   options = list(placeholder = 'Select tags to include/exclude'))
  })
  
  # Active Filters Display
  output$active_filters_box <- renderUI({
    query_text <- if(nchar(input$query) > 0) input$query else "None"
    tag_list <- if(!is.null(input$filter_tag) && length(input$filter_tag) > 0) {
      paste(input$filter_tag, collapse = ", ")
    } else {
      "None"
    }
    
    status_class <- if(input$exclude_tag && tag_list != "None") "filter-status status-exclude" else "filter-status status-active"
    prefix <- if(input$exclude_tag && tag_list != "None") "EXCLUDING: " else "Showing: "

    div(class = status_class,
        tags$b("Active Search:"), tags$br(),
        paste0("Keywords: ", query_text), tags$br(),
        paste0("Tags (", prefix, "): ", tag_list)
    )
  })

  # 2. IMPROVED MULTI-TAG FILTER LOGIC
  observeEvent(input$search_btn, {
    res <- val$db
    
    # Text search
    if(nchar(input$query) > 0) {
      res <- res %>% filter(str_detect(text, regex(input$query, ignore_case = TRUE)))
    }
    
    # Multi-tag handling using regex pattern
    if(!is.null(input$filter_tag) && length(input$filter_tag) > 0) {
      # Creates pattern like "short|doi|long"
      tag_pattern <- paste(input$filter_tag, collapse = "|")
      
      if (input$exclude_tag) {
        # Hide rows containing ANY of the selected tags
        res <- res %>% filter(!str_detect(tags, regex(tag_pattern, ignore_case = TRUE)))
      } else {
        # Show rows containing ANY of the selected tags
        res <- res %>% filter(str_detect(tags, regex(tag_pattern, ignore_case = TRUE)))
      }
    }
    current_view(res)
  }, ignoreNULL = FALSE)

  # 3. UI RENDERING (Capped at 50)
  output$paragraphs_ui <- renderUI({
    data <- current_view()
    if (nrow(data) == 0) return(p("No results match your filters."))
    display_data <- head(data, 50)
    
    lapply(1:nrow(display_data), function(i) {
      this_id <- display_data$row_id[i]
      div(class = "para-card",
          div(class = "meta-info", paste0(display_data$source_file[i], " | Page ", display_data$page[i])),
          div(class = "para-text", display_data$text[i]),
          div(class = "tag-input-area",
              fluidRow(
                column(9, textInput(paste0("tag_", this_id), NULL, 
                                    value = display_data$tags[i], 
                                    placeholder = "Enter tags...")),
                column(3, actionButton(paste0("save_", this_id), "Save", 
                                       class = "btn-success btn-sm", width = "100%"))
              )
          )
      )
    })
  })

  # 4. TARGETED SAVE LOGIC
  observe({
    visible_data <- head(current_view(), 50)
    if(nrow(visible_data) == 0) return()
    lapply(visible_data$row_id, function(id) {
      save_id <- paste0("save_", id)
      observeEvent(input[[save_id]], {
        new_tag_val <- input[[paste0("tag_", id)]]
        row_idx <- which(val$db$row_id == id)
        
        val$db$tags[row_idx] <<- new_tag_val
        write_parquet(val$db, vault_path)
        showNotification("Tag saved!", type = "message", duration = 2)
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  output$result_count <- renderUI({
    tags$b(paste("Found", nrow(current_view()), "total paragraphs"))
  })
}

shinyApp(ui, server)