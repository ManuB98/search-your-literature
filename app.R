library(shiny)
library(arrow)
library(dplyr)
library(stringr)

vault_path <- "raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Literature Vault: Search & Tagging"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Text Search with Boolean Help
      textInput("query", "Search in Text:", placeholder = "e.g. decoding|phonics"),
      
      helpText("Boolean Tips:"),
      tags$ul(style = "font-size: 11px; color: #555;",
        tags$li(tags$b("|"), " for OR (e.g., 'decoding|phonics')"),
        tags$li(tags$b(".*"), " for AND (e.g., 'reading.*comprehension')"),
        tags$li(tags$b("( )"), " for grouping (e.g., '(word|ortho).*reading')")
      ),
      hr(),
      
      # 2. Tag Filter & Exclude Toggle
      uiOutput("tag_filter_ui"),
      checkboxInput("exclude_tag", "Exclude this tag instead", value = FALSE),
      
      actionButton("search_btn", "Apply Filters", class = "btn-primary", width = "100%"),
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
        "))
      ),
      uiOutput("paragraphs_ui")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. LOAD DATABASE INTO RAM (Once at startup)
  val <- reactiveValues(db = {
    if(!file.exists(vault_path)) return(data.frame())
    showNotification("Loading 190k rows into memory...", type = "message", duration = NULL, id = "load_msg")
    d <- read_parquet(vault_path)
    removeNotification("load_msg")
    
    if (!"tags" %in% names(d)) d$tags <- ""
    # Ensure stable row IDs for saving
    d <- d %>% mutate(row_id = row_number()) 
    d
  })
  
  # Reactive for currently filtered results
  current_view <- reactiveVal(data.frame())
  
  # Generate Tag Filter Choices dynamically
  output$tag_filter_ui <- renderUI({
    available_tags <- val$db$tags %>% 
      str_split(",\\s*") %>% 
      unlist() %>% 
      unique() %>% 
      setdiff(c("", NA)) %>%
      sort()
    
    selectInput("filter_tag", "Filter by Tag:", 
                choices = c("All" = "", available_tags), 
                selected = input$filter_tag)
  })
  
  # 2. FAST FILTERING LOGIC
  observeEvent(input$search_btn, {
    res <- val$db
    
    # Filter by Text (Regex)
    if(nchar(input$query) > 0) {
      res <- res %>% filter(str_detect(text, regex(input$query, ignore_case = TRUE)))
    }
    
    # Filter by Tag
    if(!is.null(input$filter_tag) && input$filter_tag != "") {
      if (input$exclude_tag) {
        res <- res %>% filter(!str_detect(tags, fixed(input$filter_tag)))
      } else {
        res <- res %>% filter(str_detect(tags, fixed(input$filter_tag)))
      }
    }
    
    current_view(res)
  }, ignoreNULL = FALSE)

  # 3. SELECTIVE SAVE LOGIC (Only observes the 50 visible items)
  observe({
    # Limit observers to only the visible rows to prevent memory leaks
    visible_data <- head(current_view(), 50)
    if(nrow(visible_data) == 0) return()
    
    lapply(visible_data$row_id, function(id) {
      save_id <- paste0("save_", id)
      
      observeEvent(input[[save_id]], {
        new_tag_val <- input[[paste0("tag_", id)]]
        row_idx <- which(val$db$row_id == id)
        
        # Update RAM
        val$db$tags[row_idx] <<- new_tag_val
        
        # Update Disk
        write_parquet(val$db, vault_path)
        showNotification("Tag saved!", type = "message", duration = 2)
      }, ignoreInit = TRUE, once = TRUE) # 'once' helps with performance
    })
  })
  
  output$result_count <- renderUI({
    n_total <- nrow(current_view())
    tags$b(paste("Found", n_total, "paragraphs (Showing top 50)"))
  })
  
  # 4. FAST UI RENDERING
  output$paragraphs_ui <- renderUI({
    data <- current_view()
    if (nrow(data) == 0) return(p("No results match your filters."))
    
    # LIMIT: Only render top 50 cards to keep the browser responsive
    display_data <- head(data, 50)
    
    lapply(1:nrow(display_data), function(i) {
      this_id <- display_data$row_id[i]
      
      div(class = "para-card",
          div(class = "meta-info", 
              paste0("SOURCE: ", display_data$source_file[i], " | PAGE: ", display_data$page[i])),
          div(class = "para-text", display_data$text[i]),
          
          div(class = "tag-input-area",
              fluidRow(
                column(9, textInput(paste0("tag_", this_id), label = NULL, 
                                    value = display_data$tags[i], 
                                    placeholder = "Enter tags...")),
                column(3, actionButton(paste0("save_", this_id), "Save", 
                                       class = "btn-success btn-sm", width = "100%"))
              )
          )
      )
    })
  })
}

shinyApp(ui, server)