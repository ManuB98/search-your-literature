library(shiny)
library(arrow)
library(dplyr)
library(stringr)

vault_path <- "raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Literature Vault: Search & Tagging"),
  tags$head(
    tags$style(HTML("
      .para-card { border: 1px solid #ddd; padding: 20px; margin-bottom: 30px; background-color: #fff; border-radius: 8px; box-shadow: 2px 2px 5px rgba(0,0,0,0.05); }
      .para-text { font-size: 18px; line-height: 1.6; color: #2c3e50; margin-bottom: 20px; }
      .comment-area { background: #fdfdfd; padding: 15px; border-radius: 5px; border: 1px dashed #ccc; margin-top: 15px; }
      .meta-info { font-size: 12px; color: #7f8c8d; font-family: monospace; margin-bottom: 10px; }
      .shiny-input-container { width: 100% !important; }

      @media print {
        .para-card { page-break-before: always !important; break-before: page !important; border: none !important; box-shadow: none !important; display: block !important; }
        .para-text, .comment-area { page-break-inside: auto !important; display: block !important; }
        .btn, .sidebar, #main_tabs > li, .nav-tabs, hr, .filter-status, .control-label { display: none !important; }
        .col-sm-10 { width: 100% !important; margin: 0 !important; }
        textarea { border: 1px solid #eee !important; width: 100% !important; }
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
            actionButton("search_btn", "Apply Filters & View Results", class = "btn-primary btn-lg", width = "100%"),
            hr(),
            uiOutput("active_filters_box")
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
  
  # 1. DATA LOADING - Forcefully fix NULL IDs
  val <- reactiveValues(db = {
    if(!file.exists(vault_path)) return(data.frame())
    d <- read_parquet(vault_path)
    
    if (!"tags" %in% names(d)) d$tags <- ""
    if (!"comments" %in% names(d)) d$comments <- "" 
    
    # FORCE FIX: If row_id is missing OR contains NULL/NA, overwrite and save
    if (!"row_id" %in% names(d) || any(is.na(d$row_id)) || any(is.null(d$row_id))) {
        d$row_id <- seq_len(nrow(d))
        write_parquet(d, vault_path)
    }
    d
  })
  
  # Initialise with an empty dataframe so the UI doesn't break, but is ready
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
    if (is.null(data) || nrow(data) == 0) {
      return(p("No results to display. Please enter a search term and click Apply Filters."))
    }
    
    display_data <- head(data, 50)

    lapply(1:nrow(display_data), function(i) {
      this_id <- display_data$row_id[i]
      
      div(class = "para-card",
          div(class = "meta-info", paste0("ID: ", this_id, " | ", display_data$source_file[i], " | Page ", display_data$page[i])),
          div(class = "para-text", display_data$text[i]),
          div(class = "comment-area",
              tags$b(paste0("Personal Comments on Chunk ", this_id, ":")),
              textAreaInput(paste0("comm_", this_id), NULL, value = display_data$comments[i], rows = 45),
              div(style = "margin-top: 15px; font-style: italic; color: #000607ff; border-top: 1px solid #eee; padding-top: 5px;",
                  paste0("End of my comments on chunk ", this_id)),
              fluidRow(
                column(6, textInput(paste0("tag_", this_id), "Tags:", value = display_data$tags[i])),
                column(3, style = "margin-top: 25px;", actionButton(paste0("save_", this_id), "Save", class = "btn-success", width = "100%")),
                column(3, style = "margin-top: 25px;", actionButton(paste0("read_", this_id), "Reader", class = "btn-info", width = "100%"))
              )
          )
      )
    })
  })

  # 4. SAVE & MODAL LOGIC
  observe({
    data <- head(current_view(), 50)
    if(nrow(data) == 0) return()
    
    lapply(data$row_id, function(id) {
      # Use ignoreInit to stop it firing when the UI first draws
      observeEvent(input[[paste0("save_", id)]], {
        row_idx <- which(val$db$row_id == id)
        val$db$tags[row_idx] <<- input[[paste0("tag_", id)]]
        val$db$comments[row_idx] <<- input[[paste0("comm_", id)]]
        write_parquet(val$db, vault_path)
        showNotification(paste("Saved ID", id))
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("read_", id)]], {
        row_data <- val$db[val$db$row_id == id, ]
        showModal(modalDialog(
          title = paste("ID:", id, "| Source:", row_data$source_file),
          size = "l", easyClose = TRUE,
          div(id = paste0("print_content_", id), p(style="font-family:serif; font-size:20px;", row_data$text)),
          footer = tagList(
            modalButton("Close"),
            actionButton("do_print", "Open Clean Tab", class = "btn-primary", 
               onclick = sprintf("var content = document.getElementById('print_content_%s').innerHTML; var win = window.open('', '_blank'); win.document.write('<html><head><title>ID %s</title></head><body>' + content + '</body></html>'); win.document.close();", id, id))
          )
        ))
      }, ignoreInit = TRUE)
    })
  })

  output$result_summary <- renderUI({
    data <- current_view()
    if(nrow(data) > 0) tags$h4(paste("Showing top 50 of", nrow(data), "results"))
  })
}

shinyApp(ui, server)