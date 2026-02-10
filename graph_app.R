library(shiny)
library(arrow)
library(dplyr)
library(visNetwork)

vault_path <- "raw-literature/literature_vault.parquet"

ui <- fluidPage(
  titlePanel("Literature Vault: Knowledge Graph"),
  tags$style(HTML("
    #network { height: calc(100vh - 150px) !important; background: #222222; }
    .vis-network { outline: none; }
  ")),
  
  fluidRow(
    column(3,
      wellPanel(
        h4("Graph Filters"),
        helpText("Nodes are colored by type: Blue = Literature, Orange = Your Notes."),
        hr(),
        checkboxInput("show_orphans", "Show unconnected nodes", value = FALSE),
        actionButton("refresh", "Refresh Data", class = "btn-block")
      )
    ),
    column(9,
      visNetworkOutput("network")
    )
  )
)

server <- function(input, output, session) {
  
  # Load Data
  data <- reactive({
    input$refresh
    if(!file.exists(vault_path)) return(data.frame())
    read_parquet(vault_path)
  })
  
  output$network <- renderVisNetwork({
    df <- data()
    if(nrow(df) == 0) return(NULL)
    
    # 1. Prepare Nodes
    nodes <- df %>%
      transmute(
        id = row_id,
        label = paste0("ID: ", row_id),
        title = paste0("<div style='max-width:300px;'>", text, "</div>"), # Tooltip
        group = ifelse(is.na(links_to), "Literature", "Note"),
        color = ifelse(is.na(links_to), "#4da6ff", "#ffa64d")
      )
    
    # 2. Prepare Edges (Connections)
    # We draw a line from the Note to the ID it 'links_to'
    edges <- df %>%
      filter(!is.na(links_to)) %>%
      mutate(links_to = as.numeric(links_to)) %>%
      select(from = row_id, to = links_to)
    
    # 3. Handle Orphans (optional filter)
    if(!input$show_orphans) {
      connected_ids <- unique(c(edges$from, edges$to))
      nodes <- nodes %>% filter(id %in% connected_ids)
    }
    
    # 4. Build the Network
    visNetwork(nodes, edges) %>%
      visNodes(
        shape = "dot", 
        size = 15,
        font = list(color = "white")
      ) %>%
      visEdges(
        arrows = "to",
        color = list(color = "#666666", highlight = "#ffffff")
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based", 
        forceAtlas2Based = list(gravitationalConstant = -50)
      ) %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes.nodes[0]);
      }")
  })
}

shinyApp(ui, server)