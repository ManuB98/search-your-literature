library(arrow)
library(dplyr)
library(stringr)
library(httr2)

vault_path <- "raw-literature/literature_vault.parquet"

# 1. CREATE BACKUP BEFORE STARTING
if (file.exists(vault_path)) {
  backup_name <- paste0("raw-literature/backup_vault_", format(Sys.time(), "%Y%m%d_%H%M"), ".parquet")
  file.copy(vault_path, backup_name)
  message("✅ Backup created: ", backup_name)
}

# 2. LOAD DATABASE
db <- read_parquet(vault_path)
if (!"tags" %in% names(db)) db$tags <- ""

# Identify rows that still need processing
rows_to_process <- which(!str_detect(db$tags, "bibliography|not_bib"))
total_to_do <- length(rows_to_process)

message("🚀 Starting AI Tagger. Total rows to process: ", total_to_do)

# 3. THE LLM FUNCTION
check_if_bib <- function(content) {
  clean_text <- str_trunc(content, 800) # Truncate for speed
  
  params <- list(
    model = "mistralai/ministral-3-14b-reasoning",
    messages = list(
      list(role = "system", content = "You are a research assistant. Respond ONLY with 'YES' or 'NO'."),
      list(role = "user", content = paste("Does this look like a bibliography entry?\n\n", clean_text))
    ),
    max_tokens = 5,
    temperature = 0.1
  )
  
  req <- request("http://127.0.0.1:1234/v1/chat/completions") %>%
    req_body_json(params) %>%
    req_timeout(60) %>% # Increased timeout to prevent disconnects
    req_retry(max_tries = 2) %>%
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    ans <- resp_body_json(resp)$choices[[1]]$message$content
    return(toupper(str_trim(ans)))
  } else {
    return("ERROR")
  }
}

# 4. PROCESSING LOOP
count <- 0
for (i in rows_to_process) {
  count <- count + 1
  
  # Call LLM
  ai_response <- check_if_bib(db$text[i])
  
  # Determine new tag
  new_tag <- case_when(
    ai_response == "YES"   ~ "bibliography",
    ai_response == "NO"    ~ "not_bib",
    TRUE                   ~ "bib_error" # For empty or failed responses
  )
  
  # Update internal DB
  current_tags <- db$tags[i]
  if(current_tags == "" || is.na(current_tags)) {
    db$tags[i] <- new_tag
  } else {
    db$tags[i] <- paste(unique(c(str_split(current_tags, ",\\s*")[[1]], new_tag)), collapse = ", ")
  }
  
  # FINE-GRAINED FEEDBACK: Print every line
  cat(sprintf("[%d/%d] | Row: %d | AI: %-12s | Text: %s...\n", 
              count, total_to_do, i, new_tag, str_sub(db$text[i], 1, 50)))
  
  # SAVE EVERY 15 LINES
  if (count %% 15 == 0) {
    write_parquet(db, vault_path)
    cat("💾 --- Checkpoint: Database saved to disk ---\n")

  if (count %% 30 == 0) {
  write_parquet(db, vault_path)
  cat("☕ Taking a 90-second cooling break...\n")
  Sys.sleep(90) 
}
  }
}

# Final save
write_parquet(db, vault_path)
message("✅ Finished! All rows processed.")