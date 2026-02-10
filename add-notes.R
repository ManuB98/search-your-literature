library(httr2)
library(stringr)
library(purrr)
library(dotenv)
library(here)
library(arrow)   
library(dplyr)   

api_key <- Sys.getenv("MISTRAL_API_KEY")
input_dir <- "/Users/manuelbruns/Documents/Literatur"
output_dir <- here("raw-literature")
vault_path <- file.path(output_dir, "literature_vault.parquet")
backup_dir <- file.path(output_dir, "backups")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

# --- HELPER FUNCTIONS ---

process_pdf_to_df <- function(pdf_path) {
  pdf_name <- basename(pdf_path)
  message(paste("\n--- Processing:", pdf_name, "---"))
  
  # 1. API Flow
  upload_req <- request("https://api.mistral.ai/v1/files") %>%
    req_auth_bearer_token(api_key) %>%
    req_body_multipart(file = curl::form_file(pdf_path), purpose = "ocr") %>%
    req_perform()
  
  file_id <- resp_body_json(upload_req)$id
  
  url_req <- request(paste0("https://api.mistral.ai/v1/files/", file_id, "/url")) %>%
    req_auth_bearer_token(api_key) %>%
    req_perform()
  
  signed_url <- resp_body_json(url_req)$url
  
  ocr_req <- request("https://api.mistral.ai/v1/ocr") %>%
    req_auth_bearer_token(api_key) %>%
    req_body_json(list(
      model = "mistral-ocr-latest",
      document = list(type = "document_url", document_url = signed_url)
    )) %>%
    req_perform()
  
  ocr_res <- resp_body_json(ocr_req)
  
  # 2. Flatten Pages
  all_paras_raw <- map2_df(ocr_res$pages, seq_along(ocr_res$pages), function(page_data, page_num) {
    raw_text <- page_data$markdown %||% ""
    clean_text <- str_replace_all(raw_text, "!\\[.*?\\]", "")
    paras <- unlist(strsplit(clean_text, "\n\n", fixed = TRUE))
    paras <- str_squish(paras)
    paras <- paras[paras != ""]
    if(length(paras) > 0) data.frame(page = page_num, text = paras, stringsAsFactors = FALSE) else NULL
  })

  if (nrow(all_paras_raw) == 0) return(NULL)

  # 3. LOGIC: Extract ID, Delete ID Paragraphs, Keep Your Notes
  processed_list <- list()
  current_link_id <- NA_character_
  
  last_page <- -1
  current_page_idx <- 0

  for (i in 1:nrow(all_paras_raw)) {
    this_text <- all_paras_raw$text[i]
    this_page <- all_paras_raw$page[i]
    
    found_id_match <- str_extract(this_text, "(?<=\\[ID:\\s)\\d+")
    is_header <- grepl("\\.pdf\\s*\\|\\s*Page", this_text, ignore.case = TRUE)
    
    if (!is.na(found_id_match)) {
      current_link_id <- found_id_match
      message(paste("   > Pointer updated to ID:", current_link_id))
      next 
    }
    
    if (is_header) {
      next 
    }

    if (this_page != last_page) {
      current_page_idx <- 1
      last_page <- this_page
    } else {
      current_page_idx <- current_page_idx + 1
    }

    processed_list[[length(processed_list) + 1]] <- data.frame(
      source_file = pdf_name,
      page = this_page,
      para_idx = current_page_idx,
      links_to = as.character(current_link_id),
      text = this_text,
      stringsAsFactors = FALSE
    )
  }
  
  return(bind_rows(processed_list))
}

# --- MAIN INTERFACE ---

main <- function() {
  # Backup and Load Existing
  if (file.exists(vault_path)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file.copy(vault_path, file.path(backup_dir, paste0("vault_backup_", ts, ".parquet")))
    message("🛡️ Backup created.")
    existing_vault <- read_parquet(vault_path)
    
    if(!"links_to" %in% names(existing_vault)) existing_vault$links_to <- NA_character_
    if(!"row_id" %in% names(existing_vault)) existing_vault$row_id <- seq_len(nrow(existing_vault))
    
    processed_files <- unique(existing_vault$source_file)
  } else {
    existing_vault <- data.frame()
    processed_files <- c()
  }

  pdf_files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE)
  if (length(pdf_files) == 0) return(message("No PDFs found."))

  message("\nAvailable Papers:")
  for (i in seq_along(pdf_files)) {
    status <- if(basename(pdf_files[i]) %in% processed_files) "[Already Processed]" else ""
    cat(sprintf("[%d] %s %s\n", i - 1, basename(pdf_files[i]), status))
  }
  
  user_input <- readline(prompt = "\nEnter numbers or 'all': ")
  indices <- if(user_input == "all") seq_along(pdf_files) else as.numeric(unlist(strsplit(user_input, ","))) + 1
  
  new_data_list <- list()
  for (pdf in pdf_files[indices]) {
    tryCatch({
      new_data_list[[pdf]] <- process_pdf_to_df(pdf)
    }, error = function(e) message("❌ Error: ", e$message))
  }
  
  if (length(new_data_list) > 0) {
    all_new_data <- bind_rows(new_data_list)
    all_new_data$links_to <- as.character(all_new_data$links_to)
    
    # --- ROW_ID ASSIGNMENT ---
    # Find the current maximum ID to continue the sequence
    start_id <- if(nrow(existing_vault) > 0) max(existing_vault$row_id, na.rm = TRUE) else 0
    all_new_data$row_id <- seq(from = start_id + 1, length.out = nrow(all_new_data))
    
    # Merge
    if(nrow(existing_vault) > 0) {
       existing_vault$links_to <- as.character(existing_vault$links_to)
       final_vault <- bind_rows(existing_vault, all_new_data)
    } else {
       final_vault <- all_new_data
    }
    
    write_parquet(final_vault, vault_path)
    message("\n✅ Vault updated. New row_ids assigned starting from ", start_id + 1)
  }
}

main()