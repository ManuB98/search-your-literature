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

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- HELPER FUNCTIONS ---

# Function to process PDF via Mistral OCR and return a Data Frame
process_pdf_to_df <- function(pdf_path) {
  pdf_name <- basename(pdf_path)
  
  message(paste("\n--- Processing:", pdf_name, "---"))
  
  # 1. Upload & Get Signed URL (Same as your script)
  upload_req <- request("https://api.mistral.ai/v1/files") %>%
    req_auth_bearer_token(api_key) %>%
    req_body_multipart(file = curl::form_file(pdf_path), purpose = "ocr") %>%
    req_perform()
  
  file_id <- resp_body_json(upload_req)$id
  
  url_req <- request(paste0("https://api.mistral.ai/v1/files/", file_id, "/url")) %>%
    req_auth_bearer_token(api_key) %>%
    req_perform()
  
  signed_url <- resp_body_json(url_req)$url
  
  # 2. Process OCR
  ocr_req <- request("https://api.mistral.ai/v1/ocr") %>%
    req_auth_bearer_token(api_key) %>%
    req_body_json(list(
      model = "mistral-ocr-latest",
      document = list(type = "document_url", document_url = signed_url)
    )) %>%
    req_perform()
  
  ocr_res <- resp_body_json(ocr_req)
  
  # 3. Extract Paragraphs and Create tidy Data Frame
  # Loop through pages and split text into paragraphs
  df_list <- map2(ocr_res$pages, seq_along(ocr_res$pages), function(page_data, page_num) {
    raw_text <- page_data$markdown %||% ""
    # Clean image placeholders
    clean_text <- str_replace_all(raw_text, "!\\[.*?\\]", "")
    # Split by double newline to get paragraphs
    paras <- unlist(strsplit(clean_text, "\n\n", fixed = TRUE))
    paras <- str_squish(paras) # Remove extra whitespace
    paras <- paras[nchar(paras) > 50] # Only keep substantial text
    
    if(length(paras) > 0) {
      data.frame(
        source_file = pdf_name,
        page = page_num,
        para_idx = seq_along(paras),
        text = paras,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
  
  return(bind_rows(df_list))
}

# --- MAIN INTERFACE ---

main <- function() {
  pdf_files <- list.files(input_dir, pattern = "\\.pdf$", full.names = TRUE)
  if (length(pdf_files) == 0) return(message("No PDFs found."))
  
  # Check which files we already have in the vault to avoid double-paying for OCR
  if (file.exists(vault_path)) {
    existing_vault <- read_parquet(vault_path)
    processed_files <- unique(existing_vault$source_file)
    message("Files already in vault: ", length(processed_files))
  } else {
    existing_vault <- data.frame()
    processed_files <- c()
  }

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
  
  # Combine and save
  if (length(new_data_list) > 0) {
    all_new_data <- bind_rows(new_data_list)
    final_vault <- bind_rows(existing_vault, all_new_data)
    
    # Save as Parquet (Best for Positron)
    write_parquet(final_vault, vault_path)
    
    # Optional: Also save as CSV for manual Excel inspection
    # write.csv(final_vault, file.path(output_dir, "literature_vault.csv"), row.names = FALSE)
    
    message("\n✅ Vault updated! You can now open 'literature_vault.parquet' in the explorer.")
  }
}

main()