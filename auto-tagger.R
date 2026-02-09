library(arrow)
library(dplyr)
library(stringr)

vault_path <- "raw-literature/literature_vault.parquet"

if (!file.exists(vault_path)) stop("Vault not found!")

# 1. Load the database
db <- read_parquet(vault_path)

# Ensure tags column exists
if (!"tags" %in% names(db)) db$tags <- ""

message("Starting auto-tagging process...")

# 2. Define the logic
db <- db %>%
  mutate(
    # Count words (splitting by spaces)
    word_count = str_count(text, "\\w+"),
    
    # Define length tag
    length_tag = case_when(
      word_count < 35 ~ "short",
      word_count >= 35 & word_count < 250 ~ "middle",
      word_count >= 250 ~ "long"
    ),
    
    # Define DOI tag
    doi_tag = if_else(str_detect(text, fixed("doi.org", ignore_case = TRUE)), "doi", NA_character_)
  ) %>%
  # 3. Merge new tags with existing tags
  rowwise() %>%
  mutate(
    # Helper to combine tags without duplicates or messy commas
    existing_tags_list = list(str_split(tags, ",\\s*")[[1]]),
    new_tags_list = list(c(length_tag, doi_tag)),
    combined_tags = list(unique(na.omit(c(unlist(existing_tags_list), unlist(new_tags_list))))),
    tags = paste(combined_tags[combined_tags != ""], collapse = ", ")
  ) %>%
  ungroup() %>%
  select(-word_count, -length_tag, -doi_tag, -existing_tags_list, -new_tags_list, -combined_tags)

# 4. Save back to Parquet
write_parquet(db, vault_path)

message("✅ Auto-tagging complete! Length and DOI tags added.")