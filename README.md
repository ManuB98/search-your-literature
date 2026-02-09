# search-your-literature

This project converts either all your PDFs or the PDFs of your choice to raw text stored in a parquet file that you can open in Positron. 

It also includes a shiny app that you can use to then search through your literature and add tags manually. 

Lastly, it includes an auto-tagger script that adds atomatic tags to you text paragraphs, depending on the lenght and if a doi is present. You could modify this script to add other automatic tags as well.

HOW TO USE:

For this to work, you will need to create a Mistral API-key from the Mistral Website to convert PDFs to text with the help of AI. You should add this API Key to your .Renviron file under MISTRAL_API_KEY='your_key_here'. 

You will also need to modify the input_dir in convert_literature.R (line 10) to the folder where you have saved all your literature PDFs.

Finally, make sure to have all these R-packages installed: 

install.packages(c(
  "httr2",    # API communication with Mistral
  "arrow",    # High-performance Parquet database handling
  "dplyr",    # Data manipulation and filtering
  "stringr",  # Advanced text searching and regex
  "shiny",    # The interactive search & tagging app
  "purrr",    # Functional programming for file loops
  "here",     # Robust file path management
  "dotenv",   # Managing the MISTRAL_API_KEY
  "pdftools", # Local PDF text extraction (backup/metadata)
  "curl"      # Handing file uploads to the API
))