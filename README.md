# search-your-literature

This project converts either all your PDFs or the PDFs of your choice into raw text stored in a **Parquet** file, which can be viewed and queried directly in Positron.

It includes a **Shiny app** for searching through your literature using Boolean operators and adding research tags manually. Additionally, it features an **Auto-Tagger script** that automatically categorizes text paragraphs based on their length (short, middle, or long) and identifies the presence of a DOI. You can easily modify this script to add other automatic tags as needed.

---

### HOW TO USE:

1. **Mistral API Key:** You will need to generate an API key from the [Mistral Website](https://mistral.ai/) to convert PDFs into text. Add this key to your `.Renviron` file as follows:
`MISTRAL_API_KEY='your_key_here'`
2. **Input Directory:** Modify the `input_dir` variable in `convert_literature.R` (line 10) to point to the folder containing your literature PDFs.
3. **Required Packages:** Ensure you have the following R packages installed:

```r
install.packages(c( 
  "httr2",    # API communication with Mistral 
  "arrow",    # High-performance Parquet database handling 
  "dplyr",    # Data manipulation and filtering 
  "stringr",  # Advanced text searching and regex 
  "shiny",    # The interactive search & tagging app 
  "purrr",    # Functional programming for file loops 
  "here",     # Robust file path management 
  "dotenv",   # Managing the MISTRAL_API_KEY 
  "pdftools", # Local PDF text extraction 
  "curl"      # Handling file uploads to the API 
  "pbapply"
))

```

