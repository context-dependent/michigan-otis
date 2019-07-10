clean_offense_codes <- function(path) {
  
  # Read raw text from crime code pdf
  raw_text <- pdftools::pdf_text(path)
  
  # Create a temporary file to write raw text to
  tmp <- tempfile(fileext = ".txt")
  
  # Write raw text preserving line breaks and tabs
  cat(raw_text, file = tmp)
  
  # Read each line as an element in a vector
  text_lines <- read_lines(tmp)
  
  # Remove empty lines
  text_lines <- text_lines[text_lines != ""]
  
  # Identify the indices where tables start
  table_starts <- which(str_detect(text_lines, "^CRIMES THAT RESULT IN THE DENIAL"))
  
  # Find table titles (always one line before table starts)
  table_titles <- text_lines[table_starts-1] %>% trimws() %>% str_to_title()
  
  # Create a table that contains all table titles and their corresponding ranges
  table_ranges <- tibble(
      start = table_starts, 
      end = lead(table_starts) %>% replace_na(length(text_lines))
    ) %>% 
    mutate(
      title = table_titles, 
      range = map2(start, end-2, seq, by = 1)
    )
  
  # Create a table that reflects the visual column structure of the pdf
  table_contents <- table_ranges %>% 
    mutate(
      # Pull the correct range of lines for each table
      raw_content = range %>% map(~text_lines[.x] %>% str_remove("^\\s+(?=\\d+\\.|Charge)")),
      # Make a list column that contains a structured table for each title
      data = raw_content %>% map(clean_offense_code_table)
    ) %>% 
    # Select the table title and structured data list column, and then unnest
    select(category = title, data) %>% unnest()
  
  # Do remaining cleaning tasks for the description
  res <- table_contents %>% 
    mutate(
      description = description %>% 
        str_replace_all("�", "-") %>% 
        str_replace_all("\\s+",  " ") %>% 
        trimws()
    )
  
  res
}


raw_content <- table_contents[3, ]$raw_content[[1]]


extract_offense_information <- function(raw_text, charge_code_width, description_width) {
  
  tibble(
    charge_code = raw_text %>% str_sub(end = charge_code_width),
    description = raw_text %>% str_sub(start = charge_code_width + 1, end = charge_code_width + description_width),
    sentence    = raw_text %>% str_sub(start = charge_code_width + description_width + 1)
  ) %>% 
    mutate_all(trimws)
  
}

clean_offense_code_table <- function(raw_content) {
  
  col_widths <- tibble(
    raw_text            = raw_content,
    indicator_row       = str_detect(raw_text, "^\\d\\d+\\."),
    charge_code_width   = nchar(str_extract(raw_text, "^[^\\s]+")), 
    description_width   = nchar(str_extract(raw_text, "^.+(?=MANDATORY|MISDEMEANOR|FELONY)")) - charge_code_width, 
    sentence_width      = nchar(str_extract(raw_text, "MANDATORY|MISDEMEANOR|FELONY.+"))
  ) %>% 
    mutate_at(
      vars(matches("width")), 
      list(~ ifelse(indicator_row, ., NA_real_))
    ) %>% 
    fill(matches("width")) %>% 
    filter(
      !is.na(sentence_width), 
      !str_detect(raw_text, "Page|^[A-Z]{3}")
    ) %>% 
    mutate(charge_code_id = cumsum(indicator_row))
  
  nested_data <- col_widths %>% 
    mutate(
      data = pmap(
        select(col_widths, raw_text, charge_code_width, description_width), 
        extract_offense_information
      )
    ) 
  
  res <- nested_data %>% 
    select(charge_code_id, data) %>% 
    unnest() %>% 
    group_by(charge_code_id) %>% 
    summarize_all(list(~paste0(., collapse = " "))) %>% 
    mutate(charge_code = str_remove_all(charge_code, "[^\\dA-Z\\.]"))

}
