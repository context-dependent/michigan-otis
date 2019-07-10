library(tidyverse)

test_records <- read_rds("data/raw/raw-records-to-100999.rds")

test_tables <- test_records %>% clean_offender_records()


# ACTIONS ON FULL RECORDSET -----------------------------------------------

clean_offender_records <- function(raw_offender_records) {
  
  res <- raw_offender_records %>% 
    table_offender_records() %>% 
    map(parse_mdoc_dates)
  
  res$demos <- clean_demos(res$demos)
  res$marks_scars_tattoos <- clean_mst(res$status)
  
  res
  
  
}

table_offender_records <- function(raw_offender_records) {
  
  res <- raw_offender_records %>% 
    map(identify_all_tables) %>% 
    transpose() %>% 
    map(bind_rows) 
  
  res
  
}

identify_all_tables <- function(raw_offender_records) {
  
  raw_offender_records %>% 
    
    map(
      ~ .x %>% 
        mutate(offender_number = raw_offender_records$demos$offender_number) %>% 
        select(offender_number, everything()) %>% 
        rename(offender_id = offender_number) 
    )
  
}

parse_mdoc_dates <- function(dat) {
  
  dat %>% 
    mutate_at(vars(matches("date")), list(~as.Date(., format = "%m/%d/%Y")))
  
}


# CLEAN DEMOS -------------------------------------------------------------

clean_demos <- function(demos_table) {
  
  res <- demos_table %>% 
    mutate(
      height = parse_offender_height(height),
      weight = parse_offender_weight(weight),
      bmi = weight / ((height / 100) ^ 2),
      bmi_category = categorize_bmi(bmi),
      name = trimws(full_name) %>% str_to_title()
    ) %>% 
    select(
      offender_id, 
      sid, 
      name,
      birth_date,
      race, 
      gender, 
      hair_color, 
      eye_color, 
      height, 
      weight, 
      bmi,
      bmi_category
    )
  
  res
  
}

parse_offender_height <- function(height_in_inches) {
  
  res <- height_in_inches %>% 
    str_remove_all("[^\\d\\s]") %>% 
    str_split(" ") %>% 
    map(as.numeric) %>% 
    map_dbl(~ cm(.x[1] * 12) + cm(.x[2]))
  
  res
}

parse_offender_weight <- function(weight_in_pounds) {
  
  res <- weight_in_pounds %>% 
    str_remove_all("[^\\d]") %>% 
    as.numeric() 
  
  res <- res * 0.45359237
  
  res
  
}


# This is kindof gross, but I expect that the judiciary punishes more 
# harshly those who seem undeserving of mercy. 

# In modern society, poor fat people are some of the most maligned
# and ridiculed. Fatness is read by some as a signal of weak will,
# and I think it could be connected to how deserving offenders are 
# perceived to be of leniency. 

categorize_bmi <- function(bmi) {
  
  res <- cut(
    bmi, 
    breaks = c(0, 18.5, 25, 30, 35, 40, Inf), 
    labels = c(
      "Under weight", 
      "Normal weight", 
      "Over weight", 
      "Obese I", 
      "Obese II", 
      "Obese III"
    )) %>% 
    fct_explicit_na("Missing")

  res  
  
}



# CLEAN MARKS, SCARS, TATTOOS ---------------------------------------------

clean_mst <- function(status_table) {
  
  res <- status_table %>% 
    select(offender_id, mst) %>% 
    mutate(mst = trimws(mst)) %>% 
    separate_rows(mst, sep = "[\\r\\n]+") %>% 
    filter(mst != "None") %>% 
    separate(mst, into = c("type", "placement", "description"), sep = "\\s*-\\s*")
  
  
  res
  
}



# CLEAN ALIASES -----------------------------------------------------------

clean_aliases <- function(status_table) {
  
  res <- status_table %>% 
    select(offender_id, alias) %>% 
    mutate(
      alias = trimws(alias)
    ) %>% 
    separate_rows(alias, sep = "[\\r\\n]+") %>% 
    mutate(alias = str_to_title(alias) %>% str_replace_all("\\s+", " ")) %>% 
    filter(alias != "None")
  
  res
  
}


# CLEAN CURRENT STATUS ----------------------------------------------------



# CLEAN SENTENCES ---------------------------------------------------------

clean_sentences <- function(sentences_table) {
  
  
  res <- sentences_table %>% 
    mutate(
      sentence_id      = str_c("S-", 10E4 + 1:n()),
      sentence_custody = ifelse(str_detect(sentence_type, "^R"), "Probation", "Prison"), 
      sentence_status  = ifelse(str_detect(sentence_type, "A$"), "Active", "Inactive"),
      mcl_codes        = str_extract_all(mcl, "(?<=(\\s|^))[\\d\\.a-zA-Z]+"),
      minimum_sentence = parse_sentence_length(minimum_sentence), 
      maximum_sentence = parse_sentence_length(maximum_sentence)
    )
  
  res <- res %>% 
    select(
      sentence_id, 
      offender_id, 
      offense, 
      date_of_offense, 
      county,
      mcl_codes, 
      minimum_sentence, 
      maximum_sentence,
      date_of_sentence, 
      date_of_discharge = discharge_date,
      discharge_reason, 
      conviction_type, 
      sentence_custody, 
      sentence_status
    )
  
  res
  
}


parse_sentence_length <- function(sentence_length) {
  
  res <- case_when(
    !str_detect(sentence_length, "year|month|day") ~ "0 years", 
    sentence_length == "LIFE" ~ "100 years",
    TRUE ~ sentence_length
  )
  
  res <- lubridate::as.period(res)
  
  res
  
}

