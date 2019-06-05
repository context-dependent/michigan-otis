library(tidyverse)

test_records <- read_rds("data/raw/raw-records-to-100999.rds")

test_tables <- test_records %>% clean_offender_records()


# ACTIONS ON FULL RECORDSET -----------------------------------------------

clean_offender_records <- function(raw_offender_records) {
  
  res <- raw_offender_records %>% 
    table_offender_records() %>% 
    map(parse_mdoc_dates)
  
  res$demos <- clean_demos(res$demos)
  
  res
  
  
}

table_offender_records <- function(raw_offender_records) {
  
  res <- raw_offender_records %>% 
    map(identify_all_tables) %>% 
    transpose() %>% 
    map(bind_rows) 
  
  res
  
}

identify_all_tables <- function(offender_recordset) {
  
  offender_recordset %>% 
    map(
      ~ .x %>% 
        mutate(offender_number = offender_recordset$demos$offender_number) %>% 
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



