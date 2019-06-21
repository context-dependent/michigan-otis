pacman::p_load(
  "tidyverse",
  "rvest",
  "httr"
)

get_offender_records <- function(mdoc_numbers, file_stem, batch_size = 1E4) {
  
  cat("\n")
  
  counter <- 0
  
  request_timer <- timer()
  
  request_timer$start()
  
  moja <- function(mdoc_number) {
    
    req <- httr::GET(
      url = "http://mdocweb.state.mi.us/OTIS2/otis2profile.aspx",
      query = list(
        mdocNumber = mdoc_number
      )
    )
    
    offender_page <- httr::content(req)
    
    found <- record_is_found(offender_page)
    
    if(!found) {
      return(NULL)
    }
    
    counter <<- counter + 1
    
    cat_progress_bar(counter)
    
    res <- list()
    
    res$demos      <- offender_page %>% parse_offender_demos()
    res$status     <- offender_page %>% parse_offender_status()
    res$sentences  <- offender_page %>% parse_offender_sentences()
    res$conditions <- offender_page %>% parse_supervision_conditions()
    
    res
    
  }
  
  res <- list(length(mdoc_numbers))
  
  for(i in seq_along(mdoc_numbers)) {
    
    res[[i]] <- moja(mdoc_numbers[i])
    if(i == batch_size | i == length(mdoc_numbers)) {
      res <- compact(res)
      
      lap <- request_timer$lap()
      cat(" ", round(lap))
      cat("\r", length(res))
      file_path <- file_stem %>% str_c("-to-", mdoc_numbers[i], ".rds")
      write_rds(res, file_path)
      res <- NULL
      remaining_mdoc_numbers <- mdoc_numbers[-1:-i]
      if(i == length(mdoc_numbers)) {
        cat("\nall done")
        return(NULL)
      }
      get_offender_records(remaining_mdoc_numbers, file_stem = file_stem, batch_size = batch_size)
    }
    
  }
  
  res <- compact(res)
  
  res
  
}



# get offender demos from top of page

parse_offender_demos <- function(offender_page) {
    
    demo_nodes <- offender_page %>% 
      html_nodes(xpath = "//div[@id='pnlResults']//span[contains(@id, 'val')]")
    
    res <- clean_nodes_to_tibble(demo_nodes)
    
    res
    
}


parse_offender_status <- function(offender_page) {
  
  status_nodes <- offender_page %>% 
    html_nodes(xpath = "//div[@id='pnlStatusAlias']//span[contains(@id, 'val')]")
  
  res <- clean_nodes_to_tibble(status_nodes)
  
  res
  
}

parse_offender_sentences <- function(offender_page) {
  
  res <- c("PI", "PA", "RI", "RA") %>% 
    map(
      ~ sentence_div_to_tibble(offender_page, .x)
    ) %>% 
    bind_rows()
  
}

parse_supervision_conditions <- function(offender_page) {
  
  res <- offender_page %>% 
    html_node(xpath = "//span[@id='valSupervisionConditions']") %>% 
    html_text()
  
  res <- res %>% textConnection() %>% readLines()
  
  res <- res[res != ""] %>% 
    str_split_fixed(" - ", 2)
  
  colnames(res) <- c("supervision_condition_code", "supervision_condition_description")
  
  res <- as_tibble(res)
  
  res
  
}

sentence_div_to_tibble <- function(offender_page, sentence_type) {
  
  
  var_nodes <- offender_page %>% 
    html_nodes(xpath = glue::glue("//div[@id='pnl{sentence_type}Sentences']//div[contains(@class, 'span2')]")) 
  
  if(is_empty(var_nodes)) return(NULL)
  
  vars <- var_nodes %>% 
    html_text() %>% 
    snakecase::to_snake_case() %>% 
    unique()
  
  
  
  blanks <- which(vars == "")
  
  vals <- offender_page %>% 
    html_nodes(xpath = glue::glue("//div[@id='pnl{sentence_type}Sentences']//div[contains(@class, 'span3')]")) %>% 
    html_text() %>% 
    trimws() %>% 
    matrix(ncol = length(vars), byrow = TRUE)
  
  
  res <- as_tibble(vals)[, -blanks]
  
  colnames(res) <- vars[-blanks]
  
  
  res <- res %>% 
    mutate(
      sentence_type = sentence_type
    )
  
  res
  
}


clean_nodes_to_tibble <- function(target_nodes) {
  
  vars <- target_nodes %>% 
    html_attr("id") %>% 
    str_remove("val") %>% 
    snakecase::to_snake_case()
  
  vals <- target_nodes %>% 
    html_text()
  
  names(vals) <- vars
  
  res <- tibble(!!!vals)
  
  res
  
  
}


record_is_found <- function(offender_page) {
  
  not_found_node <- offender_page %>% 
    html_nodes("#lblRecordNotFound")
  
  res <- is_empty(not_found_node)
  
  res
  
}


cat_progress_bar <- function(counter, chunk_size = 10) {
  
  n_bars <- counter %/% chunk_size
  n_dots <- counter %% chunk_size
  
  res <- str_c(
    "\r", 
    strrep("|", n_bars), 
    strrep(".", n_dots), 
    strrep(" ", chunk_size - 1), 
    collapse = ""
  )
  
  cat(res)
  
}

timer <- function() {
  
  start_time_global <- NULL
  start_time_lap <- NULL
  laps <- c()
    
  res <- list(
    start = function() {
      if(is.null(start_time_global)) start_time_global <<- Sys.time()
    }, 
    lap = function() {
      time_now <- Sys.time()
      if(is.null(start_time_lap)) start_time_lap <<- start_time_global
      lap_time <- time_now - start_time_global
      
      laps <<- c(laps, lap_time)
      
      lap_time
    }, 
    total = function() {
      time_now <- Sys.time()
      total_time_elapsed <- time_now - start_time_global
      total_time_elapsed
    },
    reset = function() {
      start_time_global <<- NULL
      start_time_laps <<- NULL
      laps <<- c()
    },
    get_laps = function() {
      laps
    }
  )
  
}



