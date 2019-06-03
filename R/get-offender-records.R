get_offender_records <- function(mdoc_numbers) {
  
  cat("\n")
  
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
      cat("o")
      return(NULL)
    }
    
    cat("x")
    
    res <- list()
    
    res$demos <- offender_page %>% parse_offender_demos()
    res$status <- offender_page %>% parse_offender_status()
    res$sentences <- offender_page %>% parse_offender_sentences()
    res$conditions <- offender_page %>% parse_supervision_conditions()
    
    res
    
  }
  
  res <- list(length(mdoc_numbers))
  
  for(i in seq_along(mdoc_numbers)) {
    
    res[[i]] <- moja(mdoc_numbers[i])
    
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
  
  res <- offender_page %>% html_nodes(xpath = "div[@id='valSupervisionConditions']")
  
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


test_offender_nums <- c(
  486641, 
  238407,
  952809
)


test_fail_nums <- c(
  1, 
  2, 
  3
)
