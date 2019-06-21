

# INTRODUCTION ------------------------------------------------------------

# In this script, we'll try to get offender records for each 6 digit number. 
# As far as I can tell, all michigan offender tracking system numbers are 
# 6 digits long, from a review of the results for 'davis', a very common 
# surname. 

possible_mdoc_numbers <- 1E5:1E6

# This appears to be working, but has taken > 24 hours
# Definitely not going to be a live-feed lol 

test_pull <- possible_mdoc_numbers %>% get_offender_records()

test_pull %>% write_rds("data/raw/full-offender-list.rds")


# Round 2: scrape broke down around 241999. I'm going to restart from there

remaining_mdoc_numbers <- 9.77E5:1E6

remaining_mdoc_numbers %>% 
  get_offender_records(batch_size = 1000,file_stem = "data/raw/raw-records")


