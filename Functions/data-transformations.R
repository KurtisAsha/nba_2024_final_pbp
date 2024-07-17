
# Gets dates from scorebox ------------------------------------------------

get_scorebox_date <- function(pbp_html){

 pbp_html %>% 
 html_nodes("div.scorebox_meta") %>% 
 html_children() %>% 
 pluck(1) %>% 
 html_text() %>% 
 strptime(format = "%I:%M %p, %B %d, %Y", tz = "UTC")

}
 
# Retrieves the rows for each quarter -------------------------------------------------------------
get_quarter_row_indicies <- function(pbp_raw){

 # Get row references for quarters
 quarter_1_row_location = 3:(which(pbp_raw$X1 == "2nd Q") -2)
 quarter_2_row_location = (max(quarter_1_row_location) +5):(which(pbp_raw$X1 == "3rd Q") -2)
 quarter_3_row_location = (max(quarter_2_row_location) +5):(which(pbp_raw$X1 == "4th Q") -2)
 quarter_4_row_location = (max(quarter_3_row_location) +5):(nrow(pbp_raw) -1)
 
 quarters_list <- list(
  quarter_1_row_location, 
  quarter_2_row_location, 
  quarter_3_row_location, 
  quarter_4_row_location
 )
 
 # Get min and max rows references for each quarter
 row_references <- data.frame(
  min = map(quarters_list, ~min(.x)) %>% 
   unlist(),
  
  max = map(quarters_list, ~max(.x)) %>% 
   unlist()
 )
 
 return(row_references)
 
}

# Filters play by play data and adds relevant quarter ---------------------------------------------------
add_quarters_col <- function(pbp_raw) {
 
 quarter_rows <- get_quarter_row_indicies(pbp_raw)
 
 q1_pbp <- pbp_raw[quarter_rows[1, "min"]:quarter_rows[1, "max"], ] %>%
  mutate(quarter = "q1")
 
 q2_pbp <- pbp_raw[quarter_rows[2, "min"]:quarter_rows[2, "max"], ] %>%
  mutate(quarter = "q2")
 
 q3_pbp <- pbp_raw[quarter_rows[3, "min"]:quarter_rows[3, "max"], ] %>%
  mutate(quarter = "q3")
 
 q4_pbp <- pbp_raw[quarter_rows[4, "min"]:quarter_rows[4, "max"], ] %>%
  mutate(quarter = "q4")
 
 pbp_with_quarters <- bind_rows(
  q1_pbp,
  q2_pbp,
  q3_pbp,
  q4_pbp
 )
 
 return(pbp_with_quarters)
 
}


# Calculates cumulative timestamp -----------------------------------------
# Requires scorebox dates

calc_timestamp <- function(pbp_with_quarters, scorebox_dates){

 scorebox_date <- as.Date(scorebox_dates)
 
 pbp_with_timestamp <- pbp_with_quarters %>%
 rename(timestamp = X1) %>%
 mutate(
  timestamp = lubridate::ymd_hms(
   paste0(scorebox_date, " 0:", timestamp)
  ),
  # Calculate time difference using timestamp
  timediff = if_else(
   is.na(lag(timestamp, 1)),
   as.numeric(timestamp - timestamp),
   as.numeric(lag(timestamp, 1) - timestamp)),
  # Calculate cumulative time difference using timediff
  timediff = seconds(cumsum(timediff)),
  timestamp = lubridate::ymd_hms(
   scorebox_dates + timediff
  )
 )

return(pbp_with_timestamp)

}




