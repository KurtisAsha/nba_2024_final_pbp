
# Introduction ------------------------------------------------------------

# My intent was to explore process mining techniques on the NBA final, to that 
# end I webscraped the play by play data, cleaned and combined until realising.
# The data is not in the write format to use process mining...gutted.
# In an effort to make sure my time wasn't a total waste. I've uploaded the data
# on Kaggle and posted this here script. Hope someone finds it remotely useful. 
# For the curious, to see what I was trying to do, see `link`

# Acknowledgements
# data source - https://www.basketball-reference.com/
# Package Maintainer(s):
## tidyverse and rvest - Hadley Wickham

# Setup -------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(readODS)

source("./Functions/data-transformations.R")

# Read pbp html -----------------------------------------------------------

pbp_html <- list(
  pbp_2024_06_06 = read_html("https://www.basketball-reference.com/boxscores/pbp/202406060BOS.html") 
 ,pbp_2024_06_09 = read_html("https://www.basketball-reference.com/boxscores/pbp/202406090BOS.html")
 ,pbp_2024_06_12 = read_html("https://www.basketball-reference.com/boxscores/pbp/202406120DAL.html")
 ,pbp_2024_06_14 = read_html("https://www.basketball-reference.com/boxscores/pbp/202406140DAL.html")
 ,pbp_2024_06_17 = read_html("https://www.basketball-reference.com/boxscores/pbp/202406170BOS.html")
)

# Get scorebox date
scorebox_dates <- map(pbp_html, ~get_scorebox_date(.x))

# Read roster html --------------------------------------------------------

roster_html <- list(
 bos_roster = read_html("https://www.basketball-reference.com/teams/BOS/2024.html"), 
 dal_roster = read_html("https://www.basketball-reference.com/teams/DAL/2024.html")
) 

# Transform roster data ---------------------------------------------------

roster_raw <- map(
 # Get roster table and convert to data frame
 roster_html, ~html_element(., "table") %>% 
                    html_table()) 

# Add team names to roster data
roster_raw$bos_roster["team_name"] <- "Boston Celtics"
roster_raw$dal_roster["team_name"] <- "Dallas Mavericks"

# Cleaning columns
roster_data <- roster_raw %>% 
 bind_rows() %>% 
 janitor::clean_names() %>% 
 rename(number = no, 
        birth_country_code = birth, 
        experience = exp)

# Transform pbp data ------------------------------------------------------

 pbp_raw <- map(
 # Get play by play table and convert to data frame
 pbp_html, ~html_element(., "table") %>%
                         html_table(header = FALSE))
 
 pbp_with_quarters <- map(
 # Select rows with times only and add quarter column
  pbp_raw, ~add_quarters_col(.x)) %>% 
  map(~tibble::rowid_to_column(.x, var = "order"))

 pbp_with_timestamp <- pmap(
 # Calculate cumlulative timestamp
 list(pbp_with_quarters, scorebox_dates), ~calc_timestamp(..1, ..2))

 pbp_finals <- pbp_with_timestamp %>% map(
   ~rename(.x, 
          DAL_play = X2, 
          DAL_score_tally = X3, 
          score_board = X4,
          BOS_score_tally = X5, 
          BOS_play = X6, 
          )
  ) %>% 
  bind_rows()


# Write data --------------------------------------------------------------

 pbp_finals %>% str()
 
 write_ods(pbp_finals, 
           path = "./Outputs/nba_2024_pbp_finals_data.ods", 
           sheet = "NBA 2024 Play-by-play Finals Data")

 
 
 