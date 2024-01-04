### Scraping Kicker for last and next matches of your favorite team

# Load Packages -----------------------------------------------------------
library(rvest)
library(httr)
library(tidyverse)
library(magrittr)


# Create Function ---------------------------------------------------------
my_football_teams <- function(my_team){
  
  # Pass URLs -------------------------------------------------------------
  url <- paste0("https://www.kicker.de/", my_team, "/info")
  
  
  # Scrape html -----------------------------------------------------------
  html_raw <- read_html(GET(url, config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)))
  
  
  # Manipulate data -------------------------------------------------------
  
  # Select the relevant table elements
  table_past <- html_raw %>%
    html_elements("table") %>% 
    extract(1)
  
  table_future <- html_raw %>%
    html_elements("table") %>% 
    extract(2)
  
  # Read alt attribute from image to obtain full team names
  teams_past <- table_past %>%
    html_elements(".kick__v100-gameCell__team__logo img") %>%
    html_attr("alt") %>% 
    str_trim()
  
  # Obtain scores
  scores_past <- html_raw %>%
    html_elements(".kick__v100-scoreBoard__scoreHolder__score") %>%
    html_text(trim = TRUE)
  
  # Create output concatenating the raw vectors
  results_past <- vector()
  i <- 1
  k <- 1
  while (!is.na(teams_past[i])) {
    results_past[k] <- paste0(teams_past[i],
                              " - ",
                              teams_past[i + 1],
                              " ",
                              scores_past[i],
                              ":",
                              scores_past[i + 1])
    i <- i + 2
    k <- k + 1
  }
  
  # Obtain future matches
  matches_future_raw <- table_future %>%
    html_elements(".kick__v100-gameCell__team__name, .kick__v100-scoreBoard__dateHolder") %>%
    html_text(trim = TRUE)
  
  # Replace German with English abbreviation of the weekday
  # Create vectors with weekdays in both languages
  de <- paste0(wday(Sys.Date() + 0:6, label = TRUE, locale = "de_DE.UTF-8"), ".")
  en <- wday(Sys.Date() + 0:6, label = TRUE, locale = "English_United States.1252")
  
  # Get corresponding English weekday from matches_future_raw
  # Use as.character to avoid working with a factor
  english_wday <- en[match(matches_future_raw[matches_future_raw %in% de], de)] %>% 
    as.character()
  
  # Replace German by English weekday
  matches_future_raw[matches_future_raw %in% de] <- english_wday
  
  # Create output concatenating the raw vectors
  matches_future <- vector()
  j <- 1
  l <- 1
  while (!is.na(matches_future_raw[j])) {
    # Test if match is today, because then order of hour and day are inverted
    if(str_detect(matches_future_raw[j + 2], "heute")) {
      # if so, do this
      matches_future[l] <- 
        paste0(matches_future_raw[j],
               " - ",
               matches_future_raw[j + 3],
               ", today at ",
               matches_future_raw[j + 1],
               "h")
      # Prepare next iteration
      j <- j + 4
    } else
      # Test if match hour is already scheduled (but not today)
      if(str_detect(matches_future_raw[j + 2], "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$")) {
        # if so, do this
        matches_future[l] <- 
          paste0(matches_future_raw[j],
                 " - ",
                 matches_future_raw[j + 3],
                 ", ",
                 matches_future_raw[j + 1],
                 " at ",
                 matches_future_raw[j + 2],
                 "h")
        # Prepare next iteration
        j <- j + 4
      } else {
        # If match hour is not scheduled, do this  
        matches_future[l] <- 
          paste0(matches_future_raw[j],
                 " - ",
                 matches_future_raw[j + 2],
                 ", ",
                 matches_future_raw[j + 1],
                 " (not scheduled yet)")
        # Prepare next iteration
        j <- j + 3
      }
    
    # Prepare next iteration
    l <- l + 1
  }
  
  
  # Output ----------------------------------------------------------------
  cat("Last 10 Matches: ",
      results_past,
      "\nNext 3 Matches: ",
      matches_future,
      sep = "\n")
}

# Enjoy the function and make sure the team is written as Kicker writes it in its url

# Examples ----------------------------------------------------------------
# my_football_teams("1-fc-kaiserslautern")
# my_football_teams("fc-red-star")
# my_football_teams("deportivo-la-coruna")