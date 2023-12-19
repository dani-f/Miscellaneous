######################
#
# Birthday progress bar
# A progress bar to show how much is left till your next birthday
# Adaptable to show progress for any yearly deadline you want to track
#
# Initial idea and code based on publications in this great repository:
# https://github.com/tomaztk/Useless_R_functions
######################

library(lubridate)
library(dplyr)
library(scales)

# Choose name and birthday
name <- "Taylor Swift"
birth_date <- as.Date("1989-12-13")

today <- Sys.Date()
current_year <- format(today, format = "%Y")
this_year_birthday <- as.Date(paste0(current_year, "-", format(birth_date, format = "%m-%d")))
next_birthday <- if_else(this_year_birthday >= today, this_year_birthday,
                         this_year_birthday + years(1)) # use dplyr if_else() in order to maintain the date-class
age <- trunc((birth_date %--% today) / years(1)) + 1

# Percentage of life year spent
difference_pct <- (365 - as.integer(next_birthday - today)) / 365
# Maximum width of progress bar
width_bar <- 50
len_progress <- 40
# Cursor shape
cursor <- c("\\", "|", "/", "--")

for (len_step in 1:len_progress) {
  step <- round((len_step / len_progress * (width_bar - 5)) * difference_pct)
  # Config the cursor to iterate from position 1 to 4 and back to 1 again
  char_spinning_cursor <- (len_step %% 4) + 1 
  progress_pct <- round(len_step / len_progress * difference_pct * 100, digits =  2)
  days_left <- if (len_step == len_progress) {
    next_birthday - today} else {
      round(365 - progress_pct / 100 * 365, digits = 0)}
  
  text <- sprintf('%s |%s%s % 3s%% %s',
                  cursor[char_spinning_cursor],
                  strrep('=', step),
                  strrep('¦', width_bar - step - 5),
                  progress_pct,
                  paste0("(only ", days_left, " days left)"))
  
  cat("Progress until ", name, "'s ", ordinal(age), " birthday...\n", sep = "")
  cat(text)  
  Sys.sleep(0.2)
  # Clean the console after every iteration, at the end close with a new line
  cat(
    if (len_step == len_progress) {
      '\n'} else {
        # cat("\014") is the code to send CTRL+L to the console, and therefore will clear the screen.
        '\014'
      })
}
