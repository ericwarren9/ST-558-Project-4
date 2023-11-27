# Purpose: To get combine our data sources into one readable form we can use for analysis


# Load in required packages -----------------------------------------------

library(tidyverse)
library(RSelenium)
library(rvest)
library(janitor)


# Load in the data on salaries --------------------------------------------

### Note for this section we need to remove 2010 data since it was a lockout year

# Make For Loop to get Salary Data
salaries <- tibble()
tempData <- tibble()
for (i in 2007:2022) {
  
  if (i == 2010) {
    next
  }
  
  url <- paste0("https://overthecap.com/position/quarterback/", i)
  
  tempData <- (read_html(url) %>%
                 html_table())[[1]]
  tempData <- tempData %>% 
    mutate_all(as.character) %>%
    mutate(year = i)
  
  salaries <- bind_rows(salaries, tempData)
}

# Clean salary names 
salaries <- salaries %>%
  janitor::clean_names() %>%
  dplyr::select(- cash_spent) %>%
  mutate(year = as.character(year))

# Get salary cap numbers for league
salary_cap <- (read_html("https://www.spotrac.com/nfl/cba/") %>% 
                 html_table())[[1]] %>%
  janitor::clean_names() %>%
  dplyr::select(year, cap_maximum) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::filter((year >= 2007) & (year != 2010) & (year <= 2022)) %>%
  mutate(cap_maximum = parse_number(cap_maximum),
         year = as.character(year))

# Merge the data together; get salaries in form of cap hit percentage; this will be our data used for salaries
player_salary_percentage <- merge(salaries, salary_cap, by = "year") %>%
  mutate(cap_number = parse_number(cap_number),
         cap_percent = 100 * (cap_number / cap_maximum)) %>%
  dplyr::select(- c(cap_number, cap_maximum, team))


# Get QB player data ------------------------------------------------------

### Note for this section we need to remove 2010 data since it was a lockout year

# Make For Loop to get Salary Data
stats <- tibble()
tempData <- tibble()
for (i in 2007:2022) {
  
  if (i == 2010) {
    next
  }
  
  url <- paste0("https://www.fantasypros.com/nfl/stats/qb.php?year=", i)
  
  tempData <- (read_html(url) %>%
                 html_table())[[1]]
  tempData <- tempData[- c(1:2), ]
  tempData <- tempData %>% 
    mutate_all(as.character) %>%
    mutate(year = i)
  
  stats <- bind_rows(stats, tempData)
}

# Make column names
colnames(stats) <- c("rank", "player", "passing_completions", "passing_attempts", "passing_percentage", "passing_yards", "passing_yards_per_attempt", "passing_tds", "passing_ints", "sacks", "rushing_attempts", "rushing_yards", "rushing_tds", "fumbles", "games", "fantasy_points", "fantasy_points_per_game", "delete_column", "year")

# Change column types for data to make it into reasonable format for analysis
stats <- stats %>%
  dplyr::select(- c(delete_column, rank)) %>%
  mutate(across(! player & ! year & ! passing_yards & ! rushing_yards, as.numeric)) %>%
  mutate_at(c("passing_yards", "rushing_yards"), parse_number) %>%
  mutate(year = as.character(year),
         player = sub("^(\\S*\\s+\\S+).*", "\\1", player),
         player = ifelse(player == "Matthew Stafford", "Matt Stafford",
                         ifelse(player == "Robert Griffin", "Robert Griffin III", 
                                ifelse(player == "Mitch Trubisky", "Mitchell Trubisky", player))))


# Combine data into one tibble to make final product ----------------------

# Merge Data
final_player_info <- merge(player_salary_percentage, stats, by = c("year", "player"))

# Keep players who played at least 10 games (more than half the season) and had at least 100 passing attempts
final_player_info <- final_player_info %>%
  dplyr::filter((games >= 10) & (passing_attempts >= 100)) %>%
  distinct()

# Save the data to use on our analysis later; first csv file, then rds as well for users
write_csv(final_player_info, "player_stats_and_salary.csv")
saveRDS(final_player_info, "player_stats_and_salary.rds")