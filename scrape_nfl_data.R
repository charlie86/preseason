#devtools::install_github(repo = 'maksimhorowitz/nflscrapR')
library(furrr)
library(nflscrapR)
plan(multiprocess)

calc_season_data <- function(year) {
    reg_games <- scrape_game_ids(year, type = 'reg')
    preseason_games <- scrape_game_ids(year, type = 'pre', weeks = 1:4)
    rbind(reg_games, preseason_games)
}

years <- 2009:2018
tots <- future_map_dfr(years, calc_season_data, .progress = TRUE)
saveRDS(tots, file = 'all_game_data.rds')