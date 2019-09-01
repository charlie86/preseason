library(png)
library(grid)
library(ggridges)
library(tidyverse)

tots <- readRDS('all_game_data.rds')

results_df <- tots %>% 
    mutate(winner = case_when(home_score > away_score ~ home_team,
                              away_score > home_score ~ away_team),
           loser = case_when(home_score > away_score ~ away_team,
                             away_score > home_score ~ home_team)) %>% 
    count(season, type, winner, loser) %>% 
    gather(outcome, team, c(winner, loser)) %>% 
    group_by(team, season, type) %>% 
    summarise(wins = sum(n[outcome == 'winner']),
              games = n()) %>% 
    ungroup() %>% 
    filter(!is.na(team))

results_df %>% 
    group_by(team, type) %>% 
    summarise(pct = sum(wins) / sum(games)) %>% 
    spread(type, pct) %>% 
    ggplot(aes(x = pre, y = reg)) +
    geom_point() +
    theme_minimal() +
    scale_x_continuous(labels = function(x) percent(x, accuracy = 1)) +
    scale_y_continuous(labels = function(x) percent(x, accuracy = 1)) +
    labs(x = 'Preseason winning percentage',
         y = 'Regular season winning percentage',
         title = 'The NFL preaseason isn\'t predictive of the regular season',
         subtitle = 'Each dot represents a team\'s cumulative record from 2009-2018') +
    geom_text_repel(aes(label = team))

week_1_results_df <- tots %>% 
    mutate(winner = case_when(home_score > away_score ~ home_team,
                              away_score > home_score ~ away_team),
           loser = case_when(home_score > away_score ~ away_team,
                             away_score > home_score ~ home_team)) %>% 
    filter((type == 'pre' | week == 1)) %>% 
    count(season, type, winner, loser) %>% 
    gather(outcome, team, c(winner, loser)) %>% 
    group_by(team, season, type) %>% 
    summarise(wins = sum(n[outcome == 'winner'])) %>% 
    ungroup() %>% 
    filter(!is.na(team))

week_1_results_df %>% 
    spread(type, wins) %>% 
    group_by(pre) %>% 
    filter(!is.na(reg)) %>% 
    summarise(week_1_win_pct = mean(reg)) %>% 
    ggplot(aes(x = pre, y = week_1_win_pct)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_text(x = .3, y = .53, label = 'Expected win probability') +
    scale_y_continuous(limits = c(0, .7), breaks = seq(0, .7, by = .1),
                       labels = function(x) percent(x, accuracy = 1)) +
    labs(x = 'Preseason wins', 
         y = 'Week 1 winning percentage',
         title = 'Teams who do poorly in the preseason rarely win their first game',
         subtitle = 'Based on preseason and week 1 games from 2009-2018')
    
week_2_results_df <- tots %>% 
    mutate(winner = case_when(home_score > away_score ~ home_team,
                              away_score > home_score ~ away_team),
           loser = case_when(home_score > away_score ~ away_team,
                             away_score > home_score ~ home_team)) %>% 
    filter((type == 'pre' | week == 2)) %>% 
    count(season, type, winner, loser) %>% 
    gather(outcome, team, c(winner, loser)) %>% 
    group_by(team, season, type) %>% 
    summarise(wins = sum(n[outcome == 'winner'])) %>% 
    ungroup() %>% 
    filter(!is.na(team))

week_2_results_df %>% 
    spread(type, wins) %>% 
    group_by(pre) %>% 
    filter(!is.na(reg)) %>% 
    summarise(week_2_win_pct = mean(reg)) %>% 
    ggplot(aes(x = pre, y = week_2_win_pct)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_text(x = .3, y = .56, label = 'Expected win probability') +
    scale_y_continuous(limits = c(0, .7), breaks = seq(0, .7, by = .1), 
                       labels = function(x) percent(x, accuracy = 1)) +
    labs(x = 'Preseason wins', y = 'Week 1 winning percentage',
         title = 'By week 2, the preseason effect goes away',
         subtitle = 'Based on preseason and week 2 games from 2009-2018')

