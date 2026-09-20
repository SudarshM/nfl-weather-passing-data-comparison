#Calls necessary libraries needed for the program 
library(dplyr)
library(nflreadr)
library(ggplot2)

#Creates qb_weekly dataset for passing data and loads in the connecting data
qb_weekly <- load_player_stats(           
  seasons   = 1999:2024,                 
  stat_type = "offense") %>% 
  filter(position == "QB")               
qb_weekly
write.csv(qb_weekly, "qb_weekly_1999_2024.csv", row.names = FALSE)
schedule <- load_schedules(1999:2024)

#Connecting the data#

#removes unnecessary seasons
filtered_schedule <- schedule %>%
  filter(!season %in% c("1999", "2022", "2023", "2024"))

#makes both game_ids store as characters
filtered_schedule <- filtered_schedule %>%
  mutate(old_game_id = as.character(old_game_id))
games_weather <- games_weather %>%
  mutate(game_id = as.character(game_id))

#merges both datasets based on game_id
games_weather_schedule <- left_join(games_weather, filtered_schedule, by = c("game_id" = "old_game_id"))

#removes duplicates of game_ids
games_weather_schedule <- games_weather_schedule %>%
  distinct(game_id, .keep_all = TRUE)

#remove all unnecessary years from qb_weekly
filtered_qb_weekly <- qb_weekly %>%
  filter(!season %in% c("1999", "2022", "2023", "2024"))

#connect datasets based on the team playing
home_qbs <- right_join(
  filtered_qb_weekly,
  games_weather_schedule,
  by = c("season", "week", "recent_team" = "home_team")
) %>% mutate(was_home = TRUE)

away_qbs <- right_join(
  filtered_qb_weekly,
  games_weather_schedule,
  by = c("season", "week", "recent_team" = "away_team")
) %>% mutate(was_home = FALSE)

qb_weather_combined <- bind_rows(home_qbs, away_qbs)

qb_weather_combined <- qb_weather_combined %>%
  mutate(pass_completion = completions / attempts * 100) %>%
  mutate(pass_air_distance = passing_air_yards/attempts) %>%
  mutate(sack_rate = sacks/attempts + sacks)

#remove unnecessary columns
master_df <- qb_weather_combined %>%
  select(-player_id, -player_name, -position, -position_group,
         -headshot_url, -game_type, -result, -total, -overtime,
         -nfl_detail_id, -gsis, -pfr, -pff, -espn, -ftn, -away_rest,
         -home_rest, -home_moneyline, -away_moneyline, -spread_line,
         -away_spread_odds, -home_spread_odds, -total_line, -under_odds,
         -over_odds, -away_coach, -home_coach, -referee, -was_home, -home_team,
         -away_qb_name, -home_qb_name, -away_qb_id, -home_qb_id, -fantasy_points,
         -fantasy_points_ppr, -Source, -DistanceToStation, -temp,
         -wind, -away_score, -home_score, -div_game, -location, -weekday, -gametime,
         -away_team, -surface, -stadium_id, -game_id.y, -sack_yards,
         -sack_fumbles, -passing_yards_after_catch,
         -passing_first_downs, -passing_2pt_conversions, -pacr, -dakota,
         -rushing_fumbles, -rushing_first_downs, -rushing_epa,
         -rushing_2pt_conversions, -receptions, -receiving_2pt_conversions, -receiving_epa,
         -receiving_air_yards, -targets, -receiving_yards, -receiving_tds,
         -receiving_yards_after_catch, -receiving_first_downs, -receiving_fumbles,
         -receiving_fumbles_lost, -racr, -target_share, -air_yards_share, -wopr,
         -gameday, -special_teams_tds)

#Make graphs

#Temperature Vs. Completion Percentage
master_temperature_completion <- master_df %>%
  filter(roof == 'outdoors', attempts > 5)

temp_completion <- ggplot(master_temperature_completion, aes(x = Temperature, y = pass_completion)) +
  geom_point(color = "#1b9e77", alpha = 0.7) +
  stat_smooth(method = "lm", se = TRUE, color = "#d95f02", size = 1.2) +
  theme_classic(base_size = 16) +
  labs(x = 'Temperature (˚F)', y = 'Completion Percentage') +
  expand_limits(y = 0)
temp_completion
cor.test(master_temperature_completion$Temperature, master_temperature_completion$pass_completion, method = "pearson")


#Temperature Vs Air Distance per Completion
master_temperature_airdistance <- master_df %>%
  filter(roof == 'outdoors', pass_air_distance > 0, attempts > 10)

temp_airdistance <- ggplot(master_temperature_airdistance, aes(x = Temperature, y = pass_air_distance)) +
  geom_point(color = "#7570b3", alpha = 0.7) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#e7298a", size = 1.2) + 
  theme_classic() +
  labs(title = 'Air Yards per Completion at Different Temperatures', x = 'Temperature', y = 'Air Yards per Attempt') +
  expand_limits(y = 0)
temp_airdistance

#WindSpeed Vs Air Distance per Completion
master_wind_airdistance <- master_df %>%
  filter(roof == 'outdoors', pass_air_distance > 0, attempts > 10)

windspeed_airdistance <- ggplot(master_wind_airdistance, aes(x = WindSpeed, y = pass_air_distance)) +
  geom_point(color = "#66a61e", alpha = 0.7) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#e6ab02", size = 1.2) +
  theme_classic() +
  labs(title = 'Air Yards per Completion at Different Wind Speeds', x = 'Wind Speed', y = 'Air Yards per Attempt') +
  expand_limits(y = 0)
windspeed_airdistance

#WindSpeed Vs Attempts
master_wind_attempts <- master_df %>%
  filter(roof == 'outdoors', pass_air_distance > 0, attempts > 3)

windspeed_attempts <- ggplot(master_wind_attempts, aes(x = WindSpeed, y = attempts)) +
  geom_point(color = "#a6761d", alpha = 0.7) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#666666", size = 1.2) +
  theme_classic() +
  labs(title = 'Passing Attempts at Different Wind Speeds', x = 'Wind Speed', y = 'Passing Attempts') +
  expand_limits(y = 0)
windspeed_attempts


#Roof Completion Percentage
roof_summary <- master_df %>%
  mutate(roof_group = case_when(roof %in% c("closed", "dome") ~ "Indoors", roof %in% c("open", "outdoors") ~ "Outdoors", TRUE ~ as.character(roof))) %>%
  group_by(roof_group) %>%
  summarize(avg_pass_completion = mean(pass_completion, na.rm = TRUE))

roof_diff <- roof_summary %>%
  mutate(max_avg = max(avg_pass_completion), diff_from_max = max_avg - avg_pass_completion) %>%
  arrange(diff_from_max)

y_min <- min(roof_diff$avg_pass_completion) - 0.5
y_max <- max(roof_diff$avg_pass_completion) + 0.5

roof_completion <- ggplot(roof_diff, aes(x = reorder(roof_group, diff_from_max), y = avg_pass_completion)) +
  geom_col(fill = "steelblue") +
  geom_text(data = subset(roof_diff, diff_from_max != 0), aes(label = round(diff_from_max, 2)), vjust = 1.5, color = "orange", size = 6, fontface = "bold") +
  theme_classic() +
  labs(title = 'Average Pass Completion by Roof Group', subtitle = "The Number on the Bar = Difference in Average Pass Completion from Indoors", x = 'Roof Group', y = 'Average Pass Completion') +
  coord_cartesian(ylim = c(y_min, y_max))
roof_completion

#Average Pass Completion Vs WindSpeed
master_outdoor_wind <- master_df %>%
  filter(roof == "outdoors") %>%
  group_by(WindSpeed) %>%
  summarize(avg_completion = mean(pass_completion, na.rm = TRUE), .groups = "drop")

outdoor_wind <- ggplot(master_outdoor_wind, aes(x = WindSpeed, y = avg_completion)) +
  geom_point(color = "purple") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1.1) +
  theme_classic(base_size = 16) +
  labs(x = "Wind Speed (mph)", y = "Average Completion Percentage")
outdoor_wind
cor.test(master_outdoor_wind$WindSpeed, master_outdoor_wind$avg_completion, method = "pearson")

#Condition Vs Completion Percentage
master_condition <- master_df %>%
  filter(!is.na(EstimatedCondition)) %>%
  mutate(EstimatedCondition = factor(
    EstimatedCondition,
    levels = c("Clear", "Light Rain", "Moderate Rain", "Heavy Rain", "Light Snow", "Moderate Snow")))

condition_completion_percentage <- ggplot(master_condition, aes(x = EstimatedCondition, y = pass_completion)) +
  geom_boxplot(fill = "skyblue") +
  theme_classic() +
  labs(title = "Pass Completion by Precipitation Type", x = "Precipitation", y = "Completion Percentage")
condition_completion_percentage

#Temperature Vs Sack Rate
master_temp_sackrate <- master_df %>%
  filter(!is.na(Temperature), !is.na(sack_rate), sack_rate < 10)

temp_sackrate <- ggplot(master_temp_sackrate, aes(x = Temperature, y = sack_rate)) +
  geom_jitter(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red", linewidth = 1.1) +
  theme_classic(base_size = 16) +
  labs(x = "Temperature (˚F)", y = "Sack Rate")
temp_sackrate
cor.test(master_temp_sackrate$Temperature, master_temp_sackrate$sack_rate, method = "pearson")


#Wind Speed Vs Air Distance
master_outdoor_wind_air <- master_df %>%
  filter(roof == "outdoors", passing_air_yards > 0) %>%
  group_by(WindSpeed) %>%
  summarize(avg_air_distance = mean(passing_air_yards, na.rm = TRUE), .groups = "drop")

outdoor_wind_air <- ggplot(master_outdoor_wind_air, aes(x = WindSpeed, y = avg_air_distance)) +
  geom_point(color = "#b4b930") +
  stat_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +
  theme(text = element_text(family = "Times")) +
  theme_classic(base_size = 16) +
  labs(x = "Wind Speed (mph)", y = "Average Air Distance (yds)")
outdoor_wind_air
cor.test(master_outdoor_wind_air$avg_air_distance, master_outdoor_wind_air$WindSpeed, method = "pearson")



ggplot(master_df, aes(x = WindSpeed, y = passing_tds)) +
  geom_point() +
  stat_smooth(method = "loess", se = TRUE, color = "red", size = 1.2)
