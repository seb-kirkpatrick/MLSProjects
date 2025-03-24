library(tidyverse)

mins_file <- "atlutd_datascientist_project2_player_mins_appearances.csv"
mins <- read_csv(mins_file)

sched_file <- "atlutd_datascientist_project2_schedule.csv"
sched <- read_csv(sched_file)

event_rds <- "atlutd_datascientist_project2_eventdata.rds"
event_data <- readRDS(event_rds)

# Going to start by narrowing down the player pool to attacking players
winger_ids <- c(17,21)
am_ids <- c(18:20)
fw_ids <- c(22:24)

positions <- event_data |>
  select(player.id, match_id, position.id) |>
  filter(!is.na(player.id)) |>
  distinct() |>
  group_by(player.id, position.id) |>
  summarize(
    count = n(), 
    .groups = "drop"
    ) |>
  pivot_wider(names_from = position.id, values_from = count, values_fill = 0, names_sort = T)

attacking_players <- positions |>
  filter(if_any(17:25, ~ . > 0)) |>
  mutate(attacking_roles = rowSums(across(18:25)),
         roles = rowSums(across(2:25))) |>
  filter(attacking_roles / roles > 0.5) # As long as half of their roles are attacking, I will include them

attacking_player_ids <- attacking_players |>
  pull(player.id) # 197 players to choose from

xGA <- event_data |>
  filter(type.name=="Shot") |>
  select(shot.key_pass_id, xGA = shot.statsbomb_xg)

stats <- event_data |>
  filter(
    player.id %in% attacking_player_ids
  ) |>
  left_join(xGA, by = c("id" = "shot.key_pass_id")) |>
  group_by(player.id) |>
  reframe(
    goals = sum(shot.outcome.id == 97, na.rm=T),
    xG = sum(shot.statsbomb_xg, na.rm=T),
    shots = sum(type.name=="Shot", na.rm=T),
    assists = sum(pass.goal_assist, na.rm=T),
    xAG = sum(xGA, na.rm=T),
    passes = sum(type.name=="Pass", na.rm=T),
    pass_pct = sum(type.name=="Pass" & is.na(pass.outcome.name), na.rm=T) / passes,
    dribbles = sum(dribble.outcome.name == "Complete", na.rm=T),
    tackles = sum(duel.type.name=="Tackle", na.rm=T),
    interceptions = sum(interception.outcome.name %in% c("Success", "Success In Play", "SUccess Out", "Won"), na.rm=T),
    blocks = sum(type.name == "Block", na.rm=T),
    clearances = sum(type.name == "Clearance", na.rm=T),
    aerials = sum(clearance.aerial_won | miscontrol.aerial_won | pass.aerial_won | shot.aerial_won | clearance.aerial_won, na.rm=T),
  ) |>
  ungroup()

pm <- mins |>
  select(player_id, player_season_minutes)

stats <- stats |>
  left_join(pm, by = c("player.id" = "player_id"))

per90 <- stats |>
  mutate(
    goals_per_90 = (goals / player_season_minutes) * 90,
    xG_per_90 = (xG / player_season_minutes) * 90,
    shots_per_90 = (shots / player_season_minutes) * 90,
    assists_per_90 = (assists / player_season_minutes) * 90,
    xAG_per_90 = (xAG / player_season_minutes) * 90,
    passes_per_90 = (passes / player_season_minutes) * 90,
    pass_pct_per_90 = pass_pct,
    dribbles_per_90 = (dribbles / player_season_minutes) * 90,
    tackles_per_90 = (tackles / player_season_minutes) * 90,
    interceptions_per_90 = (interceptions / player_season_minutes) * 90,
    blocks_per_90 = (blocks / player_season_minutes) * 90,
    clearances_per_90 = (clearances / player_season_minutes) * 90,
    aerials_per_90 = (aerials / player_season_minutes) * 90
  )

percentiles <- per90 |>
  mutate(
    goals_pct = round(percent_rank(goals_per_90) * 100),
    xG_pct = round(percent_rank(xG_per_90) * 100),
    shots_pct = round(percent_rank(shots_per_90) * 100),
    assists_pct = round(percent_rank(assists_per_90) * 100),
    xAG_pct = round(percent_rank(xAG_per_90) * 100),
    passes_pct = round(percent_rank(passes_per_90) * 100),
    pass_pct_pct = round(percent_rank(pass_pct_per_90) * 100),
    dribbles_pct = round(percent_rank(dribbles_per_90) * 100),
    tackles_pct = round(percent_rank(tackles_per_90) * 100),
    interceptions_pct = round(percent_rank(interceptions_per_90) * 100),
    blocks_pct = round(percent_rank(blocks_per_90) * 100),
    clearances_pct = round(percent_rank(clearances_per_90) * 100),
    aerials_pct = round(percent_rank(aerials_per_90) * 100)
  ) |>
  filter(player_season_minutes > 180)
