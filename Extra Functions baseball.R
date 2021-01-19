####FUNCTIONS FOR LOOK UPS
teamidlookup <- function(abbr) {
  teamabbr %>%
    filter(TeamAbbr == abbr) %>%
    select(TeamID) %>%
    print() %>%
    as.integer()
}

gameidforfun<- function(x) {
  daily_schedule(x) %>%
    select(game_pk)
}

gamedateforfun <- function(x) {
  daily_schedule(x) %>%
    select(officialDate)
}

daily_schedule <- function(dt) {
  get_game_pks_mlb(dt, level_ids = c(1)) %>% 
    select(game_pk, teams.home.team.id, teams.away.team.id, gameNumber, officialDate)
}

p_lineupfunction <- function(gamedate, away.team, home.team, gamenum = 1) {
  get_probables_mlb(
    get_game_pks_mlb(gamedate, level_ids = c(1)) %>%
      filter(teams.away.team.id == away.team, teams.home.team.id == home.team, gameNumber == gamenum) %>%
      select(game_pk)
  )
}

pk_lu <- function(gamedate, away.team, home.team, gamenum = 1) {
  p <- get_game_pks_mlb(gamedate, level_ids = c(1)) %>%
    filter(teams.away.team.id == away.team, teams.home.team.id == home.team, gameNumber == gamenum) %>%
    select(game_pk)
  as.character(p)
}

team_p_war_lu <- function (x) {
  team_war %>%
    filter(team == x) %>%
    select(p_team_war)
}

standings_RS_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(RS)
}

standings_RA_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(RA)
}

teamRP_lu <- function (x) {
  team_war %>%
    filter(team == x) %>%
    select(RP_team_war)
}

CL_standings_RA_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(CL_RA)
}
CL_standings_RS_lu <- function(tm){
  standings %>%
    filter(Team == tm) %>%
    select(CL_RS)
}


##Daily Schedule Function to pull all games
pitcher_prob_test <- function(dt) {
  day_rosters <- read_csv("https://baseballmonster.com/lineups.aspx?csv=1")
  names(day_rosters) = c("team", "game_date", "game_number", "mlb_id", "player", "batting_order", "confirmed", "position")
  day_rosters <- day_rosters %>%
    filter(batting_order == 'SP') %>%
    select(team, mlb_id, player, game_number)
  day_rosters <- day_rosters %>%
    inner_join(teamabbr, by = c("team" = "bm_teamabbr")) %>%
    select(mlb_id, player, game_number, TeamID)
  
  schedule <- get_game_pks_mlb(dt, level_ids = c(1)) %>%
    select(game_pk, teams.away.team.id, teams.home.team.id)
  
  home_sched <- schedule %>%
    select(game_pk, teams.home.team.id)
  home_sched$teams.home.team.id <- as.character(home_sched$teams.home.team.id)
  home_sched <- home_sched %>%
    inner_join(day_rosters, by = c("teams.home.team.id" = "TeamID")) %>%
    select(game_pk, teams.home.team.id, mlb_id, player)
  
  away_sched <- schedule %>%
    select(game_pk, teams.away.team.id)
  away_sched$teams.away.team.id <- as.character(away_sched$teams.away.team.id)
  away_sched <- away_sched %>%
    inner_join(day_rosters, by = c("teams.away.team.id" = "TeamID")) %>%
    select(game_pk, teams.away.team.id, mlb_id, player)
  
  names(home_sched) = c("game_pk", "home.TeamID","home_p_id", "home_p_name")
  names(away_sched) = c("game_pk", "away.TeamID","away_p_id", "away_p_name")
  
  df <- home_sched %>%
    inner_join(away_sched, by = c("game_pk"))
  df
}

##See all Game PKs
#get_game_pks_mlb("2020-08-25")


##Run Game ID through individual line creation function
##Number of gid(x) = g(x)


#Get model created lines based on WAR
##FUNCTION FOR INDIVIDUAL GAME LOOK UP FOR APPLY FUNCTION
pitching_wp <- function(g_id) {
  h_team <- daily_prob_pitching %>%
    filter(game_pk %in% g_id) %>%
    select(game_pk, home.TeamID, home_p_id, home_p_name)
  h_t <- as.integer(h_team$home.TeamID)
  h_p <- as.integer(h_team$home_p_id)
  
  a_team <- daily_prob_pitching %>%
    filter(game_pk %in% g_id) %>%
    select(game_pk, away.TeamID, away_p_id, away_p_name)
  a_t <- as.integer(a_team$away.TeamID)
  a_p <- as.integer(a_team$away_p_id)
  
  home_wp <- pitch_pecotajul25 %>%
    filter(TeamID == h_t, mlbid == h_p) %>%
    select(name, team, gs, warp, ip) %>%
    mutate(if60war = (warp*6/ip)*60,
           p_war = team_p_war_lu(team),
           p_adv = (if60war - p_war)*10,
           h_team_rs = as.integer(standings_RS_lu(team)),
           h_team_ra = as.integer(standings_RA_lu(team)),
           if60RA = (h_team_ra - p_adv)) %>%
    mutate(p_wp = ((h_team_rs)^1.83)/(((h_team_rs)^1.83)+((if60RA)^1.83)))%>%
    select(team, name, p_wp)
  
  away_wp <- pitch_pecotajul25 %>%
    filter(TeamID == a_t, mlbid == a_p) %>%
    select(name, team, gs, warp, ip) %>%
    mutate(if60war = (warp*6/ip)*60,
           p_war = team_p_war_lu(team),
           p_adv = (if60war - p_war)*10,
           a_team_rs = as.integer(standings_RS_lu(team)),
           a_team_ra = as.integer(standings_RA_lu(team)),
           if60RA = (a_team_ra - p_adv)) %>%
    mutate(p_wp = ((a_team_rs)^1.83)/(((a_team_rs)^1.83)+((if60RA)^1.83)))%>%
    select(team, name, p_wp)
  
  wp_df <- data.frame(home_team = home_wp$team,
                      home_win = home_wp$p_wp,
                      away_team = away_wp$team,
                      away_win = away_wp$p_wp,
                      game_id = h_team$game_pk)
  names(wp_df) = c("home_team","home_win","away_team","away_win", "game_id")
  wp_df <- wp_df %>%
    mutate(
      home_loss = 1 - home_win,
      away_loss = 1 - away_win,
      home_w_away_l = home_win*away_loss,
      home_l_away_w = home_loss*away_win,
      home_winpercent = (home_w_away_l/(home_w_away_l+home_l_away_w))+0.025,
      away_winpercent = (home_l_away_w/(home_w_away_l+home_l_away_w))-0.025,
      h_odds_raw = 1/home_winpercent,
      a_odds_raw = 1/away_winpercent,
      home_ML_odds = ifelse(h_odds_raw > 2, 100*(h_odds_raw-1), (-100)/(h_odds_raw-1)),
      away_ML_odds = ifelse(a_odds_raw > 2, 100*(a_odds_raw-1), (-100)/(a_odds_raw-1))
    ) %>%
    select(game_id, home_team, home_ML_odds, away_team, away_ML_odds)
  data.frame(wp_df)
}

##Set Up Complete, now run output
#####COMBINE LINES AND PREDICTION
money_maker <- function(dt) {
  vegas_test  <- get_lines(dt)
  tommy_test <- hardcodebets
  
  bets <- vegas_test %>%
    inner_join(tommy_test, by = c("game_id")) %>%
    select(game_id, home, home_ML_odds, away, away_ML_odds
           ,home_line, away_line, betonline_away, betonline_home)
  bets$home_line <- gsub("[+]", "", bets$home_line)
  bets$away_line <- gsub("[+]", "", bets$away_line)
  bets$home_line <- as.numeric(bets$home_line)
  bets$away_line <- as.numeric(bets$away_line)
  
  
  bets <- bets %>%
    mutate(
      home_actual_abs = abs(home_line),
      away_actual_abs = abs(away_line),
      home_predict_abs = abs(home_ML_odds),
      away_predict_abs = abs(away_ML_odds),
      home_BOL_abs = abs(betonline_home),
      away_BOL_abs = abs(betonline_away),
      home_prob_actual = if_else(home_line > 0,
                                 100/(home_line+100),
                                 home_actual_abs/(home_actual_abs+100)),
      away_prob_actual = if_else(away_line > 0,
                                 100/(away_line+100),
                                 away_actual_abs/(away_actual_abs+100)),
      home_prob_model = if_else(home_ML_odds > 0,
                                100/(home_ML_odds+100),
                                home_predict_abs/(home_predict_abs+100)),
      away_prob_model = if_else(away_ML_odds > 0,
                                100/(away_ML_odds+100),
                                away_predict_abs/(away_predict_abs+100)),
      home_prob_BOL = if_else(betonline_home > 0,
                              100/(betonline_home+100),
                              home_BOL_abs/(home_BOL_abs+100)),
      away_prob_BOL = if_else(betonline_away > 0,
                              100/(betonline_away+100),
                              away_BOL_abs/(away_BOL_abs+100)),
      home_imp_prob_actual = home_prob_actual/(home_prob_actual+away_prob_actual),
      away_imp_prob_actual = away_prob_actual/(home_prob_actual+away_prob_actual),
      home_imp_prob_model = home_prob_model/(home_prob_model+away_prob_model),
      away_imp_prob_model = away_prob_model/(home_prob_model+away_prob_model),
      home_imp_prob_BOL = home_prob_BOL/(home_prob_BOL+away_prob_BOL),
      away_imp_prob_BOL = away_prob_BOL/(home_prob_BOL+away_prob_BOL),
      home_adv = home_imp_prob_model - home_imp_prob_actual,
      away_adv = away_imp_prob_model - away_imp_prob_actual,
      home_BOL_adv = home_imp_prob_model - home_imp_prob_BOL,
      away_BOL_adv = away_imp_prob_model - away_imp_prob_BOL,
      abs_adv = abs(home_adv),
      abs_bol_adv = abs(home_BOL_adv),
      BOLAdvantage = abs(home_BOL_adv),
      Advantage = abs(home_adv),
      BOLAdvantage = scales::percent(BOLAdvantage),
      Advantage = scales::percent(Advantage),
      bet = if_else(home_adv > 0, home, away),
      BOLbet = if_else(home_BOL_adv > 0, home, away),
      unit_bet = if_else(abs_adv > .2, 10, if_else(abs_adv > .15, 8, if_else(abs_adv > .1, 6, if_else(abs_adv > .055, 4, if_else(abs_adv >.025, 2, 1))))),
      BOL_unit_bet = if_else(abs_bol_adv > .2, 10, if_else(abs_bol_adv > .15, 8, if_else(abs_bol_adv > .1, 6, if_else(abs_bol_adv > .055, 4, if_else(abs_bol_adv >.025, 2, 1)))))
    )
  bets <- bets %>%
    select(game_id, home, home_ML_odds,home_line, away, away_ML_odds, away_line, Advantage, bet, unit_bet, betonline_home, betonline_away,BOLAdvantage, BOLbet, BOL_unit_bet)
  bets <- bets %>%
    mutate(
      BOL_bet_odds = if_else(BOLbet == home, betonline_home, betonline_away),
      abs_bol_odds = abs(BOL_bet_odds),
      euro_odds = if_else(BOL_bet_odds > 0, ((BOL_bet_odds/100)+1), (((100/(abs_bol_odds))+1))),
      risk = if_else(euro_odds < 2, (BOL_unit_bet/(euro_odds - 1)), BOL_unit_bet),
      to_win = ((risk*euro_odds) - risk)
    )
  bets
}
####RUN TO GET DAILY BETS####
#######Change Date Every Day#######
Sep012020Bets <- money_maker("2020-09-01")


##Add In Wins and Losses For the Day
##Manually Change
Sep012020Bets$w_l <- c(
  ## 1,1,0,1,0,1,1,1,0,0,0
)
Sep012020Bets <- Sep012020Bets %>%
  mutate(
    profit = if_else(w_l == 1, to_win, if_else(w_l == 0, (risk*-1), 0))
  )
Results <- rbind(Results, Sep012020Bets)

##UNIT = $2.50##
##Plot Results on Line Graph##
summary_results <- function(var = c(1,2,4,6,8,10)){
  df  <- Results %>% 
    filter(BOL_unit_bet %in% var)
  
  unit_profit_raw <- ((sum(df$profit)/10))
  unit_profit <- round(unit_profit_raw, digits = 2)
  unit_profit <- paste(unit_profit, "u", sep = "")
  dollar_profit <- (unit_profit_raw*25)
  dollar_profit <- round(dollar_profit, digits = 2)
  dollar_profit <- dollar(dollar_profit)
  ROI <- unit_profit_raw/(sum(df$risk))
  ROI <- round(ROI*100, digits = 2)
  ROI <- paste(ROI, "%", sep = "")
  wins <- (sum(df$w_l == 1, na.rm=TRUE) + 29)
  losses <- (sum(df$w_l == 0, na.rm=TRUE) +27)
  win_pct <- wins/(wins+losses)
  win_pct <- round(win_pct*100, digits = 2)
  win_pct <- paste(win_pct, "%", sep = "")
  df_t <- data_frame(unit_profit, dollar_profit, ROI, win_pct, wins, losses)
  names(df_t) <- c("Unit Profit", "Dollar Profit", "ROI", "Win %", "Wins", "Losses")
  unit_profit_var_raw <- (sum(df$profit)/10)
  unit_profit_var <- round(unit_profit_var_raw, digits = 2)
  unit_profit_var <- paste(unit_profit_var, "u", sep = "")
  dollar_profit_var <- (unit_profit_var_raw*25)
  dollar_profit_var <- round(dollar_profit_var, digits = 2)
  dollar_profit_var <- dollar(dollar_profit_var)
  ROI_var <- (unit_profit_var_raw/(sum(df$risk)))
  ROI_var <- round(ROI_var*100, digits = 2)
  ROI_var <- paste(ROI_var, "%", sep = "")
  wins_var <- sum(df$w_l == 1, na.rm=TRUE)
  losses_var <- sum(df$w_l == 0, na.rm=TRUE)
  win_pct_var <- wins_var/(wins_var+losses_var)
  win_pct_var <- round(win_pct_var*100, digits = 2)
  win_pct_var <- paste(win_pct_var, "%", sep = "")
  df_var <- data_frame(unit_profit_var, dollar_profit_var, ROI_var, win_pct_var, wins_var, losses_var)
  names(df_var) <- c("Unit Profit", "Dollar Profit", "ROI", "Win %", "Wins", "Losses")
  
  d <- df
  d$game_id <- as.integer(d$game_id)
  d <- d %>%
    inner_join(completed_games, by = c("game_id" = "game_pk"))
  len_a <- (length(d$game_id)+1)
  game_num <- c(2:len_a)
  d <- cbind(d,game_num)
  len <- (length(d$game_id))
  
  d <- d %>%
    select(game_num, profit) %>%
    mutate(
      profit_dol = profit*2.5
    ) %>%
    select(game_num, profit_dol)
  
  d <- rbind(c(1, 0), d)
  d$game_number <- factor(d$game_num, levels = c(1:len))
  
  p <- d %>%
    ggplot(aes(x=game_number, y=cumsum(profit_dol), group=1)) +
    geom_line()+
    geom_hline(yintercept = 0, color = "red", linetype = 2)+
    geom_point() +
    labs(title="Cumulative Profits",x="Game Number", y = "Profit/Loss ($)")+
    theme_classic()
  
  q <- d %>%
    ggplot(aes(x=game_number, y=cumsum(profit_dol), group=1)) +
    geom_line()+
    geom_hline(yintercept = 0, color = "red", linetype = 2)+
    labs(title="Cumulative Profits",x="Game Number", y = "Profit/Loss ($)")+
    theme_classic()
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                       base_size = 10,
                       padding = unit(c(2, 4), "mm"))
  xyz <- df_t
  tbl <- tableGrob(xyz, rows=NULL, theme=tt)
  #tbl_var <- tableGrob(df_var, rows=NULL, theme=tt)
  n <- grid.arrange(p, tbl, nrow = 2, heights = c(2, 0.5))
  #m <- grid.arrange(q, tbl_var, nrow = 2, heights = c(2, 0.5))
  n
}
summary_results()

##Change date to current date##
completed_games <- rbind(completed_games, daily_schedule("2020-08-31"))

##Save Results into CSV##
write_csv(Results, 'results_2020.csv')



