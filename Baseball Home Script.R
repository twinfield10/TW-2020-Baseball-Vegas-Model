##Packages
install.packages("xlsx")
install.packages("baseballr")
require(devtools)
require(baseballr)
library(dplyr)
library(tidyverse)
library(na.tools)
library(xml2)
library(rvest)
library(stringr)
nest <- nest_legacy
unnest <- unnest_legacy
`%notin%` <- Negate(`%in%`)
library(scales)
library(gridExtra)

#Import CSV of PECOTA Data from Baseball Prospectus
hit_pecotajul25<- read.csv('pecota2020_hitting_jul25.csv', header = TRUE, stringsAsFactors = FALSE)
pitch_pecotajul25<- read.csv('pecota2020_pitching_jul25.csv', header = TRUE, stringsAsFactors = FALSE)

##Create Team Ref Table 
teamabbr <- data.frame(c('Los Angeles Angels', 'Arizona Diamondbacks', 'Baltimore Orioles', 'Boston Red Sox', 'Chicago Cubs', 'Cincinnati Reds',
                            'Cleveland Indians', 'Colorado Rockies', 'Detroit Tigers', 'Houston Astros', 'Kansas City Royals', 'Los Angeles Dodgers',
                            'Washington Nationals', 'New York Mets', 'Oakland Athletics', 'Pittsburgh Pirates', 'San Diego Padres', 'Seattle Mariners',
                            'San Francisco Giants', 'St. Louis Cardinals', 'Tampa Bay Rays', 'Texas Rangers', 'Toronto Blue Jays', 'Minnesota Twins',
                            'Philadelphia Phillies', 'Atlanta Braves', 'Chicago White Sox', 'Miami Marlins', 'New York Yankees', 'Milwaukee Brewers'),
stringsAsFactors = FALSE)
names(teamabbr) <- c("full_name")

teamabbr$bp_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHN', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KCA', 'LAN',
                          'WAS', 'NYN', 'OAK', 'PIT', 'SDN', 'SEA',
                          'SFN', 'SLN', 'TBA', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHA', 'MIA', 'NYA', 'MIL')

teamabbr$bp_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHN', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KCA', 'LAN',
                          'WAS', 'NYN', 'OAK', 'PIT', 'SDN', 'SEA',
                          'SFN', 'SLN', 'TBA', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHA', 'MIA', 'NYA', 'MIL')
teamabbr$vegasabbr <- c('L.A. Angels', 'Arizona', 'Baltimore', 'Boston', 'Chi. Cubs', 'Cincinnati',
                        'Cleveland', 'Colorado', 'Detroit', 'Houston', 'Kansas City', 'L.A. Dodgers',
                        'Washington', 'N.Y. Mets', 'Oakland', 'Pittsburgh', 'San Diego', 'Seattle',
                        'San Francisco', 'St. Louis', 'Tampa Bay', 'Texas', 'Toronto', 'Minnesota',
                        'Philadelphia', 'Atlanta', 'Chi. White Sox', 'Miami', 'N.Y. Yankees', 'Milwaukee')
teamabbr$bm_teamabbr <- c('LAA', 'ARI', 'BAL', 'BOS', 'CHC', 'CIN',
                          'CLE', 'COL', 'DET', 'HOU', 'KC', 'LAD',
                          'WAS', 'NYM', 'OAK', 'PIT', 'SD', 'SEA',
                          'SF', 'STL', 'TB', 'TEX', 'TOR', 'MIN',
                          'PHI', 'ATL', 'CHW', 'MIA', 'NYY', 'MIL')



##Create Roster With ID's
MLBRosters <- function(hit,pitch) {
rosters <- hit %>%
  select(bpid, mlbid, name, first_name, last_name, team, season)
rostersp <- pitch %>%
  select(bpid, mlbid, name, first_name, last_name, team, season)
rbind(rosters, rostersp)
}
roster <- MLBRosters(hit_pecotajul25, pitch_pecotajul25)

hit_pecotajul25 <- hit_pecotajul25 %>%
  inner_join(teamabbr, by = c("team" = "bp_teamabbr"))
pitch_pecotajul25 <- pitch_pecotajul25 %>%
  inner_join(teamabbr, by = c("team" = "bp_teamabbr"))

##Create Team WAR Projections
team_p_war <- pitch_pecotajul25 %>%
  filter(dc_fl == 'TRUE') %>%
  select(mlbid, name, team, warp) %>%
  group_by(team) %>%
  summarize (
    p_team_war = sum(warp)
  )

team_hit_war<- hit_pecotajul25 %>%
  filter(dc_fl == 'TRUE') %>%
  select(mlbid, name, team, warp) %>%
  group_by(team) %>%
  summarize (
    hit_team_war = sum(warp)
  )
team_war <- team_hit_war %>%
  inner_join(team_p_war, by = c("team"))

team_war <- team_war %>%
  mutate(total_war = hit_team_war + p_team_war)
sp_war <- pitch_pecotajul25 %>%
  filter(dc_fl == 'TRUE') %>%
  select(mlbid, name, team, warp, gs, g) %>%
  mutate(starter_pct = gs/g,
         starter_war = starter_pct*warp) %>%
  group_by(team) %>%
  summarize(
    SP_team_war = sum(starter_war)
  )
team_war <- team_war %>%
  inner_join(sp_war, by = c("team"))

team_war <- team_war %>%
  mutate(RP_team_war = p_team_war - SP_team_war)


##Create Cluster Luck Column for Standings Table
clusterluck <- read_html("https://thepowerrank.com/cluster-luck/") %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  mutate(
    Team = case_when(
      Team == "Los Angeles Dodgers" ~ "L.A. Dodgers",
      Team == "Los Angeles Angels" ~ "L.A. Angels",
      Team == "New York Mets" ~ "N.Y. Mets",
      Team == "New York Yankees" ~ "N.Y. Yankees",
      Team == "Chicago Cubs" ~ "Chi. Cubs",
      Team == "Chicago White Sox" ~ "Chi. White Sox",
      TRUE ~ Team
    )
  ) %>%
  data.frame( stringsAsFactors = FALSE) %>%
  inner_join(teamabbr, by = c("Team" = "vegasabbr")) %>%
  mutate(
    Off = as.numeric(Off),
    Def = as.numeric(Def),
  )

####SCRAPE VEGAS LINES TO CHECK FOR ARBITRAGE OPPORTUNITIES
get_lines <- function(dt) {
  lines <- read_html("https://www.vegasinsider.com/mlb/odds/las-vegas/") %>%
    html_nodes(".cellBorderL1") %>%
    html_text() %>%
    trimws()
  lines <- str_subset(lines, "^[0123456789]")
  line_df <- data.frame(gameinfo = str_subset(lines, "^0"))%>%
    separate(gameinfo, into = c("Date", "Other"), sep = 5, remove = TRUE)
  line_df$Other <-gsub('[[:digit:]]+', '', line_df$Other)
  line_df$Other <-gsub('PM', '', line_df$Other)
  line_df$Other <-gsub(':', '', line_df$Other)
  line_df$Other <- trimws(line_df$Other, which = c("left"))
  line_df$Other <- sub('.', '', line_df$Other)
  line_df$Other <- trimws(line_df$Other)
  elems <- unlist( strsplit(line_df$Other, "\n\t\t\t\t" ) )
  m <- matrix( elems , ncol = 2 , byrow = TRUE )
  m <- as.data.frame(m)
  line_df <- cbind(line_df, m)
  
  BDTNL <- number_line()
  
  line_df <- line_df %>%
    select(Date, V1, V2)
  names(line_df) <- c("date", "away", "home")
  line_df$home  <- gsub("^.", "", line_df$home)
  
  line_df <- cbind(line_df, BDTNL) %>%
    select(date, away, home, away_line, home_line)
  
  line_df <- line_df %>%
    separate(date, into = c("month", "day"), sep = "/", remove = TRUE) %>%
    mutate(
      full_date = paste("2020", month, day, sep = "-")
    ) %>%
    inner_join(teamabbr, by = c("home" = "vegasabbr")) %>%
    select(full_date, home, away, TeamID, away_line, home_line) %>%
    mutate(home_team_id = TeamID)
  line_df <- line_df %>%
    inner_join(teamabbr, by = c("away" = "vegasabbr")) %>%
    select(full_date, home, away, home_team_id, TeamID.y, away_line, home_line) %>%
    mutate(away_team_id = TeamID.y) %>%
    select(full_date, home, away, home_team_id, away_team_id, away_line, home_line) %>%
    mutate(
      game_id = mapply(pk_lu, full_date, away_team_id, home_team_id)
    )%>%
    filter(full_date == dt)
  line_df
}
line_df <- get_lines("2020-07-24")


number_line <- function() {
  lines <- read_html("https://www.vegasinsider.com/mlb/odds/las-vegas/") %>%
    html_nodes(".cellBorderL1") %>%
    html_text() %>%
    trimws()
  m <- matrix(lines, ncol = 10, byrow = TRUE)
  m <- as.data.frame(m)
  m$V3 <- substring(m$V3, nchar(as.character(m$V3)) - 7)
  m <- m %>%
    select(V1, V3) %>%
    separate(V3, into = c("away_line", "home_line"), sep = 4, remove = TRUE)
  m
}


####CREATE Standings table
###Update Pecota Standings: Updates weekly
##Last Update: 09/01/2020

test_df <- rbind.data.frame(
  c("NYA",34.3,25.7,298,256),
  c("TBA",37.5,22.5,304,262),
  c("BOS",24.1,35.9,285,346),
  c("TOR",31.6,28.4,290,273),
  c("BAL",27.0,33.0,277,326),
  c("MIN",33.3,26.7,292,247),
  c("CLE",34.7,25.3,270,219),
  c("CHA",34.4,25.6,310,260),
  c("DET",27.6,30.4,270,306),
  c("KCA",25.7,34.3,258,293),
  c("HOU",33.1,25.9,315,277),
  c("OAK",35.7,23.3,297,248),
  c("LAA",24.4,35.6,295,311),
  c("TEX",24.1,35.9,242,318),
  c("SEA",24.7,35.3,257,334),
  c("WAS",26.2,33.8,291,291),
  c("NYN",28.7,31.3,278,282),
  c("ATL",33.5,26.5,310,282),
  c("PHI",29.6,30.4,317,314),
  c("MIA",29.4,30.6,273,289),
  c("CIN",28.3,31.7,269,274),
  c("CHN",33.7,26.3,301,277),
  c("SLN",28.6,29.4,266,264),
  c("MIL",29.1,30.9,266,296),
  c("PIT",22.5,37.5,255,322),
  c("LAN",40.6,19.4,337,212),
  c("ARI",26.0,34.0,270,307),
  c("COL",28.5,31.5,295,323),
  c("SDN",33.4,26.6,319,279),
  c("SFN",28.6,31.4,285,321),
  stringsAsFactors = FALSE
)
names(test_df) <- c("Team", "W", "L", "RS", "RA")
test_df <- test_df %>%
  inner_join(teamabbr, by = c("Team" = "bp_teamabbr")) %>%
  select(Team, W, L, RS, RA, TeamID,TeamName, TeamAbbr)
standings <- test_df
standings <- standings %>%
  inner_join(clusterluck, by = c("TeamID")) %>%
  mutate(
    RS = as.integer(RS),
    RA = as.integer(RA),
    CL_RS = (RS - Off),
    CL_RA = (RA + Def),
  ) %>%
  select(Team.x, W, L, RS, RA, CL_RS, CL_RA, TeamID, TeamName.x, TeamAbbr.x)
names(standings) = c("Team", "W", "L", "RS", "RA", "CL_RS", "CL_RA", "TeamID", "TeamName", "TeamAbbr")






  