##Filter Games with Unannounced Pitchers
daily_prob_pitching<- pitcher_prob_test("2020-09-01")
##If Duplicate, Filter out Game PK Until Available
#%>%
#filter(game_pk %notin% c("631458", "631457", "631581", "631595","631428", "631427"))


#Get Game ID, Teams, Starting Pitchers
gid1 <- daily_prob_pitching[1,1]
gid2 <- daily_prob_pitching[2,1]
gid3 <- daily_prob_pitching[3,1]
gid4 <- daily_prob_pitching[4,1]
gid5 <- daily_prob_pitching[5,1]
gid6 <- daily_prob_pitching[6,1]
gid7 <- daily_prob_pitching[7,1]
gid8 <- daily_prob_pitching[8,1]
gid9 <- daily_prob_pitching[9,1]
gid10 <- daily_prob_pitching[10,1]
gid11 <- daily_prob_pitching[11,1]
gid12 <- daily_prob_pitching[12,1]
gid13 <- daily_prob_pitching[13,1]
gid14 <- daily_prob_pitching[14,1]
gid15 <- daily_prob_pitching[15,1]

##Run Each Game
g1 <- pitching_wp(gid1)
g2 <- pitching_wp(gid2)
g3 <- pitching_wp(gid3)
g4 <- pitching_wp(gid4)
g5 <- pitching_wp(gid5)
g6 <- pitching_wp(gid6)
g7 <- pitching_wp(gid7)
g8 <- pitching_wp(gid8)
g9 <- pitching_wp(gid9)
g10 <- pitching_wp(gid10)
g11 <- pitching_wp(gid11)
g12 <- pitching_wp(gid12)
g13 <- pitching_wp(gid13)
g14 <- pitching_wp(gid14)
g15 <- pitching_wp(gid15)
#combine into 1 table
hardcodebets <- rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15) 
#change ID to character
hardcodebets$game_id <- as.character(hardcodebets$game_id)
#hard code lines to match bookie lines, no API exists for book
hardcodebets$betonline_away <- c( 148,-103,-158, 138, 139,-162,-135, 175,-198, 237, 100, 104, 200)
hardcodebets$betonline_home <- c(-160,-107, 146,-149,-150, 149, 125,-190, 182,-265,-110,-114,-220)

####RUN TO GET DAILY BETS####
#######Change Date Every Day#######
Sep012020Bets <- money_maker("2020-09-01")








 