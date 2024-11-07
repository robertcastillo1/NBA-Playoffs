options(width=90, xtable.comment = FALSE)

if( !dir.exists("_assets") ) {
  dir.create("_assets")
}

library(RMySQL)
library(dplyr)
library(corrplot)
library(xtable)
library(caret)
library(boot)
library(msm)
library(tibble)

drv <- dbDriver("MySQL")

########################################
xdbsock <- ""

xdbuser <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_USER")
xdbpw   <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_PW")
xdbname <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_DBNAME")
xdbhost <- Sys.getenv("MAS405_AWS_MY_DB_ROUSER_HOST")
xdbport <- as.integer( Sys.getenv("MAS405_AWS_MY_DB_ROUSER_PORT") )

con <-
  dbConnect(
    drv,
    user=xdbuser,
    password=xdbpw,
    dbname=xdbname,
    host=xdbhost,
    port=xdbport,
    unix.sock=xdbsock
  )

dbListTables(con)

dbGetInfo(con)

## Load All of 2010s data
xtableName <- "nba_2010s_data"

qstr <- paste0("SELECT * FROM ", xtableName)

data <- dbGetQuery(con, qstr)

data <- data[-c(1)]

head(data)


## Load Playoff Data

qstr <- paste0("SELECT * FROM ", xtableName, " WHERE PLAYOFF = 'True'")

playoff_data <- dbGetQuery(con, qstr)

playoff_data <- playoff_data[-c(1)]

# Histogram

png(
  file.path( "_assets", "myPlot_Reg_Season_Wins.png"),
  width=1080,
  height=800,
  pointsize=30
)

par(mar=c(4, 4, 4, 1))

hist(
  data$RS_WINS, breaks = 7, 
  ylim = range(0:80), 
  col = '#91171b', 
  main = "Regular Season Wins", 
  xlab = "Regular Season Wins"
)
dev.off()

## Box plots
# All Teams by Conference

png(
  file.path("_assets", "myPlot_Reg_Season_Wins_Conference.png"),
  width=1200,
  height=800,
  pointsize=20
)

par(mfrow=c(2,4))

for (i in unique(data$SEASON_ID)) {
  temp_data <- filter(data, data$SEASON_ID == i)
  boxplot(
    temp_data$RS_WINS ~ temp_data$CONF, 
    main=paste0(i), 
    xlab = "Conference", 
    ylab = "Regular Season Wins", 
    col = '#91171b')
}

dev.off()

# Playoff Teams by Conference
temp_playoff <- playoff_data[order(playoff_data$SEASON_ID), ]

png(
  file.path("_assets", "myPlot_Reg_Season_Wins_Playoff_Teams_Conference.png"),
  width=1200,
  height=800,
  pointsize=20
)

par(mfrow=c(2,4))

for (i in unique(temp_playoff$SEASON_ID)){
  print(i)
  temp_data <- filter(playoff_data, playoff_data$SEASON_ID == i)
  boxplot(temp_data$RS_WINS ~ temp_data$CONF, 
          main = paste0("Playoff Teams: ", i),
          xlab = "Conference", 
          ylab = "Regular Season Wins", 
          col = '#91171b')
}

dev.off()

## Tables
# Above .500 missed playoffs

qstr <- paste0("SELECT * FROM ", xtableName, " WHERE PLAYOFF = 'False' ", " AND RS_WINS >= 41")
above_500 <- dbGetQuery(con, qstr)

above_500 <- above_500[ , c('SEASON_ID', 'CONF', 'TEAM_ABBREVIATION', 'RS_WINS')]
above_500 <- above_500[order(above_500$SEASON_ID, above_500$CONF, above_500$TEAM_ABBREVIATION), ]

colnames(above_500) <- c("Season", "Conference", "Team","Regular Season Wins")

above_500_tbl <-
  xtable(above_500,
         caption="NBA Teams. Occurrences where a team had won at least half of games, but missed the playoffs",
         label="tab: missedplayoffsabove41wins")

xxx <- print(above_500_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_missedplayoffsabove41wins.tex") )

# Below .500 made playoffs

qstr <- paste0("SELECT * FROM ", xtableName, " WHERE PLAYOFF = 'True' ", " AND RS_WINS < 41")
under_500 <- dbGetQuery(con, qstr)

under_500 <- under_500[ , c('SEASON_ID', 'CONF', 'TEAM_ABBREVIATION', 'RS_WINS')]
under_500 <- under_500[order(under_500$SEASON_ID, under_500$CONF, under_500$TEAM_ABBREVIATION), ]

colnames(under_500) <- c("Season", "Conference", "Team","Regular Season Wins")

under_500_tbl <-
  xtable(under_500,
         caption="NBA Teams. Occurrences where a team had won less than half of games, but made the playoffs",
         label="tab: madeplayoffsunder41wins")

xxx <- print(under_500_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_madeplayoffsunder41wins.tex") )

# Most Successful Regular Season teams of the decade

qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SUM(RS_WINS) as TOT_RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, RS_WINS FROM ", xtableName, 
  ") AS temp ",
  " GROUP BY TEAM_ABBREVIATION ",
  " ORDER BY TOT_RS_WINS DESC"
)

reg_total_wins <- dbGetQuery(con, qstr)

reg_success <- head(reg_total_wins, 6)

colnames(reg_success) <- c("Conference", "Team", "Total Regular Season Wins")

reg_success_tbl <-
  xtable(reg_success,
         caption="NBA Teams.Teams with the Most Regular Season Wins in the 2010s",
         label="tab: regular_season_most_success")

xxx <- print(reg_success_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_regular_season_most_success.tex") )

# Least Successful Regular Season teams

reg_less_success <- tail(reg_total_wins, 6)
reg_less_success <- reg_less_success[order(reg_less_success$TOT_RS_WINS, decreasing = FALSE), ]

colnames(reg_less_success) <- c("Conference", "Team", "Total Regular Season Wins")

reg_less_success_tbl <-
  xtable(reg_less_success,
         caption="NBA Teams.Teams with the Fewest Regular Season Wins in the 2010s",
         label="tab: regular_season_least_success")

xxx <- print(reg_less_success_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_regular_season_least_success.tex") )

# Most Successful Playoff Teams

qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SUM(P_WINS) as TOT_P_WINS, COUNT(PLAYOFF) as PLAYOFF_APPS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, P_WINS, PLAYOFF FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp ",
  " GROUP BY TEAM_ABBREVIATION ",
  " ORDER BY TOT_P_WINS DESC"
)

p_total_wins <- dbGetQuery(con, qstr)

p_success <- head(p_total_wins, 6)

colnames(p_success) <- c("Conference", "Team", "Total Playoff Wins", "Playoff Appearances")

p_success_tbl <-
  xtable(p_success,
         caption="NBA Teams.Teams with the Most Playoff Wins in the 2010s and Number of Playoff Appearances",
         label="tab: playoff_most_success")

xxx <- print(p_success_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_playoff_most_success.tex") )

# Least Success Playoff Teams/Fewest Appearances

qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SUM(P_WINS) as TOT_P_WINS, COUNT(PLAYOFF) as MISSED_PLAYOFFS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, P_WINS, PLAYOFF FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp ",
  " GROUP BY TEAM_ABBREVIATION ",
  " ORDER BY MISSED_PLAYOFFS DESC"
)

no_p_appearances <- dbGetQuery(con, qstr)
no_p_appearances <- filter(no_p_appearances, no_p_appearances$MISSED_PLAYOFFS == 8)
no_p_appearances$MISSED_PLAYOFFS <- c(0,0)
colnames(no_p_appearances)[colnames(no_p_appearances) == "MISSED_PLAYOFFS"] = "PLAYOFF_APPS"

p_appearances <- rbind(p_total_wins, no_p_appearances)

p_less_success <- tail(p_appearances, 6)
p_less_success <- p_less_success[order(p_less_success$TOT_P_WINS, p_less_success$PLAYOFF_APPS, p_less_success$TEAM_ABBREVIATION, decreasing = FALSE), ]

colnames(p_less_success) <- c("Conference", "Team", "Total Playoff Wins", "Playoff Appearances")

p_less_success_tbl <-
  xtable(p_less_success,
         caption="NBA Teams.Teams with the Fewest Playoff Wins in the 2010s and Number of Playoff Appearances",
         label="tab: playoff_least_success")


xxx <- print(p_less_success_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_playoff_least_success.tex") )


## Correlation Plot

png(
  file.path( "_assets", "myPlot_Correlation_Plot.png"),
  width=900,
  height=800,
  pointsize=12
)

M = cor(select_if(data, is.numeric))
corrplot(M, method = 'square', type = 'lower', diag = FALSE, addCoef.col = 'black')

dev.off()

### Individual Variable Tables

## Assists
# Above 90th percentile no playoffs
ast_90 <- quantile(data$AST, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, AST, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, AST, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE AST > ", ast_90,
  " ORDER BY SEASON_ID ASC"
)

ast_no_p <- dbGetQuery(con, qstr)
colnames(ast_no_p) <- c("Conference", "Team", "Season", "Assists", "Total Regular Season Wins")
ast_no_p_tbl <-
  xtable(ast_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Assists, missed Playoffs",
         label="tab: above_90th_percentile_assists_no_playoffs")

xxx <- print(ast_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_assists_noplayoffs.tex") )

# Below Mean Made Playoffs
ast_10 <- quantile(data$AST, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, AST, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, AST, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE AST < ", ast_10,
  " ORDER BY SEASON_ID ASC"
)

ast_p <- dbGetQuery(con, qstr)
colnames(ast_p) <- c("Conference", "Team", "Season", "Assists", "Playoff Wins")
ast_p_tbl <-
  xtable(ast_p,
         caption="NBA Teams.Teams with less than 10th Percentile Assists, made Playoffs",
         label="tab: below_10th_percentile_assists_playoffs")

xxx <- print(ast_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_assists_playoffs.tex") )

## Blocks
# Above 90th percentile no playoffs
blk_90 <- quantile(data$BLK, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, BLK, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, BLK, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE BLK > ", blk_90,
  " ORDER BY SEASON_ID ASC"
)

blk_no_p <- dbGetQuery(con, qstr)
colnames(blk_no_p) <- c("Conference", "Team", "Season", "Blocks", "Regular Season Wins")
blk_no_p_tbl <-
  xtable(blk_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Blocks, missed Playoffs",
         label="tab: above_90th_percentile_blocks_no_playoffs")

xxx <- print(blk_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_blocks_noplayoffs.tex") )

# Below Mean Made Playoffs
blk_10 <- quantile(data$AST, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, BLK, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, BLK, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE BLK < ", blk_10,
  " ORDER BY SEASON_ID ASC"
)

blk_p <- dbGetQuery(con, qstr)
colnames(blk_p) <- c("Conference", "Team", "Season", "Blocks", "Playoff Wins")
blk_p_tbl <-
  xtable(blk_p,
         caption="NBA Teams.Teams with less than 10th Percentile Blocks, made Playoffs",
         label="tab: below_10th_percentile_blocks_playoffs")

xxx <- print(blk_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_blocks_playoffs.tex") )

## DREB
# Above 90th percentile no playoffs
dreb_90 <- quantile(data$DREB, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, DREB, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, DREB, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE DREB > ", dreb_90,
  " ORDER BY SEASON_ID ASC"
)

dreb_no_p <- dbGetQuery(con, qstr)
colnames(dreb_no_p) <- c("Conference", "Team", "Season", "Defensive Rebounds", "Regular Season Wins")
dreb_no_p_tbl <-
  xtable(dreb_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Defensive Rebounds, missed Playoffs",
         label="tab: above_90th_percentile_drebs_no_playoffs")

xxx <- print(dreb_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_drebs_noplayoffs.tex") )

# Below Mean Made Playoffs
dreb_10 <- quantile(data$DREB, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, DREB, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, DREB, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE DREB < ", dreb_10,
  " ORDER BY SEASON_ID ASC"
)

dreb_p <- dbGetQuery(con, qstr)
colnames(dreb_p) <- c("Conference", "Team", "Season", "Defensive Rebounds", "Playoff Wins")
dreb_p_tbl <-
  xtable(dreb_p,
         caption="NBA Teams.Teams with less than 10th Percentile Defensive Rebounds, made Playoffs",
         label="tab: below_10th_percentile_drebs_playoffs")

xxx <- print(dreb_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_drebs_playoffs.tex") )

## FG3_PCT
# Above 90th percentile no playoffs
fg3_pct_90 <- quantile(data$FG3_PCT, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG3_PCT, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG3_PCT, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE FG3_PCT > ", fg3_pct_90,
  " ORDER BY SEASON_ID ASC"
)

fg3_pct_no_p <- dbGetQuery(con, qstr)
colnames(fg3_pct_no_p) <- c("Conference", "Team", "Season", "3-Point Percentage", "Regular Season Wins")
fg3_pct_no_p_tbl <-
  xtable(fg3_pct_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile 3-Point Percent, missed Playoffs",
         label="tab: above_90th_percentile_fg3_pct_no_playoffs")

xxx <- print(fg3_pct_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_fg3_pct_noplayoffs.tex") )

# Below Mean Made Playoffs
fg3_pct_10 <- quantile(data$FG3_PCT, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG3_PCT, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG3_PCT, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE FG3_PCT < ", fg3_pct_10,
  " ORDER BY SEASON_ID ASC"
)

fg3_pct_p <- dbGetQuery(con, qstr)
colnames(fg3_pct_p) <- c("Conference", "Team", "Season", "3-Point Percentage", "Playoff Wins")
fg3_pct_p_tbl <-
  xtable(fg3_pct_p,
         caption="NBA Teams.Teams with less than 10th Percentile 3-Point Percent, made Playoffs",
         label="tab: below_10th_percentile_fg3_pct_playoffs")

xxx <- print(fg3_pct_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_fg3_pct_playoffs.tex") )

## FG_PCT
# Above 90th percentile no playoffs
fg_pct_90 <- quantile(data$FG_PCT, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG_PCT, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG_PCT, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE FG_PCT > ", fg_pct_90,
  " ORDER BY SEASON_ID ASC"
)

fg_pct_no_p <- dbGetQuery(con, qstr)
colnames(fg_pct_no_p) <- c("Conference", "Team", "Season", "Field Goal Percentage", "Regular Season Wins")
fg_pct_no_p_tbl <-
  xtable(fg_pct_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile FG Percent, missed Playoffs",
         label="tab: above_90th_percentile_fg_pct_no_playoffs")

xxx <- print(fg_pct_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_fg_pct_noplayoffs.tex") )

# Below Mean Made Playoffs
fg_pct_10 <- quantile(data$FG_PCT, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG_PCT, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FG_PCT, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE FG_PCT < ", fg_pct_10,
  " ORDER BY SEASON_ID ASC"
)

fg_pct_p <- dbGetQuery(con, qstr)
colnames(fg_pct_p) <- c("Conference", "Team", "Season", "Field Goal Percentage", "Playoff Wins")
fg_pct_p_tbl <-
  xtable(fg_pct_p,
         caption="NBA Teams.Teams with less than 10th Percentile FG Percent, made Playoffs",
         label="tab: below_10th_percentile_fg_pct_playoffs")

xxx <- print(fg_pct_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_fg_pct_playoffs.tex") )

## FT_PCT
# Above 90th percentile no playoffs
ft_pct_90 <- quantile(data$FT_PCT, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FT_PCT, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FT_PCT, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE FT_PCT > ", ft_pct_90,
  " ORDER BY SEASON_ID ASC"
)

ft_pct_no_p <- dbGetQuery(con, qstr)
colnames(ft_pct_no_p) <- c("Conference", "Team", "Season", "Free Throw Percentage", "Regular Season Wins")
ft_pct_no_p_tbl <-
  xtable(ft_pct_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile FT Percent, missed Playoffs",
         label="tab: above_90th_percentile_ft_pct_no_playoffs")

xxx <- print(ft_pct_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_ft_pct_noplayoffs.tex") )

# Below Mean Made Playoffs
ft_pct_10 <- quantile(data$FT_PCT, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FT_PCT, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, FT_PCT, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE FT_PCT < ", ft_pct_10,
  " ORDER BY SEASON_ID ASC"
)

ft_pct_p <- dbGetQuery(con, qstr)
colnames(ft_pct_p) <- c("Conference", "Team", "Season", "Free Throw Percentage", "Playoff Wins")
ft_pct_p_tbl <-
  xtable(ft_pct_p,
         caption="NBA Teams.Teams with less than 10th Percentile FT Percent, made Playoffs",
         label="tab: below_10th_percentile_ft_pct_playoffs")

xxx <- print(ft_pct_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_ft_pct_playoffs.tex") )

## OREB
# Above 90th percentile no playoffs
oreb_90 <- quantile(data$OREB, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, OREB, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, OREB, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE OREB > ", oreb_90,
  " ORDER BY SEASON_ID ASC"
)

oreb_no_p <- dbGetQuery(con, qstr)
colnames(oreb_no_p) <- c("Conference", "Team", "Season", "Offensive Rebounds", "Regular Season Wins")
oreb_no_p_tbl <-
  xtable(oreb_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Offensive Rebounds, missed Playoffs",
         label="tab: above_90th_percentile_orebs_no_playoffs")

xxx <- print(oreb_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_orebs_noplayoffs.tex") )

# Below Mean Made Playoffs
oreb_10 <- quantile(data$OREB, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, OREB, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, OREB, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE OREB < ", oreb_10,
  " ORDER BY SEASON_ID ASC"
)

oreb_p <- dbGetQuery(con, qstr)
colnames(oreb_p) <- c("Conference", "Team", "Season", "Offensive Rebounds", "Playoff Wins")
oreb_p_tbl <-
  xtable(oreb_p,
         caption="NBA Teams.Teams with less than 10th Percentile Offensive Rebounds, made Playoffs",
         label="tab: below_10th_percentile_orebs_playoffs")

xxx <- print(oreb_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_orebs_playoffs.tex") )

## PTS
# Above 90th percentile no playoffs
pts_90 <- quantile(data$PTS, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, PTS, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, PTS, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE PTS > ", pts_90,
  " ORDER BY SEASON_ID ASC"
)

pts_no_p <- dbGetQuery(con, qstr)
colnames(pts_no_p) <- c("Conference", "Team", "Season", "Points", "Regular Season Wins")
pts_no_p_tbl <-
  xtable(pts_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Points, missed Playoffs",
         label="tab: above_90th_percentile_pts_no_playoffs")

xxx <- print(pts_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_points_noplayoffs.tex") )

# Below Mean Made Playoffs
pts_10 <- quantile(data$PTS, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, PTS, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, PTS, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE PTS < ", pts_10,
  " ORDER BY SEASON_ID ASC"
)

pts_p <- dbGetQuery(con, qstr)
colnames(pts_p) <- c("Conference", "Team", "Season", "Points", "Playoff Wins")
pts_p_tbl <-
  xtable(pts_p,
         caption="NBA Teams.Teams with less than 10th Percentile Points, made Playoffs",
         label="tab: below_10th_percentile_pts_playoffs")

xxx <- print(pts_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_points_playoffs.tex") )

## REB
# Above 90th percentile no playoffs
reb_90 <- quantile(data$REB, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, REB, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, REB, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE REB > ", reb_90,
  " ORDER BY SEASON_ID ASC"
)

reb_no_p <- dbGetQuery(con, qstr)
colnames(reb_no_p) <- c("Conference", "Team", "Season", "Rebounds", "Regular Season Wins")
reb_no_p_tbl <-
  xtable(reb_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Rebounds, missed Playoffs",
         label="tab: above_90th_percentile_rebs_no_playoffs")

xxx <- print(reb_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_rebs_noplayoffs.tex") )

# Below Mean Made Playoffs
reb_10 <- quantile(data$REB, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, REB, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, REB, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE REB < ", reb_10,
  " ORDER BY SEASON_ID ASC"
)

reb_p <- dbGetQuery(con, qstr)
colnames(reb_p) <- c("Conference", "Team", "Season", "Rebounds", "Playoff Wins")
reb_p_tbl <-
  xtable(reb_p,
         caption="NBA Teams.Teams with less than 10th Percentile Rebounds, made Playoffs",
         label="tab: below_10th_percentile_rebs_playoffs")

xxx <- print(reb_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_rebs_playoffs.tex") )

## STL
# Above 90th percentile no playoffs
stl_90 <- quantile(data$STL, probs = .90)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, STL, RS_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, STL, RS_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'False') AS temp",
  " WHERE STL > ", stl_90,
  " ORDER BY SEASON_ID ASC"
)

stl_no_p <- dbGetQuery(con, qstr)
colnames(stl_no_p) <- c("Conference", "Team", "Season", "Steals", "Regular Season Wins")
stl_no_p_tbl <-
  xtable(stl_no_p,
         caption="NBA Teams.Teams with more than 90th Percentile Steals, missed Playoffs",
         label="tab: above_90th_percentile_steals_no_playoffs")

xxx <- print(stl_no_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_above_90th_steals_noplayoffs.tex") )

# Below Mean Made Playoffs
stl_10 <- quantile(data$STL, probs = .10)
qstr <- paste0(
  "SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, STL, P_WINS FROM", 
  " (SELECT CONF, TEAM_ABBREVIATION, SEASON_ID, STL, P_WINS FROM ", xtableName, 
  " WHERE PLAYOFF = 'True') AS temp",
  " WHERE STL < ", stl_10,
  " ORDER BY SEASON_ID ASC"
)

stl_p <- dbGetQuery(con, qstr)
colnames(stl_p) <- c("Conference", "Team", "Season", "Steals", "Playoff Wins")
stl_p_tbl <-
  xtable(stl_p,
         caption="NBA Teams.Teams with less than 10th Percentile Steals, made Playoffs",
         label="tab: below_10th_percentile_steals_playoffs")

xxx <- print(stl_p_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_below_10th_steals_playoffs.tex") )

## Individual Variables Scatterplots

skip_list <- c("TEAM_ABBREVIATION", "SEASON_ID", "DREB", "FG3A", "FG3M", "FGA", "FGM",
               "FTA", "FTM", "OREB", "CONF", "PLAYOFF", "P_WINS")
# Regular Season
for (i in colnames(data)) {
  if(i %in% skip_list){
    next
  }
  else if (i == "RS_WINS"){
    next
  }
  else{
    png_title <- paste0("myPlot_Scatterplot_", i, "_vs_Reg_Season_wins.png")
    print(png_title)
    png(
      file.path( "_assets", png_title),
      width=1080,
      height=800,
      pointsize=20
    )
    par(mfrow = c(1,1))
    plot(data$RS_WINS, data[ , i], main = paste0(i, " vs. Regular Season Wins"),
         xlab = "Regular Season Wins", ylab = paste0(i), col = '#91171b', pch = 16)
    dev.off()
  }
}


# Playoffs

for (i in colnames(data)) {
  if(i %in% skip_list){
    next
  }
  else{
    png_title <- paste0("myPlot_Scatterplot_", i, "_vs_Playoff_wins.png")
    print(png_title)
    png(
      file.path( "_assets", png_title),
      width=1080,
      height=800,
      pointsize=20
    )
    par(mfrow = c(1,1))
    plot(data$P_WINS, data[ , i], main = paste0(i, " vs. Playoff Wins"),
         xlab = "Playoff Wins", ylab = paste0(i), col = '#91171b', pch = 16)
    dev.off()
  }
}

### Poisson Regression

## Regular Season Wins

# Model Creation
rs_model1 <- glm(data = data, RS_WINS ~ AST + BLK + DREB + FG3_PCT + FG_PCT + PTS, family = "poisson")
summary(rs_model1)

rs_model2 <- step(rs_model1, direction = "backward", trace = 0)
summary(rs_model2)

# Model Comparison
anova(rs_model1, rs_model2, test = "Chisq")

best_rs_model <- glm(data = data, RS_WINS ~ AST + BLK + DREB + FG3_PCT + FG_PCT + PTS, family = "poisson")

#Goodness of Fit
cov.rs <- vcovHC(best_rs_model, type = "HC0")
rs_std.err <- sqrt(diag(cov.rs))

rs_r.est <- cbind(Estimate=coef(best_rs_model), "Robust SE" = rs_std.err,
                  "PR(>|z|)" = 2 * pnorm(abs(coef(best_rs_model)/rs_std.err), lower.tail = FALSE),
                  LL = coef(best_rs_model) - 1.96 * rs_std.err,
                  UL = coef(best_rs_model) + 1.96 * rs_std.err)
rs_r.est

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6), ~ exp(x7)),
                 coef(best_rs_model), cov.rs)

rs_r.est <- data.frame(rs_r.est)
rs_r.est <- cbind(Coefficients = rownames(rs_r.est), rs_r.est)
rownames(rs_r.est) <- NULL
rs_pvalues <- rs_r.est[, 4]
rs_coefs <- rs_r.est[, 1]
rs_rexp.est <- exp(rs_r.est[, -c(1, 3, 4)])
rs_rexp.est <- add_column(rs_rexp.est, "Robust SE" = s, .after = "Estimate")
rs_rexp.est <- add_column(rs_rexp.est, "P-Values" = rs_pvalues, .before = "LL")
rs_rexp.est <- add_column(rs_rexp.est, "Coefficients" = rs_coefs, .before = "Estimate")
rs_rexp.est <- rs_rexp.est[ , c(1:4)]

rs_sum_tbl <-
  xtable(rs_rexp.est,
         caption="Poisson Regression Model: Predicting Regular Season Wins Summary",
         label="tab: summary_regression_rs_wins")

xxx <- print(rs_sum_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_regular_season_wins_regression_summary.tex") )

best_rs_model_fit <- with(best_rs_model, cbind(res.deviance = deviance, df = df.residual,
                                               p = pchisq(deviance, df.residual,
                                                          lower.tail = FALSE)))

best_rs_model_fit <- data.frame(best_rs_model_fit)
colnames(best_rs_model_fit) <- c("Residual Deviance", "DF", "P-Value")

rs_chisq_tbl <-
  xtable(best_rs_model_fit,
         caption="Poisson Regression Model: Goodness of Fit Results - Regular Season Wins",
         label="tab: fit_results_regression_rs_wins")

xxx <- print(rs_chisq_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_fit_results_regular_season_wins.tex") )

# 10-Fold CV & RMSE
set.seed(50)
rs_cv_results <- data.frame(sqrt(cv.glm(data = data, glmfit = best_rs_model, K = 10)$delta))
rs_cv_results <- t(rs_cv_results)
rownames(rs_cv_results) <- NULL
colnames(rs_cv_results) <- c("RMSE", "Bias Corrected RMSE")

rs_cv_model_tbl <-
  xtable(rs_cv_results,
         caption="Poisson Regression Model: CV Regular Season Wins Results",
         label="tab: results_cv_rs_wins")

xxx <- print(rs_cv_model_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_results_cv_rs_wins.tex") )

## Playoff Wins

# Model Creation
p_model1 <- glm(data = data, P_WINS ~ AST + FG3_PCT + FG_PCT + PTS + RS_WINS, family = "poisson")
summary(p_model1)

p_model2 <- step(p_model1, direction = "backward", trace = 0)
summary(p_model2)

# Model Comparison
anova(p_model1, p_model2, test = "Chisq")

best_p_model <- glm(data = data, P_WINS ~ AST + FG3_PCT + FG_PCT + PTS + RS_WINS, family = "poisson")

#Goodness of Fit
cov.p <- vcovHC(best_p_model, type = "HC0")
p_std.err <- sqrt(diag(cov.p))

p_r.est <- cbind(Estimate=coef(best_p_model), "Robust SE" = p_std.err,
                 "PR(>|z|)" = 2 * pnorm(abs(coef(best_p_model)/p_std.err), lower.tail = FALSE),
                 LL = coef(best_p_model) - 1.96 * p_std.err,
                 UL = coef(best_p_model) + 1.96 * p_std.err)
p_r.est

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6)),
                 coef(best_p_model), cov.p)

p_r.est <- data.frame(p_r.est)
p_r.est <- cbind(Coefficients = rownames(p_r.est), p_r.est)
rownames(p_r.est) <- NULL
p_pvalues <- p_r.est[, 4]
p_coefs <- p_r.est[, 1]
p_rexp.est <- exp(p_r.est[, -c(1, 3, 4)])
p_rexp.est <- add_column(p_rexp.est, "Robust SE" = s, .after = "Estimate")
p_rexp.est <- add_column(p_rexp.est, "P-Values" = p_pvalues, .before = "LL")
p_rexp.est <- add_column(p_rexp.est, "Coefficients" = p_coefs, .before = "Estimate")
p_rexp.est <- p_rexp.est[ , c(1:4)]

p_sum_tbl <-
  xtable(p_rexp.est,
         caption="Poisson Regression Model: Predicting Playoff Wins Summary",
         label="tab: summary_regression_p_wins")

xxx <- print(p_sum_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_playoff_wins_regression_summary.tex") )

best_p_model_fit <- with(best_p_model, cbind(res.deviance = deviance, df = df.residual,
                                             p = pchisq(deviance, df.residual,
                                                        lower.tail = FALSE)))

best_p_model_fit <- data.frame(best_p_model_fit)
colnames(best_p_model_fit) <- c("Residual Deviance", "DF", "P-Value")

p_chisq_tbl <-
  xtable(best_p_model_fit,
         caption="Poisson Regression Model: Goodness of Fit Results - Playoff Wins",
         label="tab: fit_results_regression_p_wins")

xxx <- print(p_chisq_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_fit_results_playoff_wins.tex") )

# 10-Fold CV & RMSE
set.seed(50)
p_cv_results <- data.frame(sqrt(cv.glm(data = data, glmfit = best_p_model, K = 10)$delta))
p_cv_results <- t(p_cv_results)
rownames(p_cv_results) <- NULL
colnames(p_cv_results) <- c("RMSE", "Bias Corrected RMSE")

p_cv_model_tbl <-
  xtable(p_cv_results,
         caption="Poisson Regression Model: CV Playoff Wins Results",
         label="tab: results_cv_p_wins")

xxx <- print(p_cv_model_tbl, include.rownames=FALSE, table.placement="H")

writeLines( xxx, file.path("_assets", "myTable_results_cv_p_wins.tex") )

dbDisconnect(con)
