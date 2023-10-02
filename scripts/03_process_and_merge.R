# Process game logs and merge with player data

library(data.table)
library(lubridate)

#######
# Prepare data
#######

inpath_player <- 'data/player_data.csv' # player-level
inpath_game <- 'data/game_data.csv' # game logs
inpath_allstar_dates <- 'data/allstar_dates.csv'
inpath_bkvl <- 'data/andata.csv' # used for race 
outpath <- 'data/comment_data.csv'

# from BKVL: Big-market teams
# (leaving NYN and SFW out, which don't exist in the−0.231
# time frame of the data)
bigm_teams <- c(
    "ATL","BOS","BRK","CHI","DAL","GSW","HOU",
    "LAC","LAL","NJN","NYK","PHI","WAS","WSB"
)

# a cumulative sum function for aggregating stats below
cumsum0 <- function(x){
    # a version of cumsum that treats NA as 0
    x[is.na(x)] <- 0
    cumsum(x)
}


###
# Begin with game logs
###

# load all the game logs
game_data <- fread(inpath_game)
game_data[, date := ymd(date_game)]
setorder(game_data, player_id, date)
setkey(game_data, player_id, date)

# flag big-market
game_data[, bigm := team_id %in% bigm_teams]

# we'll need the all-star dates
allstar_dates <- fread(inpath_allstar_dates)

# set the "All-Star date" for 1999 as the median date of all of the games
# (we need this to calculate a window to look at stats over -- even though
# there was no all-star game in 1999, the 2000 game needs last-year stats)
d <- allstar_dates$allstar_date
year(d) <- 1999
allstar_dates <- rbind(
    allstar_dates,
    data.table(allstar_date = median(d))
)
# set a key to sort
setkey(allstar_dates, allstar_date)


# get intervals per season by All-Star
# (between 14 and 350 days before the allstar game)
allstar_dates[, allstar_date := ymd(allstar_date)]
allstar_dates[, as_window_start := allstar_date - days(350)]
allstar_dates[, as_window_end := allstar_date - days(14)]
allstar_dates[, as_window_season := as.integer(year(allstar_date))]


# find games that fall into pre-All-Star election window for each season
game_data[, as_window_season := as.integer(NA)]
for(dseason in allstar_dates$as_window_season){
    as_window_interval <- allstar_dates[as_window_season == dseason, .(as_window_start, as_window_end)]
    game_data[date > as_window_interval$as_window_start & date < as_window_interval$as_window_end, as_window_season := dseason]
}

# get wins
game_data[, win := as.numeric(substr(game_result,1,1) == "W")]

##
# aggregate stats
##

# The following switch (TRUE/FALSE) decides how stats over the target
# window are calculated.
# If FALSE (default), use the same method as basketball-reference.com:
# e.g. sum points over all relevant games and minutes played over all
# the relevant games and take their ratio times 36.
# If TRUE, use the methdon from BKVL:
# e.g. first calculate points per minute played for each relevant game,
# then take the mean of those values.
if(FALSE){
    # Following BKVL,
    # calculate the rate per game and then average across games
    game_data[, pts_std := mean(36 * pts / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, ast_std := mean(36 * ast / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, trb_std := mean(36 * trb / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, blk_std := mean(36 * blk / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, stl_std := mean(36 * stl / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, fga_std := mean(36 * fga / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, fta_std := mean(36 * fta / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, tov_std := mean(36 * tov / mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    # true shooting percentage
    game_data[, tsp_std := mean(pts / (2 * (fga + (0.44 * fta))), na.rm = TRUE), by = .(player_id, as_window_season)]
}else{
    # Consistent with basketball-reference.com, 
    # calculate the rate across the whole window
    game_data[, pts_std := 36 * sum(pts, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, ast_std := 36 * sum(ast, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, trb_std := 36 * sum(trb, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, blk_std := 36 * sum(blk, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, stl_std := 36 * sum(stl, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, fga_std := 36 * sum(fga, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, fta_std := 36 * sum(fta, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    game_data[, tov_std := 36 * sum(tov, na.rm=TRUE) / sum(mp, na.rm = TRUE), by = .(player_id, as_window_season)]
    # true shooting percentage
    game_data[, tsp_std := sum(pts, na.rm=TRUE) / (2 * (sum(fga, na.rm=TRUE) + (0.44 * sum(fta, na.rm=TRUE)))), by = .(player_id, as_window_season)]
}

# wins and big market
game_data[, win_std := mean(win, na.rm = TRUE), by = .(player_id, as_window_season)]
game_data[, bigm_std := mean(bigm, na.rm = TRUE), by = .(player_id, as_window_season)]
# total minutes played
game_data[, min_played_std := mean(mp, na.rm=TRUE), by = .(player_id, as_window_season)]

# career-cumulative stats at each game
game_data[, pts_cum := 36 * cumsum0(pts) / cumsum0(mp), by = player_id]
game_data[, ast_cum := 36 * cumsum0(ast) / cumsum0(mp), by = player_id]
game_data[, trb_cum := 36 * cumsum0(trb) / cumsum0(mp), by = player_id]
game_data[, blk_cum := 36 * cumsum0(blk) / cumsum0(mp), by = player_id]
game_data[, stl_cum := 36 * cumsum0(stl) / cumsum0(mp), by = player_id]
game_data[, fga_cum := 36 * cumsum0(fga) / cumsum0(mp), by = player_id]
game_data[, fta_cum := 36 * cumsum0(fta) / cumsum0(mp), by = player_id]
game_data[, tov_cum := 36 * cumsum0(tov) / cumsum0(mp), by = player_id]
# true shooting percentage
game_data[, tsp_cum := cumsum0(pts) / (2 * (cumsum0(fga) + (0.44 * cumsum0(fta)))), by = player_id]

# wins and big market
game_data[, win_cum := cumsum0(win)/(1:.N), by = .(player_id)]
game_data[, bigm_cum := cumsum0(bigm)/(1:.N), by = .(player_id)]

# collapse down to one row per season
player_data_std <- unique(
    game_data[!is.na(as_window_season), .(
        player_id, as_window_season,
        win_std, bigm_std, min_played_std,
        win_cum, bigm_cum,
        pts_std, ast_std, trb_std, blk_std, stl_std, fga_std, fta_std, tov_std, tsp_std, 
        pts_cum, ast_cum, trb_cum, blk_cum, stl_cum, fga_cum, fta_cum, tov_cum, tsp_cum
    )],
    by = c('player_id','as_window_season'),
    fromLast = TRUE # make sure we're getting cum. stats from last game of window
)
setnames(player_data_std, "as_window_season", "season")

###
# Get player race data data (from BKVL)
###

bkvl_data <- fread(inpath_bkvl)
setkey(bkvl_data, player_id, year)

bkvl_data[, Black := (race - 2) * -1]

# one row per player:
race_data <- unique(bkvl_data[,.(player_id, Black)])


###
# Get player advanced stat data merge in other data
###

player_data <- fread(inpath_player)

# merge Black
dta <- merge(race_data, player_data, on = "player_id", all.x = FALSE, all.y = TRUE)

# merge game-calculated stats
dta <- as.data.table(merge.data.frame(dta, player_data_std, on = c('player_id', 'season'), all.x = TRUE, all.y = FALSE))
setkey(dta, player_id, season)



###
# Calculate lags, cumulative stats, and other transformations
###

setorder(dta, player_id, season)

# tenure in league (skipping missing seasons)
dta[, time := 1:.N, by = player_id]
dta[, time_2 := time**2]

# Toggling the following code (TRUE/FALSE) decides how missed seasons
# will be dealth with when considering lagged variables.
# If FALSE (not run, default), then lagged variables after missed seasons
# will be from the previous season in which the player was present.
# If TRUE (do run), then lagged variables after missed seasons will
# be treated as missing data.
if(FALSE){
    # make sure we have rows for any mid-career missing seaasons
    # e.g. injury, played in a different league
    player_careers <- unique(dta[,.(season_first = min(season), season_last = max(season)), by = player_id])
    dta <- setDT(merge.data.frame(
        player_careers[, .(season = season_first:season_last), by = player_id],
        dta,
        all.x = TRUE
    ))
    setkey(dta, player_id, season)
    setorder(dta, player_id, season)
}

# age
dta[, age := pmin(age_rs, age_po, na.rm = TRUE)]
dta[, age_0 := min(age, na.rm=TRUE), by = player_id]
dta[, age_0_2 := age_0**2]
# categorical year variable
dta[, cyear := as.factor(as.character(season))]


# get most common position per player
# (definite cludge. On tie will pick temporally earlier position)
dta[, pos := names(which.max(table(pos_rs))), by = player_id]
# for players who still have multiple positions (<20 of them), pick first one
# (another cludge)
dta$pos[nchar(dta$pos) > 2] <- sapply(strsplit(dta$pos[nchar(dta$pos) > 2],","), function(x){x[[1]]})
# categorize into the three categories used in BKVL
dta[pos == "C", pos := "Center"]
dta[pos %in% c("PF","SF"), pos := "Forward (Power/Small)"]
dta[pos %in% c("PG","SG"), pos := "Guard (Point/Shooting)"]


# convert selections to binary indicators
# (as scraped, they indicate which team a player was on) 
dta[, all_star := as.numeric(all_star > 0)]
dta[, all_nba := as.numeric(all_nba > 0)]
dta[, all_defensive := as.numeric(all_defensive > 0)]
# no all-star in 1999
dta[season == 1999, all_star := NA]

##
# calculate (D)BMP for _approximate_ window 
# average (weighted by number of games) for RS(t) and PO(t-1)
##
# lag games played
dta$g_rs[is.na(dta$g_rs)] <- 0
dta$g_po[is.na(dta$g_po)] <- 0
dta[, g_rs_t_1 := shift(g_rs, 1, fill = 0)]
dta[, g_po_t_1 := shift(g_po, 1, fill = 0)]
dta[, g_std := g_rs + g_po_t_1]
# a zeroed out playoff and games played variable to make calculations right
dta[, bpm_po_zero := ifelse(is.na(bpm_po), 0, bpm_po)]
dta[, dbpm_po_zero := ifelse(is.na(dbpm_po), 0, dbpm_po)]
# calculate weighted average per season
dta[, bpm_std := (bpm_rs*g_rs + shift(bpm_po_zero, 1, fill = 0)*g_po_t_1)/g_std, by = player_id]
dta[, dbpm_std := (dbpm_rs*g_rs + shift(dbpm_po_zero, 1, fill = 0)*g_po_t_1)/g_std, by = player_id]
# calculate cumulative weighted average
dta[, bpm_cum := cumsum0(bpm_rs * g_rs + shift(bpm_po_zero, 1, fill = 0)*g_po_t_1) / cumsum(g_std), by = player_id]
dta[, dbpm_cum := cumsum0(dbpm_rs * g_rs + shift(dbpm_po_zero, 1, fill = 0)*g_po_t_1) / cumsum(g_std), by = player_id]


# one-season lag (prior to all-star at t-1)
dta[, pts_std_t_1 := shift(pts_std, 1), by = player_id]
dta[, ast_std_t_1 := shift(ast_std, 1), by = player_id]
dta[, trb_std_t_1 := shift(trb_std, 1), by = player_id]
dta[, blk_std_t_1 := shift(blk_std, 1), by = player_id]
dta[, stl_std_t_1 := shift(stl_std, 1), by = player_id]
dta[, tsp_std_t_1 := shift(tsp_std, 1), by = player_id]
dta[, bpm_std_t_1 := shift(bpm_std, 1), by = player_id]
dta[, dbpm_std_t_1 := shift(dbpm_std, 1), by = player_id]
dta[, win_std_t_1 := shift(win_std, 1), by = player_id]
dta[, bigm_std_t_1 := shift(bigm_std, 1), by = player_id]
dta[, min_played_std_t_1 := shift(min_played_std, 1), by = player_id]
dta[, champions_t_1 := shift(champions, 1), by = player_id]
dta[, finals_t_1 := shift(finals, 1), by = player_id]
dta[, finals_conf_t_1 := shift(as.numeric(finals_eastern | finals_western), 1), by = player_id]
dta[, semifinals_conf_t_1 := shift(as.numeric(semifinals_eastern | semifinals_western), 1), by = player_id]
dta[, playoffs_t_1 := shift(playoffs, 1), by = player_id]
dta[, all_star_t_1 := shift(all_star, 1), by = player_id]
dta[, all_nba_t_1 := shift(all_nba, 1), by = player_id]
dta[, all_defensive_t_1 := shift(all_defensive, 1), by = player_id]
# special case for the 1999-2000 season, lagged All-Star should come from 1998
for(pid in unique(dta[season %in% c(1998,2000)]$player_id)){
    if(all(c(1998,2000) %in% dta[player_id==pid]$season)){
        dta[player_id == pid & season==2000]$all_star_t_1 <- dta[player_id == pid & season==1998]$all_star
    }
}


# cumulative stats (prior to all-star at t-1)
dta[, pts_cum_t_1 := shift(pts_cum, 1), by = player_id]
dta[, ast_cum_t_1 := shift(ast_cum, 1), by = player_id]
dta[, trb_cum_t_1 := shift(trb_cum, 1), by = player_id]
dta[, blk_cum_t_1 := shift(blk_cum, 1), by = player_id]
dta[, stl_cum_t_1 := shift(stl_cum, 1), by = player_id]
dta[, tsp_cum_t_1 := shift(tsp_cum, 1), by = player_id]
dta[, bpm_cum_t_1 := shift(bpm_cum, 1), by = player_id]
dta[, dbpm_cum_t_1 := shift(dbpm_cum, 1), by = player_id]
dta[, win_cum_t_1 := shift(win_cum, 1), by = player_id]
dta[, bigm_cum_t_1 := shift(bigm_cum, 1), by = player_id]
# (following BKVL, cum. minutes played is average across seasons)
dta[, min_played_cum_t_1 := shift(cumsum0(min_played_std)/(1:.N), 1), by = player_id]
dta[, champions_cum_t_1 := shift(cumsum0(champions)/(1:.N), 1), by = player_id]
dta[, finals_cum_t_1 := shift(cumsum0(finals)/(1:.N), 1), by = player_id]
dta[, finals_conf_cum_t_1 := shift(cumsum0(finals_eastern | finals_western)/(1:.N), 1), by = player_id]
dta[, semifinals_conf_cum_t_1 := shift(cumsum0(semifinals_eastern | semifinals_western)/(1:.N), 1), by = player_id]
dta[, playoffs_cum_t_1 := shift(cumsum0(playoffs)/(1:.N), 1), by = player_id]
dta[, all_star_cum_t_1 := shift(cumsum0(all_star), 1), by = player_id]
dta[, all_nba_cum_t_1 := shift(cumsum0(all_nba), 1), by = player_id]
dta[, all_defensive_cum_t_1 := shift(cumsum0(all_defensive), 1), by = player_id]


###
# Write the whole thing to disk
###
fwrite(dta, file = outpath)