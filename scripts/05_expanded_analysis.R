library(rio) # file i/o
library(data.table)
library(miceadds) # for glm.cluster
library(margins) # AME
library(sandwich)
library(glue) # for easy string literals in model specification
library(modelsummary)

####
# Load data and define paths
####
inpath <- 'data/comment_data.csv'
outpath_perf <- 'output/table_performance.xlsx'
outpath_allnba_allstar <- 'output/table_allnba_allstar_t_1.xlsx'
outpath_allnba_allnba <- 'output/table_allnba_allnba_t_1.xlsx'

dta <- fread(inpath)
dta[, cyear := as.character(cyear)]



####
# Prepare variable lists
####

base_allstar_vars <- c('all_star','all_star_t_1', 'all_star_cum_t_1')
confinv <- c("height", "pos", "age_0", "age_0_2", "Black", "time", "time_2", "cyear")
confvar <- c("pts_std_t_1", "ast_std_t_1", "trb_std_t_1", "min_played_std_t_1", "playoffs_t_1", "win_std_t_1", "bigm_std_t_1")
perft <- c("pts_std", "ast_std", "trb_std")
situt <- c("min_played_std", "playoffs", "win_std", "bigm_std")
varhi <- c("pts_cum_t_1", "ast_cum_t_1", "trb_cum_t_1", "min_played_cum_t_1", "playoffs_cum_t_1", "win_cum_t_1", "bigm_cum_t_1")
efficiency_vars <- c("tsp_std", "tsp_std_t_1", "tsp_cum_t_1")
championship_vars <- c('champions_t_1', 'finals_t_1', 'finals_conf_t_1', 'semifinals_conf_t_1', 'champions_cum_t_1', 'finals_cum_t_1', 'finals_conf_cum_t_1', 'semifinals_conf_cum_t_1')
defensive_vars <- c("dbpm_std_t_1", "dbpm_std", "dbpm_cum_t_1", "blk_std", "stl_std", "blk_std_t_1", "stl_std_t_1", "blk_cum_t_1", "stl_cum_t_1", "all_defensive", "all_defensive_t_1")
bpm_vars <- c("bpm_std", "bpm_std_t_1", "bpm_cum_t_1")

allvars_comm <- c(base_allstar_vars, confinv, confvar, perft, situt, varhi, efficiency_vars, championship_vars, defensive_vars, bpm_vars)



####
# Empty lists to store logistic fits and AME fits
####

fits_perf <- list()
fits_allnba_allstar <- list()
fits_allnba_allnba <- list()
ames_perf <- list()
ames_allnba_allstar <- list()
ames_allnba_allnba <- list()

####
# Model 6 (predicting All-Star)
####


# M6 (cumulative), unaltered

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$orig <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$orig <- margins(fits_perf$orig$glm_res, vcov = fits_perf$orig$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M6 (cumulative), plus efficiency (TSpct)

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$effic <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$effic <- margins(fits_perf$effic$glm_res, vcov = fits_perf$effic$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M6 (cumulative), plus championships

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    championship_vars
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$champ <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$champ <- margins(fits_perf$champ$glm_res, vcov = fits_perf$champ$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M6 (cumulative), plus defensive

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    defensive_vars
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$def <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$def <- margins(fits_perf$def$glm_res, vcov = fits_perf$def$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M6 (cumulative), plus BPM

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    bpm_vars
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$bpm <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$bpm <- margins(fits_perf$bpm$glm_res, vcov = fits_perf$bpm$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M6 (cumulative), plus efficiency, defensive, championships, bmp

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars, defensive_vars, championship_vars, bpm_vars
)
model_form <- as.formula(glue(
    "all_star ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_star',covars)
complete <- complete.cases(dta[, ..allvars])

fits_perf$all4 <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_perf$all4 <- margins(fits_perf$all4$glm_res, vcov = fits_perf$all4$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


summaries_perf <- lapply(names(fits_perf), function(m){
    res <- list(
        tidy = as.data.frame(get_estimates(ames_perf[[m]])),
        glance = get_gof(fits_perf[[m]]$glm_res)
    )
    class(res) <- "modelsummary_list"
    return(res)
})
names(summaries_perf) <- names(fits_perf)

modelsummary(summaries_perf,
    output=outpath_perf, statistic = "conf.int",
    fmt = fmt_statistic(estimate=4, conf.int=3),
    stars = TRUE
)




#####
# Model 7: All-NBA predicted by past all-star
#####

# M7 (cumulative), unaltered

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$orig <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$orig <- margins(fits_allnba_allstar$orig$glm_res, vcov = fits_allnba_allstar$orig$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M7 (cumulative), plus efficiency (TSpct)

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$effic <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$effic <- margins(fits_allnba_allstar$effic$glm_res, vcov = fits_allnba_allstar$effic$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)



# M7 (cumulative), plus defensive

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    defensive_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$def <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$def <- margins(fits_allnba_allstar$def$glm_res, vcov = fits_allnba_allstar$def$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)

# M7 (cumulative), plus championships

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    championship_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$champ <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$champ <- margins(fits_allnba_allstar$champ$glm_res, vcov = fits_allnba_allstar$champ$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M7 (cumulative), plus BPM

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    bpm_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$bpm <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$bpm <- margins(fits_allnba_allstar$bpm$glm_res, vcov = fits_allnba_allstar$bpm$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M7 (cumulative), plus efficiency, defensive, championships, bmp

covars <- c(
    'all_star_t_1', 'all_star_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars, defensive_vars, championship_vars, bpm_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allstar$all4 <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allstar$all4 <- margins(fits_allnba_allstar$all4$glm_res, vcov = fits_allnba_allstar$all4$vcov,
    variables = c("all_star_t_1","all_star_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)



summaries_allnba_allstar <- lapply(names(fits_allnba_allstar), function(m){
    res <- list(
        tidy = as.data.frame(get_estimates(ames_allnba_allstar[[m]])),
        glance = get_gof(fits_allnba_allstar[[m]]$glm_res)
    )
    class(res) <- "modelsummary_list"
    return(res)
})
names(summaries_allnba_allstar) <- names(fits_allnba_allstar)

modelsummary(summaries_allnba_allstar,
    output=outpath_allnba_allstar, statistic = "conf.int",
    fmt = fmt_statistic(estimate=4, conf.int=3),
    stars = TRUE
)

#####
# Model 8: All-NBA predicted by past All-NBA
#####

# M8 (cumulative), unaltered

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$orig <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$orig <- margins(fits_allnba_allnba$orig$glm_res, vcov = fits_allnba_allnba$orig$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)


# M8 (cumulative), plus efficiency (TSpct)

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$effic <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$effic <- margins(fits_allnba_allnba$effic$glm_res, vcov = fits_allnba_allnba$effic$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)



# M8 (cumulative), plus defensive

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    defensive_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$def <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$def <- margins(fits_allnba_allnba$def$glm_res, vcov = fits_allnba_allnba$def$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)



# M8 (cumulative), plus championships

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    championship_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$champ <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$champ <- margins(fits_allnba_allnba$champ$glm_res, vcov = fits_allnba_allnba$champ$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)




# M8 (cumulative), plus BPM

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    bpm_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$bpm <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$bpm <- margins(fits_allnba_allnba$bpm$glm_res, vcov = fits_allnba_allnba$bpm$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)



# M8 (cumulative), plus efficiency, defensive, championships, bmp

covars <- c(
    'all_nba_t_1', 'all_nba_cum_t_1',
    confinv, confvar, perft, situt, varhi,
    efficiency_vars, defensive_vars, championship_vars, bpm_vars
)
model_form <- as.formula(glue(
    "all_nba ~ ",
    glue_collapse(covars, sep = " + ")
))

allvars <- c('all_nba',covars)
complete <- complete.cases(dta[, ..allvars])

fits_allnba_allnba$all4 <- glm.cluster(model_form, data = dta[complete], family = "binomial", cluster = "player_id")
ames_allnba_allnba$all4 <- margins(fits_allnba_allnba$all4$glm_res, vcov = fits_allnba_allnba$all4$vcov,
    variables = c("all_nba_t_1","all_nba_cum_t_1"), data = dta[complete],
    change = c(0, 1)
)




# make the table
summaries_allnba_allnba <- lapply(names(fits_allnba_allnba), function(m){
    res <- list(
        tidy = as.data.frame(get_estimates(ames_allnba_allnba[[m]])),
        glance = get_gof(fits_allnba_allnba[[m]]$glm_res)
    )
    class(res) <- "modelsummary_list"
    return(res)
})
names(summaries_allnba_allnba) <- names(fits_allnba_allnba)

modelsummary(summaries_allnba_allnba,
    output=outpath_allnba_allnba, statistic = "conf.int",
    fmt = fmt_statistic(estimate=4, conf.int=3),
    stars = TRUE
)




# save all the models to disk
export(fits_perf, file = 'models/fits_perf.rds')
export(fits_allnba_allstar, file = 'models/fits_allnba_allstar.rds')
export(fits_allnba_allnba, file = 'models/fits_allnba_allnba.rds')
export(ames_perf, file = 'models/ames_perf.rds')
export(ames_allnba_allstar, file = 'models/ames_allnba_allstar.rds')
export(ames_allnba_allnba, file = 'models/ames_allnba_allnba.rds')


base_allstar_vars <- c('all_star','all_star_t_1', 'all_star_cum_t_1')
confinv <- c("height", "pos", "age_0", "age_0_2", "Black", "time", "time_2", "cyear")
confvar <- c("pts_std_t_1", "ast_std_t_1", "trb_std_t_1", "min_played_std_t_1", "playoffs_t_1", "win_std_t_1", "bigm_std_t_1")
perft <- c("pts_std", "ast_std", "trb_std")
situt <- c("min_played_std", "playoffs", "win_std", "bigm_std")
varhi <- c("pts_cum_t_1", "ast_cum_t_1", "trb_cum_t_1", "min_played_cum_t_1", "playoffs_cum_t_1", "win_cum_t_1", "bigm_cum_t_1")
efficiency_vars <- c("tsp_std", "tsp_std_t_1", "tsp_cum_t_1")
championship_vars <- c('champions_t_1', 'finals_t_1', 'finals_conf_t_1', 'semifinals_conf_t_1', 'champions_cum_t_1', 'finals_cum_t_1', 'finals_conf_cum_t_1', 'semifinals_conf_cum_t_1')
defensive_vars <- c("dbpm_std_t_1", "dbpm_std", "dbpm_cum_t_1", "blk_std", "stl_std", "blk_std_t_1", "stl_std_t_1", "blk_cum_t_1", "stl_cum_t_1", "all_defensive", "all_defensive_t_1")
bpm_vars <- c("bpm_std", "bpm_std_t_1", "bpm_cum_t_1")


# html output
year_coef_names <- glue("year {1986:2016}")
names(year_coef_names) <- glue("cyear{1986:2015}")
coef_names <- c(
    "(Intercept)"      = "(Intercept)",
    "all_star"          = "All-Star",
    "all_star_t_1"      = "All-Star (t-1)",
    "all_star_cum_t_1"  = "All-Star (cum.)",
    "all_nba"           = "All-NBA",
    "all_nba_t_1"       = "All-NBA (t-1)",
    "all_nba_cum_t_1"   = "All-NBA (cum.)",
    "height"           = 'Height',
    "Black"            = "Black",
    "age_0"            = "Starting age",
    "age_0_2"          = "(Starting age)^2",
    "time"             = "Years in NBA",
    "time^2"           = "(Years in NBA)^2",
    "posForward (Power/Small)" = "Forward (ref. center)",
    "posGuard (Point/Shooting)" = "Guard (ref. center)",
    "min_played_std"    = "Minutes played",
    "min_played_std_t_1"= "Minutes played (t-1)",
    "min_played_cum_t_1"= "Minutes played (cum.)",
    "pts_std"          = "Points / 36 min.",
    "pts_std_t_1"      = "Points / 36 min. (t-1)",
    "pts_cum_t_1"      = "Points / 36 min. (cum.)",
    "ast_std"          = "Assists / 36 min.",
    "ast_std_t_1"      = "Assists / 36 min. (t-1)",
    "ast_cum_t_1"      = "Assists / 36 min. (cum.)",
    "trb_std"          = "Rebounds / 36 min.",
    "trb_std_t_1"      = "Rebounds / 36 min. (t-1)",
    "trb_cum_t_1"      = "Rebounds / 36 min. (cum.)",
    "playoffs"         = "Playoffs",
    "playoffs_t_1"     = "Playoffs (t-1)",
    "playoffs_cum"     = "Playoffs (cum.)",
    "win_std"          = "Win %",
    "win_std_t_1"      = "Win % (t-1)",
    "win_cum_t_1"      = "Win % (cum)",
    "bigm_std"         = "Big market",
    "bigm_std_t_1"     = "Big market (t-1)",
    "bigm_cum_t_1"     = "Big market (cum)",
    "tsp_std"          = "True shooting %",
    "tsp_std_t_1"      = "True shooting % (t-1)",
    "tsp_cum_t_1"      = "True shooting % (cum.)",
    "champions"        = "Champions",
    "champions_t_1"    = "Champions (t-1)",
    "champions_cum"    = "Champions (cum.)",
    "finals"           = "Finals",
    "finals_t_1"       = "Finals (t-1)",
    "finals_cum"       = "Finals (cum.)",
    "finals_conf"      = "Conf. finals",
    "finals_conf_t_1"  = "Conf. finals (t-1)",
    "finals_conf_cum"  = "Conf. finals (cum.)",
    "semifinals_conf"    = "Conf. semifinals",
    "semifinals_conf_t_1"= "Conf. semifinals (t-1)",
    "semifinals_conf_cum"= "Conf. semifinals (cum.)",
    "blk_std"          = "Blocks / 36 min.",
    "blk_std_t_1"      = "Blocks / 36 min. (t-1)",
    "blk_cum_t_1"      = "Blocks / 36 min. (cum.)",
    "stl_std"          = "Steals / 36 min.",
    "stl_std_t_1"      = "Steals / 36 min. (t-1)",
    "stl_cum_t_1"      = "Steals / 36 min. (cum.)",
    "dbpm_std"         = "Defensive BPM",
    "dbpm_std_t_1"     = "Defensive BPM (t-1)",
    "dbpm_cum_t_1"     = "Defensive BPM (cum.)",
    "all_defensive"    = "All-Defensive",
    "all_defensive_t_1"= "All-Defensive (t-1)",
    "all_defensive_cum"= "All-Defensive (cum.)",
    "bpm_std"          = "BPM",
    "bpm_std_t_1"      = "BPM (t-1)",
    "bpm_cum_t_1"      = "BPM (cum.)"
)

# full summaries for appendix
modelsummary(
    lapply(fits_perf, function(x){x$glm_res}),
    vcov = lapply(fits_perf, function(x){x$vcov}),
    output = 'output/summary_performance.html',
    coef_map = c(coef_names, year_coef_names)
)
modelsummary(
    lapply(fits_allnba_allstar, function(x){x$glm_res}),
    vcov = lapply(fits_allnba_allstar, function(x){x$vcov}),
    output = 'output/summary_allnba_allstar.html',
    coef_map = c(coef_names, year_coef_names)
)
modelsummary(
    lapply(fits_allnba_allnba, function(x){x$glm_res}),
    vcov = lapply(fits_allnba_allnba, function(x){x$vcov}),
    output = 'output/summary_allnba_allnba.html',
    coef_map = c(coef_names, year_coef_names)
)

# table for paper
fits_compare <- list(
    allstar_allstar = fits_perf$all4,
    allnba_allstar = fits_allnba_allstar$all4,
    allnba_allnba = fits_allnba_allnba$all4
)
modelsummary(
    lapply(fits_compare, function(x){x$glm_res}),
    vcov = lapply(fits_compare, function(x){x$vcov}),
    output = 'output/summary_compare.docx',
    coef_map = coef_names,
    statistic = "conf.int",
    fmt = fmt_statistic(estimate=4, conf.int=3),
    stars = TRUE
)

