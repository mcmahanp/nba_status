library(rio) # file i/o
library(data.table)
library(miceadds) # for glm.cluster
library(margins) # AME
library(sandwich)
library(glue) # for easy string literals in model specification
library(modelsummary)
library(lme4) # for random effects model

####
# Load data and define paths
####
inpath_comment <- 'data/comment_data.csv'
inpath_bkvl <- 'data/andata.dta'
outpath <- 'output/table3_side_by_side.xlsx'

dta_comm <- fread(inpath_comment)
dta_bkvl <- import(inpath_bkvl)

# make sure `cyear` is character
dta_comm[, cyear := as.character(cyear)]

####
# Reproduce data processing from BKVL
####
team_labels <- attr(dta_bkvl$team,'labels')
pos_labels <- c(
    "Center" = 1, 
    "Forward (Power/Small)" = 2, 
    "Guard (Point/Shooting)" = 3
)

## convert to data table and make factors for `pos` and `team`
dta_bkvl <- data.table(dta_bkvl)
dta_bkvl[, pos := as.factor(names(pos_labels)[pos])]
dta_bkvl[, team := as.factor(names(team_labels)[team])]

# make a categorical version of year
dta_bkvl[, cyear := as.character(year)]

## Reproduce data transformations from 04an-result.do:
# Allstardom
dta_bkvl[, allstar_cnt := sum(allstar_y), by = player_id]
dta_bkvl[sampleC == 1, nobs := seq(.N), by = .(player_id)]

dta_bkvl[player_id == "duncati01", allstar_cnt := 15]
dta_bkvl[player_id == "malonka01", allstar_cnt := 14]
dta_bkvl[player_id == "onealsh01", allstar_cnt := 15]
dta_bkvl[player_id == "jordami01", allstar_cnt := 14]
dta_bkvl[player_id == "robinda01", allstar_cnt := 10]
dta_bkvl[player_id == "wadedw01",  allstar_cnt := 12]
dta_bkvl[player_id == "olajuha01", allstar_cnt := 12]
dta_bkvl[player_id == "ewingpa01", allstar_cnt := 11]
dta_bkvl[player_id == "iversal01", allstar_cnt := 11]
dta_bkvl[player_id == "drexlcl01", allstar_cnt := 10]
dta_bkvl[player_id == "boshch01",  allstar_cnt := 11]

dta_bkvl[, points_tot := sum(pts_sdt_nd), by = player_id]
dta_bkvl[, assist_tot := sum(ast_sdt_nd), by = player_id]
dta_bkvl[, rebound_tot := sum(trb_sdt_nd), by = player_id]

# time-constant features
dta_bkvl[, age_0_2 := age_0**2]
dta_bkvl[, Black := (race - 2) * -1]

# time-varying features
dta_bkvl[, time_2 := time**2]

# Time-constant features by ever all-star
dta_bkvl[, allstar_ev := max(allstar_x), by = player_id]
dta_bkvl[, rank_ev := rank_t > 0, by = player_id]

# create sample variable for restricted sample
dta_bkvl[, sampleD := sampleC & (rank_ev == 1 | allstar_ev == 1)]
# rename for consistency
setnames(dta_bkvl, "allstar_starter_t", "starter_x")

# we'll only be using sampleC so drop unused rows
dta_bkvl <- dta_bkvl[sampleC==1]





####
# Prepare variable lists and subset to complete
# cases in the basic and most complete models
####

base_comm <- c('all_star','all_star_t_1')
confinv_comm <- c("height", "pos", "age_0", "age_0_2", "Black", "time", "time_2", "cyear")
confvar_comm <- c("pts_std_t_1", "ast_std_t_1", "trb_std_t_1", "min_played_std_t_1", "playoffs_t_1", "win_std_t_1", "bigm_std_t_1")
perft_comm <- c("pts_std", "ast_std", "trb_std")
situt_comm <- c("min_played_std", "playoffs", "win_std", "bigm_std")
varhi_comm <- c("pts_cum_t_1", "ast_cum_t_1", "trb_cum_t_1", "min_played_cum_t_1", "playoffs_cum_t_1", "win_cum_t_1", "bigm_cum_t_1")
allvars1_comm <- c(base_comm, confinv_comm)
allvars2_comm <- c(base_comm, "all_star_cum_t_1", confinv_comm, confvar_comm, perft_comm, situt_comm, varhi_comm)

complete_comm1 <- complete.cases(dta_comm[, ..allvars1_comm])
complete_comm2 <- complete.cases(dta_comm[, ..allvars2_comm])


base_comm <- c('allstar_y','allstar_x')
confinv_bkvl <- c("height", "pos", "age_0", "age_0_2", "Black", "time", "time_2", "cyear")
confvar_bkvl <- c("pts_sdt_nd", "ast_sdt_nd", "trb_sdt_nd", "min_played_nd", "playoffs_nd", "win_nd", "bigm_nd")
perft_bkvl <- c("pts_sdt_t", "ast_sdt_t", "trb_sdt_t")
situt_bkvl <- c("min_played_t", "playoffs_t", "win_t", "bigm_t")
varhi_bkvl <- c("pts_sdt_hi", "ast_sdt_hi", "trb_sdt_hi", "min_played_hi", "playoffs_hi", "win_hi", "bigm_hi")
allvars1_bkvl <- c(base_comm, confinv_bkvl)
allvars2_bkvl <- c(base_comm, "allstar_hi", confinv_bkvl, confvar_bkvl, perft_bkvl, situt_bkvl, varhi_bkvl)

# (this should be every case, but just to be consistent:)
complete_bkvl1 <- complete.cases(dta_bkvl[, ..allvars1_bkvl])
complete_bkvl2 <- complete.cases(dta_bkvl[, ..allvars2_bkvl])


####
# Compare the datasets
####

# make a `season` variable in dta_bkvl that tries to correct
# the `year` misalignment in BKVL's data
dta_bkvl[, season := as.integer(ifelse(year == 1998, 2000, year + 1))]

# random comparisons:
# pairs of stats that should more or less match up
# between BKVL's data and comment data
statpairs <- list(
    c("allstar_y", "all_star"),
    c("allstar_x", "all_star_t_1"),
    c("allstar_hi", "all_star_cum_t_1"),
    c("pts_sdt_nd", "pts_std_t_1"),
    c("pts_sdt_t", "pts_std"),
    c("pts_sdt_hi", "pts_cum_t_1"),
    c("ast_sdt_nd", "ast_std_t_1"),
    c("ast_sdt_t", "ast_std"),
    c("ast_sdt_hi", "ast_cum_t_1"),
    c("trb_sdt_nd", "trb_std_t_1"),
    c("trb_sdt_t", "trb_std"),
    c("trb_sdt_hi", "trb_cum_t_1"),
    c("min_played_t", "min_played_std"),
    c("min_played_nd", "min_played_std_t_1"),
    c("min_played_hi", "min_played_cum_t_1"),
    c("playoffs_t", "playoffs"),
    c("playoffs_nd", "playoffs_t_1"),
    c("playoffs_hi", "playoffs_cum_t_1"),
    c('win_t', 'win_std'),
    c('win_nd', 'win_std_t_1'),
    c('win_hi', 'win_cum_t_1')
)
rand_plot <- function(allstar_count = 0){
    # a simple function to check random stats on random 
    # players between the two datasets
    if(allstar_count > 0){
        player_pool <- table(
            dta_comm[complete_comm1 & all_star > 0]$player_id
        )
        pl <- sample(names(player_pool)[player_pool >= allstar_count], 1)
    }else{
        pl <- sample(dta_comm[complete_comm1]$player_id, 1)
    }
    print(pl)
    statpair <- sample(statpairs,1)
    print(statpair)
    vars_bkvl <- c('year', statpair[[1]][1])
    d_bkvl <- dta_bkvl[complete_bkvl1 & player_id == pl, ..vars_bkvl]
    vars_comm <- c('season', statpair[[1]][2])
    d_comm <- dta_comm[complete_comm1 & player_id == pl, ..vars_comm]
    print(d_bkvl)
    print(d_comm)
    plot(NA, 
        main = paste(pl, paste(statpair, collapse='; '), sep = '\n'),
        xlim = range(d_bkvl[[1]], d_comm[[1]], na.rm = TRUE),
        ylim = range(d_bkvl[[2]], d_comm[[2]], na.rm = TRUE)
    )
    lines(d_bkvl, type= 'b', pch = 16, cex = 1.5, col='#994455')
    lines(d_comm, type= 'b', pch = 16, cex = 1.5, col='#004488')
}

# Evaluate which cases are included / not included in the two datasets
player_seasons_bkvl1 <- paste(dta_bkvl[complete_bkvl1]$player_id, dta_bkvl[complete_bkvl1]$season)
player_seasons_bkvl2 <- paste(dta_bkvl[complete_bkvl2]$player_id, dta_bkvl[complete_bkvl2]$season)
player_seasons_comm1 <- paste(dta_comm[complete_comm1]$player_id, dta_comm[complete_comm1]$season)
player_seasons_comm2 <- paste(dta_comm[complete_comm2]$player_id, dta_comm[complete_comm2]$season)

bkvl_not_comm_1 <- dta_bkvl[complete_bkvl1][!(player_seasons_bkvl1 %in% player_seasons_comm1)]
bkvl_not_comm_2 <- dta_bkvl[complete_bkvl2][!(player_seasons_bkvl2 %in% player_seasons_comm2)]
comm_not_bkvl_1 <- dta_comm[complete_comm1][!(player_seasons_comm1 %in% player_seasons_bkvl1)]
comm_not_bkvl_2 <- dta_comm[complete_comm2][!(player_seasons_comm2 %in% player_seasons_bkvl2)]

cat(sprintf("Player-seasons included in BKVL but not in basketball-reference: %d\n", nrow(bkvl_not_comm_1)))
cat(sprintf("Player-seasons included in basketball-reference but not in BKVL (model 1): %d\n", nrow(comm_not_bkvl_1)))
cat(sprintf("Player-seasons included in basketball-reference but not in BKVL (model 6): %d\n", nrow(comm_not_bkvl_2)))



####
# Create the "four careers" figure for the comment
####
pids <- c("jordami01","hardati01","barroea01","leedo01")

pdf('output/four_careers.pdf', width = 12, height = 10)

layout(matrix(1:4, nrow = 2, byrow = TRUE))
col <- c('#BB5566','#004488')
for(pl in pids){
    player_name = dta_bkvl[player_id == pl]$name[1]
    d_bkvl <- na.omit(dta_bkvl[complete_bkvl1 & player_id == pl, .(season, pts_sdt_t)])
    d_comm <- na.omit(dta_comm[complete_comm1 & player_id == pl, .(season, pts_std)])
    xlim <- range(d_bkvl$season, d_comm$season, na.rm = TRUE)
    ylim <- range(0,d_bkvl$pts_sdt_t, d_comm$std, na.rm = TRUE)
    # empty plot
    plot(
        NA, xlim = xlim, ylim=ylim,
        main=player_name, xlab = "All-Star year", ylab = "Points / 36 min." 
    )
    lines(d_bkvl, type= 'b', pch = 16, cex = 1.5, col=col[1])
    lines(d_comm, type= 'b', pch = 15, cex = 1.5, col=col[2])
    legend(
        'bottomleft',
        legend = c('BKVL', 'basketball-reference.com'), 
        col = col, pch = 16)
}

dev.off()







#####
####
# Regressions
####
#####



# empty table to store AME results
table3_sbs <- data.frame(
    estimate = paste(rep(glue("M{1:6}"),each=2),rep(c("est","se"),times=2)),
    AS_lag_bkvl = NA,
    AS_cum_bkvl = NA,
    AS_lag_corr = NA,
    AS_cum_corr = NA
)

####
# reproduce analysis using data from BKVL:
####

# lists to hold the model objects
# (one for raw logistic regressions, one for the AMEs)
bkvl_fits <- list()
bkvl_ames <- list()

## M1: unadjusted association
m1_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x"
))
bkvl_fits$m1 <- glm.cluster(
    m1_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m1 <- margins(bkvl_fits$m1$glm_res, vcov = bkvl_fits$m1$vcov,
    variables = "allstar_x", data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[1, 2] <- summary(bkvl_ames$m1)$AME
table3_sbs[2, 2] <- summary(bkvl_ames$m1)$SE

## M2: Time-constant confounders
m2_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x + ",
    glue_collapse(confinv_bkvl, sep=" + ")
))
bkvl_fits$m2 <- glm.cluster(
    m2_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m2 <- margins(bkvl_fits$m2$glm_res, vcov = bkvl_fits$m2$vcov,
    variables = "allstar_x", data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[3, 2] <- summary(bkvl_ames$m2)$AME
table3_sbs[4, 2] <- summary(bkvl_ames$m2)$SE

## M3: Time-varying confounders
m3_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x + ",
    glue_collapse(confinv_bkvl, sep = " + "), " + ",
    glue_collapse(confvar_bkvl, sep = " + ")
))
bkvl_fits$m3 <- glm.cluster(
    m3_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m3 <- margins(bkvl_fits$m3$glm_res, vcov = bkvl_fits$m3$vcov,
    variables = "allstar_x", data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[5, 2] <- summary(bkvl_ames$m3)$AME
table3_sbs[6, 2] <- summary(bkvl_ames$m3)$SE

## M4: Performance after AS
m4_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x + ",
    glue_collapse(confinv_bkvl, sep = " + "), " + ",
    glue_collapse(confvar_bkvl, sep = " + "), " + ",
    glue_collapse(perft_bkvl, sep = " + ")
))
bkvl_fits$m4 <- glm.cluster(
    m4_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m4 <- margins(bkvl_fits$m4$glm_res, vcov = bkvl_fits$m4$vcov,
    variables = "allstar_x", data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[7, 2] <- summary(bkvl_ames$m4)$AME
table3_sbs[8, 2] <- summary(bkvl_ames$m4)$SE

## M5: Situation after AS
m5_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x + ",
    glue_collapse(confinv_bkvl, sep = " + "), " + ",
    glue_collapse(confvar_bkvl, sep = " + "), " + ",
    glue_collapse(perft_bkvl, sep = " + "), " + ",
    glue_collapse(situt_bkvl, sep = " + ")
))
bkvl_fits$m5 <- glm.cluster(
    m5_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m5 <- margins(bkvl_fits$m5$glm_res, vcov = bkvl_fits$m5$vcov,
    variables = "allstar_x", data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[9, 2] <- summary(bkvl_ames$m5)$AME
table3_sbs[10, 2] <- summary(bkvl_ames$m5)$SE

## M6: Cumulative AS (only with general AS t-1)
m6_bkvl <- as.formula(glue(
    "allstar_y ~ allstar_x + allstar_hi + ",
    glue_collapse(confinv_bkvl, sep = " + "), " + ",
    glue_collapse(confvar_bkvl, sep = " + "), " + ",
    glue_collapse(perft_bkvl, sep = " + "), " + ",
    glue_collapse(situt_bkvl, sep = " + "), " + ",
    glue_collapse(varhi_bkvl, sep = " + ")
))
bkvl_fits$m6 <- glm.cluster(
    m6_bkvl, data = dta_bkvl,
    family = "binomial", cluster = "player_id"
)
bkvl_ames$m6 <- margins(bkvl_fits$m6$glm_res, vcov = bkvl_fits$m6$vcov,
    variables = c("allstar_x", "allstar_hi"), data = dta_bkvl,
    change = c(0, 1)
)
table3_sbs[11, 2] <- summary(bkvl_ames$m6)$AME[2]
table3_sbs[12, 2] <- summary(bkvl_ames$m6)$SE[2]
table3_sbs[11, 3] <- summary(bkvl_ames$m6)$AME[1]
table3_sbs[12, 3] <- summary(bkvl_ames$m6)$SE[1]

####
# reproduce analysis using corrected data:
####

# lists to hold the model objects
# (one for raw logistic regressions, one for the AMEs)
corr_fits <- list()
corr_ames <- list()

## M1: unadjusted association
m1_corr <- as.formula(glue(
    "all_star ~ all_star_t_1"
))
corr_fits$m1 <- glm.cluster(
    m1_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m1 <- margins(corr_fits$m1$glm_res, vcov = corr_fits$m1$vcov,
    variables = "all_star_t_1", data = dta_comm[complete_comm2],
    change = c(0, 1)
)
table3_sbs[1, 4] <- summary(corr_ames$m1)$AME
table3_sbs[2, 4] <- summary(corr_ames$m1)$SE

## M2: Time-constant confounders
m2_corr <- as.formula(glue(
    "all_star ~ all_star_t_1 + ",
    glue_collapse(confinv_comm, sep = " + ")
))
corr_fits$m2 <- glm.cluster(
    m2_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m2 <- margins(corr_fits$m2$glm_res, vcov = corr_fits$m2$vcov,
    variables = "all_star_t_1", data = dta_comm[complete_comm2],
    change = c(0, 1)
)
table3_sbs[3, 4] <- summary(corr_ames$m2)$AME
table3_sbs[4, 4] <- summary(corr_ames$m2)$SE

## M3: Time-varying confounders
m3_corr <- as.formula(glue(
    "all_star ~ all_star_t_1 + ",
    glue_collapse(confinv_comm, sep = " + "), " + ",
    glue_collapse(confvar_comm, sep = " + ")
))
corr_fits$m3 <- glm.cluster(
    m3_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m3 <- margins(corr_fits$m3$glm_res, vcov = corr_fits$m3$vcov,
    variables = "all_star_t_1", data = dta_comm[complete_comm2],
    change = c(0, 1)
)
table3_sbs[5, 4] <- summary(corr_ames$m3)$AME
table3_sbs[6, 4] <- summary(corr_ames$m3)$SE

## M4: Performance after AS
m4_corr <- as.formula(glue(
    "all_star ~ all_star_t_1 + ",
    glue_collapse(confinv_comm, sep = " + "), " + ",
    glue_collapse(confvar_comm, sep = " + "), " + ",
    glue_collapse(perft_comm, sep = " + ")
))
corr_fits$m4 <- glm.cluster(
    m4_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m4 <- margins(corr_fits$m4$glm_res, vcov = corr_fits$m4$vcov,
    variables = "all_star_t_1", data = dta_comm[complete_comm2][season>1985],
    change = c(0, 1)
)
table3_sbs[7, 4] <- summary(corr_ames$m4)$AME
table3_sbs[8, 4] <- summary(corr_ames$m4)$SE

## M5: Situation after AS
m5_corr <- as.formula(glue(
    "all_star ~ all_star_t_1 + ",
    glue_collapse(confinv_comm, sep = " + "), " + ",
    glue_collapse(confvar_comm, sep = " + "), " + ",
    glue_collapse(perft_comm, sep = " + "), " + ",
    glue_collapse(situt_comm, sep = " + ")
))
corr_fits$m5 <- glm.cluster(
    m5_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m5 <- margins(corr_fits$m5$glm_res, vcov = corr_fits$m5$vcov,
    variables = "all_star_t_1", data = dta_comm[complete_comm2][season>1985],
    change = c(0, 1)
)
table3_sbs[9, 4] <- summary(corr_ames$m5)$AME
table3_sbs[10, 4] <- summary(corr_ames$m5)$SE

## M6: Cumulative AS (only with general AS t-1)
m6_corr <- as.formula(glue(
    "all_star ~ all_star_t_1 + all_star_cum_t_1 + ",
    glue_collapse(confinv_comm, sep = " + "), " + ",
    glue_collapse(confvar_comm, sep = " + "), " + ",
    glue_collapse(perft_comm, sep = " + "), " + ",
    glue_collapse(situt_comm, sep = " + "), " + ",
    glue_collapse(varhi_comm, sep = " + ")
))
corr_fits$m6 <- glm.cluster(
    m6_corr, data = dta_comm[complete_comm2],
    family = "binomial", cluster = "player_id"
)
corr_ames$m6 <- margins(corr_fits$m6$glm_res, vcov = corr_fits$m6$vcov,
    variables = c("all_star_t_1", "all_star_cum_t_1"), data = dta_comm[complete_comm2][season>1985],
    change = c(0, 1)
)
table3_sbs[11, 4] <- summary(corr_ames$m6)$AME[2]
table3_sbs[12, 4] <- summary(corr_ames$m6)$SE[2]
table3_sbs[11, 5] <- summary(corr_ames$m6)$AME[1]
table3_sbs[12, 5] <- summary(corr_ames$m6)$SE[1]


####
# Complete model with random effects instead of robest SE:
####

## M6: Cumulative AS (scaling some variables for better convergence)
m6_re <- 
    all_star ~ all_star_t_1 + all_star_cum_t_1 + scale(height) + pos + scale(age_0) +
        I(scale(age_0)^2) + Black + scale(time) + I(scale(time)^2) + cyear + scale(pts_std_t_1) + scale(ast_std_t_1) +
        scale(trb_std_t_1) + scale(min_played_std_t_1) + playoffs_t_1 + win_std_t_1 +
        bigm_std_t_1 + scale(pts_std) + scale(ast_std) + scale(trb_std) + scale(min_played_std) +
        playoffs + win_std + bigm_std + scale(pts_cum_t_1) + scale(ast_cum_t_1) +
        scale(trb_cum_t_1) + scale(min_played_cum_t_1) + playoffs_cum_t_1 + win_cum_t_1 +
        bigm_cum_t_1 + (1 | player_id)
f6_re <- glmer(
    m6_re, data = dta_comm[complete_comm2],
    control = glmerControl(optimizer = "nlminbwrap", optCtrl = list(eval.max=500, iter.max=500)),
    family = "binomial"
)

# uncomment to test for convergence issues 
# by estimating with multiple optimizers
# (this is very slow):
# f6_re_all <- allFit(f6_re)

ame6_re <- margins(f6_re,
    variables = c("all_star_t_1", "all_star_cum_t_1"), data = dta_comm[complete_comm2][season>1985],
    type="response", change = c(0, 1)
)

# Find players with large-magnitude random effects
player_ranef <- ranef(f6_re)$player_id
player_ranef$player_id <- rownames(player_ranef)
player_ranef_lo <- head(player_ranef[order(player_ranef$`(Intercept)`),'player_id'], 20)
player_ranef_hi <- head(player_ranef[order(-player_ranef$`(Intercept)`),'player_id'], 20)

br_url <- "https://www.basketball-reference.com/players/%s/%s.html"
sprintf(br_url, substr(player_ranef_lo,1,1), player_ranef_lo)
sprintf(br_url, substr(player_ranef_hi,1,1), player_ranef_hi)

####
# write to disk
####
# table for comment
export(table3_sbs, outpath)
# model objects in raw .rds files
export(bkvl_fits, file = 'models/original_fits.rds')
export(bkvl_ames, file = 'models/original_ames.rds')
export(corr_fits, file = 'models/corrected_bkvlinal_fits.rds')
export(corr_ames, file = 'models/corrected_bkvlinal_ames.rds')
export(list(f6_re=f6_re,ame6_re=ame6_re), file = 'models/randomeffects.rds')

if(FALSE){
    table3_sbs <- import(outpath)
    bkvl_fits <- import('models/original_fits.rds')
    bkvl_ames <- import('models/original_ames.rds')
    corr_fits <- import('models/corrected_bkvlinal_fits.rds')
    corr_ames <- import('models/corrected_bkvlinal_ames.rds')
    f6_re <- import('models/randomeffects.rds')$f6_re
    ame6_re <- import('models/randomeffects.rds')$ame6_re
}

####
# tables output
####
# more legible and matching coefficient names

year_coef_names <- glue("year {1986:2016}")
names(year_coef_names) <- glue("cyear{1986:2015}")
coef_names_corr <- c(
    "(Intercept)"      = "(Intercept)",
    "all_star"         = "All-Star",
    "all_star_t_1"     = "All-Star (t-1)",
    "all_star_cum_t_1" = "All-Star (cum.)",
    "height"           = 'Height',
    "Black"            = "Black",
    "age_0"            = "Starting age",
    "age_0_2"          = "(Starting age)^2",
    "time"             = "Years in NBA",
    "time_2"           = "(Years in NBA)^2",
    "posForward (Power/Small)" = "Forward (ref. center)",
    "posGuard (Point/Shooting)" = "Guard (ref. center)",
    "min_played_std_t_1"= "Minutes played (t-1)",
    "min_played_std"    = "Minutes played",
    "min_played_cum_t_1"= "Minutes played (cum.)",
    "pts_std_t_1"      = "Points / 36 min. (t-1)",
    "pts_std"          = "Points / 36 min.",
    "pts_cum_t_1"      = "Points / 36 min. (cum.)",
    "ast_std_t_1"      = "Assists / 36 min. (t-1)",
    "ast_std"          = "Assists / 36 min.",
    "ast_cum_t_1"      = "Assists / 36 min. (cum.)",
    "trb_std_t_1"      = "Rebounds / 36 min. (t-1)",
    "trb_std"          = "Rebounds / 36 min.",
    "trb_cum_t_1"      = "Rebounds / 36 min. (cum.)",
    "playoffs_t_1"     = "Playoffs (t-1)",
    "playoffs"         = "Playoffs",
    "playoffs_cum"     = "Playoffs (cum.)",
    "win_std_t_1"      = "Win % (t-1)",
    "win_std"          = "Win %",
    "win_cum_t_1"      = "Win % (cum)",
    "bigm_std_t_1"     = "Big market (t-1)",
    "bigm_std"         = "Big market",
    "bigm_cum_t_1"     = "Big market (cum)",
    year_coef_names
)

coef_names_bkvl <- c(
    "(Intercept)"      = "(Intercept)",
    "allstar_y"        = "All-Star",
    "allstar_x"        = "All-Star (t-1)",
    "allstar_hi"       = "All-Star (cum.)",
    "height"           = 'Height',
    "Black"            = "Black",
    "age_0"            = "Starting age",
    "age_0_2"          = "(Starting age)^2",
    "time"             = "Years in NBA",
    "time_2"           = "(Years in NBA)^2",
    "posForward (Power/Small)" = "Forward (ref. center)",
    "posGuard (Point/Shooting)" = "Guard (ref. center)",
    "min_played_nd"    = "Minutes played (t-1)",
    "min_played_t"     = "Minutes played",
    "min_played_hi"    = "Minutes played (cum.)",
    "pts_sdt_nd"       = "Points / 36 min. (t-1)",
    "pts_sdt_t"        = "Points / 36 min.",
    "pts_sdt_hi"       = "Points / 36 min. (cum.)",
    "ast_sdt_nd"       = "Assists / 36 min. (t-1)",
    "ast_sdt_t"        = "Assists / 36 min.",
    "ast_sdt_hi"       = "Assists / 36 min. (cum.)",
    "trb_sdt_nd"       = "Rebounds / 36 min. (t-1)",
    "trb_sdt_t"        = "Rebounds / 36 min.",
    "trb_sdt_hi"       = "Rebounds / 36 min. (cum.)",
    "playoffs_nd"      = "Playoffs (t-1)",
    "playoffs_t"       = "Playoffs",
    "playoffs_hi"      = "Playoffs (cum.)",
    "win_nd"           = "Win % (t-1)",
    "win_t"            = "Win %",
    "win_hi"           = "Win % (cum)",
    "bigm_nd"          = "Big market (t-1)",
    "bigm_t"           = "Big market",
    "bigm_hi"          = "Big market (cum)",
    year_coef_names
)

modelsummary(
    lapply(corr_fits, function(x){x$glm_res}),
    vcov = lapply(corr_fits, function(x){x$vcov}),
    output = 'output/corrected_bkvl.html',
    coef_map = coef_names_corr
)
modelsummary(
    lapply(bkvl_fits, function(x){x$glm_res}),
    vcov = lapply(bkvl_fits, function(x){x$vcov}),
    output = 'output/original_model.html',
    coef_map = coef_names_bkvl
)




