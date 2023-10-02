library(rio)
library(margins)

###
# load all of the AMEs and associated models
###
ame <- list(
    # Original M6 on corrected data
    "Original corrected\n(M6)" = import('models/ames_perf.rds')$orig,
    # Adding performance
    "Original + performance \n(M6.6)" = import('models/ames_perf.rds')$all4,
    # predicting All-NBA from All-NBA
    "All-NBA predicted by\nAll-NBA (ANBA1)" = import('models/ames_allnba_allnba.rds')$orig,
    # predicting All-NBA from All-NBA with performance
    "All-NBA predicted by\nAll-NBA + performance\n(ANBA6)" = import('models/ames_allnba_allnba.rds')$all4
)

###
# Extract point estimates and 95% confidence intervals
# for effect of cumulative selections
###
cum_estimates <- sapply(ame, function(m){
    msum <- summary(m, level = 0.95)
    return(as.numeric(msum[1, c('AME','lower','upper')]))
})

###
# Create a forest plot
###

# simple forest plot
forestplot <- function(ests, main = '', sorted = TRUE){
    if(sorted)(
        ests <- ests[,order(ests[1,])]
    )

    npars <- ncol(ests)
    ylim <- c(0.6, npars + 0.4)
    xlim <- range(ests)

    # adjust margins
    orig_mar <- par('mar')
    par(mar = c(4, 10, 4, 2))

    # empty plot
    plot(
        NA, xlim = xlim, ylim = ylim,
        main = main, ylab = NA, xlab = 'Estimate',
        yaxt = 'n'
    )
    axis(2, at = seq(npars), labels = colnames(ests), las = 1)
    abline(h = seq(npars), lwd = 0.5, lty = 3)
    abline(v = 0, lwd = 0.5)


    # intervals
    for(i in seq(npars)){
        lines(ests[2:3, i], c(i, i), lend='butt', lwd = 5)
    }

    # point estimates
    points(ests[1,], seq(npars), pch = 18, cex = 3)
    points(ests[1,], seq(npars), pch = 18, cex = 1.5, col = 'white')

    # reset graphics parameters
    par(mar = orig_mar)
}

pdf('output/forestplot.pdf', width = 8, height = 5, useDingbats = FALSE)
forestplot(cum_estimates, main = "Average marginal effect of cumulative selections")
dev.off()