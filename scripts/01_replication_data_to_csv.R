# simple script to convert andata.dta to andata.csv

library(rio)

andata <- import('data/andata.dta')

# fix stata labeled vectors
# (this only affects `team`)
for (cname in names(andata)) {
    if ("labels" %in% names(attributes(andata[[cname]]))) {
        ncats <- length(unique(andata[[cname]]))
        labels <- attr(andata[[cname]], "labels")
        labeled <- names(labels)[match(andata[[cname]], labels)]
        andata[[cname]] <- labeled
    }
}

export(andata, 'data/andata.csv')

