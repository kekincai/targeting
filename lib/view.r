ht <- function(x = train$最終接触時間, main = NULL, xlab = NULL, ylab = NULL) {
    need_rm <- is.na(x)
    x <- x[!need_rm]
    main <- paste0(
        main, "\n", "mean=",
        round(mean(x), 2), ", sd=", round(sd(x), 2)
    )
    hist(x,
        col = "steelblue",
        main = main, xlab = xlab, ylab = ylab
    )
}

bar <- function(x = train$結婚, main = NULL, xlab = NULL, ylab = NULL) {
    tb <- table(x)
    if (length(tb) > 5) {
        las <- 2
    } else {
        las <- 1
    }
    barplot(tb,
        col = "steelblue",
        main = main, xlab = xlab, ylab = ylab, las = las
    )
}

view <- function(d = train,
                 res = 600, w = 8, h = 6, pointsize = 10,
                 dir = "../out/") {
    items <- names(d)
    for (i in seq_along(items)) {
        item <- items[i]
        fn <- paste0(dir, i, "_", item, ".png")
        message("Saving: ", fn)
        png(fn, w * res, h * res, pointsize = pointsize, res = res)
        par(mar = c(7.1, 4.1, 4.1, 2.1), family = "HiraKakuProN-W3")
        if (class(d[, item]) %in% c("factor", "character")) {
            bar(d[, item], main = item)
        } else {
            ht(d[, item], main = item)
        }
        dev.off()
    }
}

if (FALSE) {
    source("../lib/view.r")
    view()
}
