split_levs <- function(x = "jan=1, feb=2, mar=3, apr=4, may=5,
                        jun=6, jul=7, aug=8, sep=9, oct=10, nov=11") {
    # split levels into levels and labels
    if (x == "") {
        return(NULL)
    }
    x <- x |>
        strsplit(split = ",|=") |>
        unlist() |>
        trimws() |>
        matrix(, ncol = 2, byrow = TRUE)
    list(levs = x[, 1], labels = x[, 2])
}
read_data <- function(fn = "../data/train.csv",
                      def_fn = "../def/train_def.csv") {
    # read the data
    d <- read.csv(fn, colClasses = "character")
    def <- read.csv(def_fn)
    i_def <- match(names(d), def$ヘッダ名称)
    use <- def$use[i_def]
    d <- d[, use]
    i_def <- i_def[use]
    for (i in seq_len(ncol(d))) {
        df <- def[i_def[i], ]
        di <- d[, i]
        d[, i] <- switch(df$class,
            int = as.integer(di),
            factor = {
                f <- split_levs(df$levels)
                if (is.null(f)) {
                    factor(di)
                } else {
                    factor(di, f$levs, f$labels)
                }
            },
            POSIxct = as.POSIXct(di),
            di
        )
    }
    names(d) <- def$name[i_def]
    out_fn <- paste0(
        "../rdata/", gsub("\\.\\./.+/(.+)\\.csv$", "\\1", fn),
        ".rdata"
    )
    message("Writing: ", out_fn)
    save(d, file = out_fn)
}

if (FALSE) {
    source("../lib/read.r")
    read_data()
    train <- get_load("../rdata/train.rdata")
}
