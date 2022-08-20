get_load <- function(fn) {
    # load the rdata file
    get(load(fn))
}
