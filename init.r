# initalization your project here

## create folders
dirs <- c("src", "lib", "rdata", "out", "output", "data")
for (i in dirs) {
    if (!dir.exists(i)) {
        dir.create(i)
    }
}
