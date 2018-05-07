# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files

make_gadget_printfile <- function(main = "main", file, path = NULL,
                                  printatstart = 1, steps = "all") {
    if (!is.null(path)) {
        main <- read_gadget_main(file, path)
        dir_name <- basename(path)
    } else {
        main <- read_gadget_main
        dir_name <- basename(getwd())
    }
    stockfiles <- main$stockfiles
    fleetfiles <- main$fleetfiles
    lik_files <- main$likelihood
    header <- sprintf("; printfile for %s - created by gadgetSim %s on %s",
                      dir_name, packageVersion("gadgetSim"), date())
    # first format the likelihood content
    likelihood <- read_gadget_likelihood(lik_files)
}
