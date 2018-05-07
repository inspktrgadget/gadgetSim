# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files

make_gadget_printfile <- function(main = "main", file, path = NULL,
                                  printatstart = 1, steps = "all") {
    if (!is.null(path)) {
        main <- read_gadget_main(file, path)
    } else {main <- read_gadget_main}

}
