# functions to read auxiliary files (i.e. data files, aggfiles, etc.)



#' Read auxiliary Gadget files (i.e. data files, aggfiles, auxiliary stockfiles,
#' etc.)
#'
#' This suite of functions are specific to types of Gadget auxiliary stockfiles,
#' with special attention paid to the various auxiliary stockfiles such as
#' refweightfile, initial condition and renewal data distribution files,
#' spawnfile, etc. These functions are for use in \code{\link{check_stockfile}}
#'
#' @param file Character. The name of the file to be read
#' @inheritParams call_gadget
#'
#' @return Typically a list with the elements specific to what is needed for
#' each type of auxiliary file. Aggfiles and data files will often return a
#' data.frame
#'
#' @name read_gadget_aux_files
#'
#' @examples
#' gad_data <- read_gadget_refweightfile("Modelfiles/cod.refweightfile",
#'                                       path = gad_mod_dir)
read_gadget_refweightfile <- function(file, path = NULL) {
    datfile <- readLines(check_path(file))
    return(format_auxiliary_file(datfile, c("length", "weight")))
}

#' @rdname read_gadget_aux_files
read_gadget_stock_len_aggfile <- function(file, path = NULL) {
    aggfile <- readLines(check_path(file))
    return(format_auxiliary_file(aggfile, c("name", "lower", "upper")))
}

#' @rdname read_gadget_aux_files
read_gadget_preylengths <- function(file, path = NULL) {
    aggfile <- readLines(check_path(file))
    return(format_auxiliary_file(aggfile, c("name", "lower", "upper")))
}

#' @rdname read_gadget_aux_files
read_gadget_init_cond <- function(file, data_dist_type, path = NULL) {
    data_file <- readLines(check_path(file))
    aux_names <-
        switch(data_dist_type,
               normalcondfile = c("age", "area", "age.factor", "area.factor",
                                  "mean", "sd", "relcond"),
               normalparamfile = c("age", "area", "age.factor", "area.factor",
                                   "mean", "sd", "alpha", "beta"),
               numberfile = c("area", "age", "length", "number", "weight"))
    return(format_data_dist_file(data_file, aux_names))
}

#' @rdname read_gadget_aux_files
read_gadget_yearstepfile <- function(file, path = NULL) {
    ys_file <- readLines(check_path(file))
    return(format_auxiliary_file(ys_file,
                                 c("year", "step", "migration_matrix")))
}

#' @rdname read_gadget_aux_files
read_gadget_migration_file <- function(file, migration_type, path = NULL) {
    mig_file <- readLines(check_path(file))
    mig_file <- strip_comments(mig_file)
    mig_file <- mig_file[mig_file != ""]
    mig_mat_ind <- grep("[migrationmatrix]", mig_file, fixed = TRUE)
    mig_file <- make_list_at_index(mig_file, mig_mat_ind)
    # formalized this line of code into a function, but keeping it for a minute
    #unname(split(mig_file,
    # sort(rep(mig_mat_ind, length(mig_file) / length(mig_mat_ind)))))
    mig_mat_names <-
        unlist(lapply(mig_file, function(x) {
            return(split_ws(x[2])[2])
        }))
    if (migration_type == "matrices") {
        mig_data <-
            lapply(mig_file, function(x) {
                mig_values <- split_ws(x[3:length(x)])
                mig_values <-
                    tryCatch(as.numeric(mig_values),
                             warning = function(w) return(mig_values),
                             error = function(e) return(mig_values))
                return(matrix(mig_values, nrow = sqrt(length(mig_values)),
                              byrow = TRUE))
            })
    } else if (migration_type == "ratios") {
        mig_data <-
            lapply(mig_file, function(x) {
                mig_values <- split_ws_list(x[3:length(x)])
                mig_values <-
                    do.call("rbind", lapply(mig_values, function(y) {
                        tmp <-
                            tryCatch(as.numeric(mig_values),
                                     warning = function(w) return(mig_values),
                                     error = function(e) return(mig_values))
                        return(data.frame(t(matrix(y))))
                    }))
                names(mig_values) <- c("from", "to", "ratio")
                return(mig_values)
            })
    } else {
        stop("Migration data must be one of the following", "\n",
             paste(paste0(" * ", c("migration matrices", "migration ratios")),
                   collapse = "\n"))
    }
    out <- setNames(mig_data, mig_mat_names)
    return(out)
}

#' @rdname read_gadget_aux_files
read_gadget_maturity_file <- function(file, mat_fun_type, path = NULL) {
    matfile <- readLines(check_path(file))
    matfile <- trimws(strip_comments(matfile))
    matfile <- split_ws(matfile)
    base_args <- c("maturestocksandratios")
    mat_fun_args <-
        switch(mat_fun_type,
               newconstant = c(base_args, "coefficients", "maturitysteps"),
               newconstantweight = c(base_args, "coefficients",
                                     "maturitysteps"),
               fixedlength = c(base_args, "maturitysteps", "maturitylengths"),
               continuous = c(base_args, "coefficients"))
    mat_fun_inds <- get_index(mat_fun_args, matfile)
    matfile <- make_list_at_index(matfile, mat_fun_inds, keep_indices = FALSE)
    matfile[[1]] <- data.frame(matrix(matfile[[1]], ncol = 2, byrow = TRUE),
                                   stringsAsFactors = FALSE)
    names(matfile[[1]]) <- c("stockname", "ratio")
    matfile[[1]]$ratio <- as.numeric(matfile[[1]]$ratio)
    return(setNames(matfile, mat_fun_args))
}

#' @rdname read_gadget_aux_files
read_gadget_renewal <- function(file, data_dist_type, path = NULL) {
    data_file <- readLines(check_path(file))
    aux_names <-
        switch(data_dist_type,
               normalcondfile = c("year", "step", "area", "age", "number",
                                  "mean", "sd", "relcond"),
               normalparamfile = c("year", "step", "area", "age", "number",
                                   "mean", "sd", "alpha", "beta"),
               numberfile = c("year", "step", "area", "age",
                              "length", "number", "weight"))
    return(format_data_dist_file(data_file, aux_names))
}

#' @rdname read_gadget_aux_files
read_gadget_spawnfile <- function(file, path = NULL) {
    spawnfile <- readLines(check_path(file))
    spawnfile <- trimws(strip_comments(spawnfile))
    spawnfile <- split_ws(spawnfile)
    base_args <- c("spawnsteps", "spawnareas",
                   "firstspawnyear", "lastspawnyear")
    lenfun_args <- c("proportionfunction", "mortalityfunction",
                     "weightlossfunction")
    if (any(grepl("^onlyparent$", spawnfile))) {
        arg_names <- c(base_args, "onlyparent", lenfun_args)
    } else {
        arg_names <- c(base_args, "spawnstocksandratios", lenfun_args,
                       "recruitment", "stockparameters")
    }
    arg_ind <- get_index(arg_names, spawnfile)
    spawnfile <- make_list_at_index(spawnfile, arg_ind, keep_indices = FALSE)
    names(spawnfile) <- arg_names
    return(structure(spawnfile, class = c("gadget_spawnfile", "list")))
}

#' @rdname read_gadget_aux_files
read_gadget_strayfile <- function(file, path = NULL) {
    strayfile <- readLines(check_path(file))
    strayfile <- trimws(strip_comments(strayfile))
    strayfile <- split_ws(strayfile)
    arg_names <- c("straysteps", "strayareas",
                   "straystockandratios", "proportionfunction")
    arg_ind <- get_index(arg_names, strayfile)
    strayfile <- make_list_at_index(strayfile, arg_ind, keep_indices = FALSE)
    names(strayfile) <- arg_names
    return(structure(strayfile, class = c("gadget_strayfile", "list")))
}

read_gadget_datafile <- function(file, colnames = NULL, path = NULL) {
    datfile <- readLines(check_path(file))
    datfile <- trimws(strip_comments(datfile))
    dat_list <-
        lapply(datfile, function(x) {
            tmp <- split_ws(x)
            tmp <- data.frame(matrix(tmp, nrow = 1), stringsAsFactors = FALSE)
            if (!is.null(colnames)) {
                names(tmp) <- colnames
            }
            return(tmp)
        })
    return(do.call("rbind", dat_list))
}

#' @rdname read_gadget_aux_files
format_auxiliary_file <- function(aux_file, aux_names) {
    aux_file <- trimws(strip_comments(aux_file))
    gadget_auxfile <-
        lapply(aux_file, function(x) {
            out <- matrix(split_tab(x), ncol = length(aux_names))
            names(out) <- aux_names
            return(out)
        })
    out_data <- data.frame(do.call("rbind", gadget_auxfile),
                           stringsAsFactors = FALSE)
    return(setNames(out_data, aux_names))
}

#' @rdname read_gadget_aux_files
format_data_dist_file <- function(data_dist_file, data_names) {
    data_file <- trimws(data_dist_file[-grep("^;", data_dist_file)])
    gadget_data_file <-
        lapply(data_file, function(x) {
            out <- matrix(split_tab(x), ncol = length(data_names))
            names(out) <- data_names
            return(out)
        })
    out_data <- data.frame(do.call("rbind", gadget_data_file),
                           stringsAsFactors = FALSE)
    return(setNames(out_data, data_names))
}
