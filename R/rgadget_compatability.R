# functions to make gadgetSim objects more Rgadget appropriate

#' Convert classes of Rgadget and gadgetSim objects to be compatible with one another
#'
#' Much care has been taken in an attempt to keep gadgetSim and Rgadget as compatible as possible, but
#' Rgadget uses slightly different syntax for creating classes of objects that gadgetSim.
#' These functions alter classes appropriately allowing for greater compatibility between the two
#' packages allowing for (hopefully) seamless integration.
#'
#' @param gs_obj An object created or read by gadgetSim (i.e. should be of class \code{gadget_*}
#' (i.e. \code{gadget_main}, \code{gadget_likelihood}, etc))
#'
#' @return The same \code{gs_obj}, but with a class appropriate for use in Rgadget
#' @export
#'
#' @name rg_class_compatibility
#'
#' @examples
#' # converting from gadgetSim to Rgadget
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' gs_main <- read_gadget_main(path = path)
#' gs2rg_classes(gs_main)
#'
#' # converting classes from Rgadget to gadgetSim
#' rg_main <- Rgadget::read.gadget.main(paste(path, "main", sep = "/"))
#' rg2gs_classes(rg_main)
gs2rg_classes <- function(gs_obj) {
    UseMethod("gs2rg_classes")
}

gs2rg_classes.gadget_main <- function(gs_obj) {
    class(gs_obj) <- c("gadget.main", "list")
    return(gs_obj)
}

gs2rg_classes.gadget_stocks <- function(gs_obj) {
    class(gs_obj) <- c("gadget.stocks", "list")
    return(gs_obj)
}

gs2rg_classes.gadget_stock <- function(gs_obj) {
    class(gs_obj) <- c("gadget.stock", "list")
    return(gs_obj)
}

gs2rg_classes.gadget_likelihood <- function(gs_obj) {
    class(gs_obj) <- c("gadget.likelihood", "list")
    return(gs_obj)
}

#' @rdname rg_class_compatibility
#' @param rg_obj An object created or read by Rgadget (i.e. likely will be of class \code{gadget.*}
#' (i.e. \code{gadget.main}, \code{gadget.printfile}, etc))
#' @export
rg2gs_classes <- function(rg_obj) {
    class(rg_obj) <- gsub("gadget.", "gadget_", class(rg_obj))
    return(rg_obj)
}

