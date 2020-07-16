#' Check necessary packages are loaded
#' @description Internal Function. Checks packages necessary for operation are loaded. Stops execution if not
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param pack Character. Package name.

packTest <-
  function(
    pack
  ){
    if (!requireNamespace(pack, quietly = TRUE)) {
      stop(paste0("Package \"", pack, "\" is needed for this function to work. Please install it."),
           call. = FALSE)
    }
  }
