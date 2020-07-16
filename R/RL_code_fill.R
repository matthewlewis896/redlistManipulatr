#' Fill Out Level 1 or 2 Habitat Codes
#' @description Optionally fill out Level 2 habitat codes if Level 1 codes exist and vice versa.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @param level1.recode (optional) Logical. If Level 2 habitat info exists, should the corresponding Level 1 habitat be given the same info? See Details. Default is TRUE.
#' @param level2.recode (optional) Logical. If Level 1 habitat info exists, should the corresponding Level 2 habitats be given the same info? See Details. Default is TRUE.
#' @param subset (optional). Numeric. Specify proportion of x to run for between 0 and 1. Defaults to 1.
#' @param parallel (optional) Logical. If TRUE uses multiple cores. Default is TRUE.
#' @param num.cores (optional) Numeric. Specify number of cores to use if running in parallel. Default is number of CPU cores available - 1.
#' @param verbose (optional) Numeric. If 0 gives no progress update, if 1 prints progress bar, if 2 prints 1 row per task completed. Default is 1.
#'
#' @details The \code{level1.recode} parameter will fill upstream Level 1 habitat categories if any Level 2 categories have habitat information. e.g. if habitat category 2.1 (Dry Savanna) is suitable and of major importance (coded as 31), habitat category 2 (Savanna) will be filled with this info too.
#' @details Similarly, \code{level2.recode} will fill all downstream Level 2 habitat categories if the corresponding Level 1 category is filled. e.g. if if habitat category 2 (Savanna) is suitable and of major importance (coded as 31), habitat categories 2.1 (Dry Savanna) and 2.2 (Moist Savanna) will be given the same information.
#' @return A dataframe in wide format (one column per habitat type and one row per species' season).
#' @export

RL_code_fill <-
  function(
    x,
    level1.recode = T,
    level2.recode = T,
    subset = 1,
    parallel = T,
    num.cores = parallel::detectCores()-1,
    verbose = 1
    ) {
  #x is the input df
  #y is the final condition - how long is this going for?
  #z is parent_columns we have IUCN hab data in
  #aa is all columns we have IUCN hab data in

    # Handles verbose option
    if(is.numeric(verbose) == FALSE){
      stop("Please supply a valid value for 'verbose'.")
    }else if(!(verbose %in% c(0:2))){
      stop("Please supply a valid value for 'verbose'.")
    }

    #level1 recode
    if(is.logical(level1.recode) == FALSE){
      stop("Please supply a valid value for 'level1.recode'.")
    }
    #level2 recode
    if(is.logical(level2.recode) == FALSE){
      stop("Please supply a valid value for 'level2.recode'.")
    }

    #handles subset
    if(!is.numeric(subset)){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else if(subset <0 | subset > 1){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else{
      subset <- round(subset * nrow(x))
      message("\nManipulating data for a subset of ", subset, "/", nrow(x), " (", round((100*subset/nrow(x)),2), "%) of the data.")
    }


    if(is.numeric(num.cores) == FALSE){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores %% 1 != 0){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores > parallel::detectCores()){
      stop("You have supplied a number of CPU cores greater than the number available.")
    }

    # Getting data (parallel or not)

    if(parallel == TRUE){
      df <- code_fill_par(
        x = x,
        level1.recode = level1.recode,
        level2.recode = level2.recode,
        subset = subset,
        num.cores = num.cores,
        verbose = verbose
      )
    }else{
      df <- code_fill_nopar(
        x = x,
        level1.recode = level1.recode,
        level2.recode = level2.recode,
        subset = subset,
        verbose = verbose
      )
    }
}
