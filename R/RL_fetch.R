#' Fetch Red List data
#' @description Download Red List habitat and elevation data from the Red List API.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x List, vector, or dataframe with a column corresponding to binomial species names or IUCN Red List IDs.
#' @param col.name Character string. Column name if x is a dataframe. Not needed if x is a list or vector.
#' @param query Character string. 'name' if querying by species name; 'ID' if querying by IUCN Red List ID.
#' @param key Character string. IUCN Red List API key - available from https://apiv3.iucnredlist.org/api/v3/token
#' @param sleep_dur (optional) Numeric. Duration of sleep between API calls in seconds. Defaults to 2 seconds.
#' @param subset (optional). Numeric. Specify proportion of x to run for between 0 and 1. Defaults to 1.
#' @param parallel (optional) Logical. If TRUE uses multiple cores. Default is TRUE.
#' @param num.cores (optional) Numeric. Specify number of cores to use if running in parallel. Default is number of CPU cores available - 1.
#' @param verbose (optional) Numeric. If 0 gives no progress update, if 1 prints progress bar, if 2 prints 1 row per task completed. Default is 1.
#'
#' @details Each cell value of the output dataframe contains the pasted combination of habitat suitability and major importance, following the datasets \code{habitats} and \code{major_importance}.
#' @details If the suitability value is missing for a habitat, this is coded as \code{999}. No habitat data available at all results in all habitats being coded as \code{66}.
#' @return A dataframe in wide format (one column per habitat category and one row per species' season).
#' @export

RL_fetch <-
  function(x,
           col.name,
           key,
           query,
           subset = 1,
           sleep_dur = 2,
           parallel = TRUE,
           num.cores = parallel::detectCores() - 1,
           verbose = 1) {

    # Checking needed packages are installed.

    packTest("rredlist")

    # Testing if API key is valid.
    if(is.character(key) == F){
      stop("Please supply a valid API key. API keys should be provided as a character string.")
    }
    # Testing if sleep duration is valid
    if(is.numeric(sleep_dur) == F){
      stop("Please supply a valid sleep duration. This should be a numeric input.")
    }
    # Handles verbose option
    if(is.numeric(verbose) == FALSE){
      stop("Please supply a valid value for 'verbose'.")
    }else if(!(verbose %in% c(0:2))){
        stop("Please supply a valid value for 'verbose'.")
    }

    if(is.data.frame(x) == TRUE){
      if(!is.character(col.name)){
        stop("Please supply a valid value for 'col.name'. 'col.name' should be a character input.")
      }else{
        x <- x[,col.name]
      }
    }
    if(is.list(x) == TRUE){
      x <- unlist(x)
    }
    #handles subset
    if(!is.numeric(subset)){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else if(subset <0 | subset > 1){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else{
      subset <- round(subset * length(x))
      message("\nFetching data for a subset of ", subset, "/", length(x), " (", round((100*subset/length(x)),2), "%) of the data.")
    }
    #handles query
    if(!is.character(query)){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input.")
    }else if(!(query %in% c("name", "ID"))){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input of 'name' or 'ID'.")
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
      df <- fetch_par(
        x = x,
        key = key,
        query = query,
        subset = subset,
        sleep_dur = sleep_dur,
        num.cores = num.cores,
        verbose =verbose
      )
    }else{
      df <- fetch_nopar(
        x = x,
        key = key,
        query = query,
        subset = subset,
        sleep_dur = sleep_dur,
        verbose =verbose
      )
    }
    df <- as.data.frame(df)
    return(df)
  }
