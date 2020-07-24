#' Checks min/max elevations and recodes if min > max
#' @description Checks all species' elevations and if min > max recodes min to -99999 and max to 99999
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param x A dataframe in wide or long form, as output by \code{RL_fetch()} and \code{RL_reformat_long()} respectively.
#' @param verbose (optional) Logical. Should species with incorrect elevation values be printed out? Defaults to \code{FALSE}.
#' @return A dataframe in the same format as the input.
#' @export

RL_elevation_check <-
  function(
    x,
    verbose = FALSE
  ){
    if(!is.logical(verbose)){
      stop("Please supply a valid logical value for 'verbose'.")
    }

    min_alt <-
      x$min_alt %>%
      as.character() %>%
      as.numeric()
    max_alt <-
      x$max_alt %>%
      as.character() %>%
      as.numeric()

    delta_alt <-
      max_alt - min_alt

    alts_to_change <-
      which(delta_alt <= 0)

    if(length(alts_to_change) >0L){
      if(verbose == TRUE){
        species_to_change <-
          x[alts_to_change,c("iucn_id", "binomial")]
        message("The following species have had elevations changed:\n")
        print(species_to_change)
      }

      x$min_alt[alts_to_change] = -99999
      x$max_alt[alts_to_change] <- 99999
    }else{
      if(verbose == TRUE){
        message("No species have had elevations changed.")
      }

    }



    return(x)
  }
