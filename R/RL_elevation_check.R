#' Checks min/max elevations and recodes if min > max
#' @description Checks all species' elevations and if min > max recodes min to -99999 and max to 99999
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param x A dataframe in wide or long form, as output by \code{RL_fetch()} and \code{RL_reformat_long()} respectively.
#' @return A dataframe in the same format as the input.
#' @export

RL_elevation_check <-
  function(
    x
  ){
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

    x$min_alt[alts_to_change] = -99999
    x$max_alt[alts_to_change] <- 99999

    return(x)
  }