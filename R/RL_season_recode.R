#' Recode NA seasons to Resident
#' @description Recodes seasons coded on the Red List as NA (and pulled down by RL_fetch() as 999) to Resident (1).
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_season_recode <-
  function(
    x
  ){
    x$season[x$season == 999] <- 1
    return(x)
  }
