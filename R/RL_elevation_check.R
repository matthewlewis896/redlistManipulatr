#' Checks min/max elevations and recodes if min >= max
#' @description Checks all species' elevations and if min >= max recodes min to -99999 and max to 99999. Other options possible, see details.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param x A \code{data.frame} in wide or long form, as output by \code{RL_fetch()} and \code{RL_reformat_long()} respectively.
#' @param recode_val A \code{numeric} vector of length 1 or 2 specifying the recode values for minimum and maximum elevation values if they are incorrect. Defaults to \code{c(-99999, 99999)}. \code{NA} is also a suitable input.
#' @param equal_to A \code{logical} input. Should cases where min == max elevation also be recoded? Defaults to \code{TRUE}.
#' @param verbose (optional) A \code{logical} input. Should species with incorrect elevation values be printed out? Defaults to \code{FALSE}.
#' @details \code{recode_val} allows recoding to other values than -99999 and +99999. \code{equal_to} allows changing of min >= max to min > max only.
#' @return A \code{data.frame} in the same format as the input.
#' @export

RL_elevation_check <-
  function(
    x,
    recode_val,
    equal_to = TRUE,
    verbose = FALSE
  ){
    # check inputs
    assertthat::assert_that(
      assertthat::is.flag(verbose),
      assertthat::is.flag(equal_to)
    )
    assertthat::assert_that(
      missing(recode_val) ||
      ((is.numeric(recode_val) || is.na(recode_val)) &&
       length(recode_val) %in% c(1,2)),
      msg = "`recode_val` should either be a numeric vector of length 1 or 2, or NA"
    )
    if(missing(recode_val)){
      recode_val <- c(-99999, 99999)
    }else if(length(recode_val) ==1L){
      recode_val <- rep(recode_val, 2)
    }

    # choose comparator
    if(equal_to){
      comparator <- `<=`
    }else{
      comparator <- `<`
    }

    # get vals from data
    min_alt <-
      x$min_alt %>%
      as.character() %>%
      as.numeric()
    max_alt <-
      x$max_alt %>%
      as.character() %>%
      as.numeric()

    # get delta
    delta_alt <-
      max_alt - min_alt


    alts_to_change <-
      which(comparator(delta_alt, 0))

    if(length(alts_to_change) >0L){
      if(verbose == TRUE){
        species_to_change <-
          x[alts_to_change,c("iucn_id", "binomial")]
        message("The following species have had elevations changed:\n")
        print(species_to_change)
      }

      x$min_alt[alts_to_change] <- recode_val[1]
      x$max_alt[alts_to_change] <- recode_val[2]
    }else{
      if(verbose == TRUE){
        message("No species have had elevations changed.")
      }
    }
    return(x)
  }
