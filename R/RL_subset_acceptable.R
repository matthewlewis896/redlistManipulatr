#' Subset to acceptable suitabilities and major importances of habitats
#' @description Subset data to only retain desired suitabilities and major importances. Other suitabilities/major importances are recoded as NA. Rows containing only NA values can then be removed.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @param acceptable_suitabilities (optional) A vector of suitabilities deemed acceptable. Must be in the form \code{c(3,2)}. See details. Defaults to accepting all.
#' @param acceptable_importances (optional) A vector of major importances deemed acceptable. Must be in the form \code{c(1,3)}. See details. Defaults to accepting all.
#' @param na.rm (optional) Logical. If a row has all habitats recoded to NA, should it be removed? Default is TRUE.
#' @details See \code{suitability} and \code{major_importance} for code values corresponding to each suitability/major importance name.
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_subset_acceptable <-
  function(
    x,
    acceptable_suitabilities = NA,
    acceptable_importances = NA,
    na.rm = TRUE
  ){
    if(!is.logical(na.rm)){
      stop("Please provide a valid logical input for 'na.rm'.")
    }

    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    if(!is.na(acceptable_suitabilities) |
       !is.na(acceptable_importances)
       ){
      rows_to_remove <- c()
      for(i in 1:nrow(x)){
        vals <- x[i, cols_to_check]
        vals <-
          vals %>%
          lapply(.,
                 function(y){
                   if(!is.na(y)){
                     y <-
                       y %>%
                       strsplit("") %>%
                       unlist()
                     maj_imp <- y[length(y)]
                     suit <-
                       y[1:(length(y)-1)] %>%
                       paste(collapse = "")

                     allow = 0
                     if(!is.na(acceptable_suitabilities)){
                       if(suit %in% acceptable_suitabilities){
                         allow = allow + 1
                       }
                     }else{
                       allow = allow + 1
                     }
                     if(!is.na(acceptable_importances)){
                       if(maj_imp %in% acceptable_importances){
                         allow = allow + 1
                       }
                     }else{
                       allow = allow + 1
                     }

                     if(allow == 2){
                       y <- paste0(suit, maj_imp)
                     }else{
                       y <- NA
                     }
                   }
                   return(y)
                 }) %>%
          unlist()
        if(all(is.na(vals))){
          rows_to_remove <- c(rows_to_remove, i)
        }
        x[i, cols_to_check] <- vals
      }
      if(na.rm == TRUE &
         length(rows_to_remove) >0L){
        x <- x[-rows_to_remove,]
      }
    }
    return(x)
  }
