#' Recode NA suitabilities to Suitable
#' @description Recodes habitats coded on the Red List as NA (and pulled down by RL_fetch() as 999) to Suitable (3).
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_suit_recode <-
  function(
    x
  ){
    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()
    for(i in 1:nrow(x)){
      for(j in cols_to_check){
        val <-
          x[i,j]
        if(!is.na(val)){
          val <-
            val %>%
            strsplit("") %>%
            unlist()
          suit <-
            paste0(
              val[1:(length(val)-1)],
              collapse=""
            )
          if(suit==999){
            x[i,j] <- paste0("3", val[length(val)])
          }
        }
      }
    }
    return(x)
  }
