#' Column names to values
#' @description Replaces suitability/major importance values with the column name. Use should follow that of RL_col_rename() and RL_subset_acceptable() to ready for GEE output.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_value_replace <-
  function(
    x
  ){
    cols_to_check <-
      NatureMapRedList::hab_col_positions() %>%
      unlist() %>%
      sort()

    for(i in cols_to_check){
      levels(x[,i]) <- c(levels(x[,i]), colnames(x)[i])
      x[!is.na(x[,i]), i] <- colnames(x)[i]
    }

    return(x)
  }
