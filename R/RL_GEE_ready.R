#' Change wide form habitat column names and give cells their value (for GEE)
#' @description Rename columns to new names - e.g. iucn_1_1 becomes 110. Then replaces suitability/major importance values with the column name. Use should follow that of RL_subset_acceptable() to ready for GEE output.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_GEE_ready <-
  function(
    x
  ){
    # Change column names
    lut <-
      NatureMapRedList::hab_conversion_lut

    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    old_colnames <-
      colnames(x)[cols_to_check] %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    new_colnames <-
      lut$NewCode[lut$IUCNLevel %in% old_colnames]

    colnames(x)[cols_to_check] <- new_colnames

    # copy values
    for(i in cols_to_check){
      levels(x[,i]) <- c(levels(x[,i]), colnames(x)[i])
      x[!is.na(x[,i]), i] <- colnames(x)[i]
    }

    return(x)
  }
