#' Rename Habitat Columns to New Names (GEE use)
#' @description Rename columns to new names - e.g. iucn_1_1 becomes 110.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_col_rename <-
  function(
    x
  ){
    lut <-
      NatureMapRedList::hab_conversion_lut

    cols_to_check <-
      NatureMapRedList::hab_col_positions() %>%
      unlist() %>%
      sort()

    old_colnames <-
      colnames(x)[cols_to_check] %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    new_colnames <-
      lut$NewCode[lut$IUCNLevel %in% old_colnames]

    colnames(x)[cols_to_check] <- new_colnames

    return(x)
  }
