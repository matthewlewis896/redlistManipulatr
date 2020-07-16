#' Wide format dataframe for RL_fetch()
#' @description Internal Function. Makes a wide format dataframe to output RL_fetch() into according to the Red List level2 habitats. The number of rows corresponds to the number of seasons coded by the Red List.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param habs Dataframe of Red List habitat categories. Taken from NatureMapRedList::habitats.
#' @param seas Dataframe of Red List season codes Taken from NatureMapRedList::seasons.
#'
#' @return A dataframe in wide format (one column per habitat type and one row per species).


wideform <-
  function(
    habs = NatureMapRedList::habitats,
    seas = NatureMapRedList::seasons
  ){
    habs$Level2[is.na(habs$Level2)] <- habs$Level1[is.na(habs$Level2)]
    habs$Level3[is.na(habs$Level3)] <- habs$Level2[is.na(habs$Level3)]
    habs <- habs[, "Level3"]
    habs <- unique(habs)

    df <-
      as.data.frame(matrix(ncol = length(habs) + 8, nrow = nrow(seas)))

    hab_names <- c()
    for (i in 1:length(habs)) {
      name <-
        paste(unlist(strsplit(habs[i], split = "[.]")), collapse = "_")
      hab_names <- c(hab_names, paste("iucn", name, sep = "_"))
    }
    colnames(df) <-
      c(
        "iucn_id",
        "season",
        "kingdom",
        "class",
        "binomial",
        "iucn_category",
        "min_alt",
        "max_alt",
        hab_names
      )
    return(df)
  }
