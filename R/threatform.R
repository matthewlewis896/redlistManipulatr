#' Long format dataframe for RL_threats
#' @description Internal Function. Makes a long format dataframe to output RL_threats() into according to the Red List threats.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param threats Dataframe of Red List threat categories. Taken from redlistManipulatr::threats
#'
#' @return A dataframe in long format (one row per threat-species combination).


threatform <-
  function(
    threats = redlistManipulatr::threats
  ){
    threats$Level2[is.na(threats$Level2)] <- threats$Level1[is.na(threats$Level2)]
    threats$Level3[is.na(threats$Level3)] <- threats$Level2[is.na(threats$Level3)]
    threats <- threats[, "Level3"]
    threats <- unique(threats)

    df <-
      as.data.frame(matrix(nrow = length(threats),
                           ncol = 12))

    colnames(df) <-
      c(
        "iucn_id",
        "binomial",
        "code",
        "title",
        "level1_code",
        "level2_code",
        "level3_code",
        "timing",
        "scope",
        "severity",
        "score",
        "invasive"
      )
    return(df)
  }
