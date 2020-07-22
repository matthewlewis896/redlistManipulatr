#' Recode NA seasons to Resident
#' @description Recodes seasons coded on the Red List as NA (and pulled down by RL_fetch() as 999) to Resident (1).
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @param combine.rows (optional) Logical. If a species ends up with multiple rows for a season due to recoding, should these rows be combined? If TRUE selects the highest ranked suitability-major importance combination. See \code{suitability_ordered}.Defaults to FALSE.
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_season_recode <-
  function(
    x,
    combine.rows = FALSE
  ){
    sp <-
      x$iucn_id %>%
      unique()

    sp_999 <-
      x$iucn_id[x$season == 999] %>%
      unique()

    x$season[x$season == 999] <- 1

    if(combine.rows == TRUE){
      output <- x[0,]
      for(i in 1:length(sp)){
        species <- sp[i]
        temp <- x[x$iucn_id == species,]

        if(species %in% sp_999){
          if(nrow(temp[temp$season == 1,]) >1){
            t1 <- temp[temp$season != 1,]
            t2 <- temp[temp$season == 1,]

            t3 <- t2[1,]
            hab_cols <-
              hab_col_positions() %>%
              unlist() %>%
              sort() %>%
              as.vector()

            pref_order = NatureMapRedList::suitability_ordered

            for(j in hab_cols){
              vals <- t2[,j]
              vals <- vals[!is.na(vals)]
              if(length(vals) >0L){
                best_val <-
                  vals %>%
                  lapply(.,
                         function(z){
                           which(pref_order == z)
                         }) %>%
                  unlist() %>%
                  which.min()
                t3[1, j] <- vals[best_val]
              }

            }
            temp <-
              rbind(
                t1,
                t3
              )
            temp <- temp[order(temp$season),]


          }
        }

        output <-
          rbind(
            output,
            temp
          )
      }
    }else{
      output <- x
    }

    row.names(output) <- 1:nrow(output)

    return(output)
  }
