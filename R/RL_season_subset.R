#' Subset to species-season combinations
#' @description Subset wide format data to species-season combinations. Also allows copying of data for missing desired seasons
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @param season_df A dataframe consisting of a column for species and a column for season. A species with multiple seasons desired should have one row per season. See Details.
#' @param query Character. Are species represented by names ("name") or IUCN IDs ("ID")?
#' @param species.col.name (optional) Character. What column name of \code{season_df} are the species names/IDs found in. Defaults to the first column of the dataframe.
#' @param season.col.name (optional) Character. What column name of \code{season_df} are the seasons found in. Defaults to the second column of the dataframe.
#' @param fill.missing.seasons (optional) Vector. If seasons are present in \code{season_df} but not on the Red List, should data be copied from seasons which are present and if so which? Acceptable values are season codes - see \code{seasons}. Defaults to NA (no copying).
#' @param retain.na.seasons (optional) Logical. Some seasons are coded as NA on the Red List. RL_fetch() pulls these down as 999. Would you like to retain these seasons? Defaults to FALSE.
#' @param retain.missing.sp (optional) Logical. If a species is not found in season_df, should it be retained? Defaults to TRUE.
#' @details \code{season_df} should have seasons coded numerically as per \code{NatureMapRedList::seasons$Code}.
#' @return A dataframe in wide format (one column per habitat type).
#' @export

RL_season_subset <-
  function(
    x,
    season_df,
    query,
    species.col.name = NA,
    season.col.name = NA,
    fill.missing.seasons = NA,
    retain.na.seasons = FALSE,
    retain.missing.sp = TRUE
  ){
    if(!is.character(query)){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input.")
    }else if(!(query %in% c("name", "ID"))){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input of 'name' or 'ID'.")
    }else if(query =="name"){
      query = "binomial"
    }else{
      query = "iucn_id"
    }

    if(is.na(species.col.name)){
      species.col = 1
    }else{
      if(!is.character(species.col.name)){
        stop("Please supply a valid character input for 'species.col.name'.")
      }
      species.col = which(colnames(season_df) == species.col.name)
    }
    if(is.na(season.col.name)){
      season.col = 2
    }else{
      if(!is.character(season.col.name)){
        stop("Please supply a valid character input for 'season.col.name'.")
      }
      season.col = which(colnames(season_df) == season.col.name)
    }
    if(!is.logical(retain.missing.sp)){
      stop("Please supply a valid logical input for 'retain.missing.sp'.")
    }
    if(!is.logical(retain.na.seasons)){
      stop("Please supply a valid logical input for 'retain.na.seasons'.")
    }

    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    pref_order <- NatureMapRedList::suitability_ordered

    output <-
      x[0,]

    for(i in 1:nrow(season_df)){
      species <- season_df[i,species.col]
      seas <- season_df[i,season.col]

      temp <- x[x[,query] == species &
                x$season == seas,]
      #copy from other seasons if no data for desired season
      if(all(nrow(temp) == 0L &
         all(!is.na(fill.missing.seasons)))){
        other_seasons <-
          x[x[,query] == species &
            x$season %in% fill.missing.seasons,]

        if(nrow(other_seasons) >0L){
          temp <- other_seasons[1,]
          temp$season <- seas
          if(nrow(other_seasons) > 1L){
            for(j in cols_to_check){
              vals <- other_seasons[,j]
              vals <- vals[!is.na(vals)]
              best_val <-
                vals %>%
                lapply(.,
                       function(z){
                         which(pref_order == z)
                       }) %>%
                unlist() %>%
                which.min()
              temp[1, j] <- vals[best_val]
            }
          }
        }else{
          temp <- x[1,]
          temp[1,] <- NA
          temp[1, query] <- species
          temp[1,"season"] <- seas
        }
      }
      if(retain.na.seasons == TRUE){
        temp <-
          rbind(
            temp,
            x[x[,query] == species &
                x$season == 999,]
          )
      }
      output <- rbind(output, temp)
    }
    if(retain.missing.sp == TRUE){
      missing_sp <- x[!(x[,query] %in% unique(season_df[,species.col])),]
      output <- rbind(output, missing_sp)
    }
    return(output)
  }
