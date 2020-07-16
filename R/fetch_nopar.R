#' Fetch Red List data (non-parallel)
#' @description Download Red List habitat and elevation data from the Red List API.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x Vector of species names or IDs.
#' @param query Character string. 'name' if querying by species name; 'ID' if querying by IUCN Red List ID.
#' @param key Character string. IUCN Red List API key - available from https://apiv3.iucnredlist.org/api/v3/token
#' @param sleep_dur (optional) Numeric. Duration of sleep between API calls in seconds. Defaults to 2 seconds.
#' @param subset (optional). Numeric. Specify proportion of x to run for between 0 and 1. Defaults to 1.
#' @param verbose (optional) Numeric. If 0 gives no progress update, if 1 prints progress bar, if 2 prints 1 row per task completed. Default is 1.
#'
#' @return A dataframe in wide format (one column per habitat type and one row per species' season).

fetch_nopar <-
  function(x,
           key,
           query,
           subset,
           sleep_dur,
           verbose) {



    temp_df <- NatureMapRedList::wideform()
    seasons <- NatureMapRedList::seasons
    suitability <- NatureMapRedList::suitability
    major_importance <- NatureMapRedList::major_importance

    if(verbose == 1){
      pb = txtProgressBar(min = 0, max = subset, initial = 0, style = 3)
    }

    ret <- foreach::foreach (
      i = 1:subset,
      .combine = rbind,
      .packages = c("rredlist")#packages needing maintaining in this loop
    ) %do% {
      #Let's see how many of 5 seasons we can fill - output df in the form we want
      df <- temp_df
      #what species we after
      species <- as.character(x[i])
      #trying with an error phase if needed
      df <- tryCatch({
        if (query == "name") {
          habs <- rredlist::rl_habitats(name = species, key = key)

        } else if (query == "ID") {
          habs <-
            rredlist::rl_habitats(id = as.numeric(as.character(species)), key = key)
        }
        if(length(habs$result)>0){

          habs$result$season <- tolower(as.character(habs$result$season))
          habs$result$season[habs$result$season == "unknown"] <- "seasonal occurrence unknown"

          habs$result$suitability <- tolower(as.character(habs$result$suitability))

          habs$result$season[is.na(habs$result$season)] = 999
          habs$result$suitability[is.na(habs$result$suitability)] = 999

          #reformatting season
          for(j in 1:length(habs$result$season)){
            season_code <-
              seasons$Code[tolower(seasons$Seasonality) == tolower(habs$result$season[j])]

            if(any(is.na(season_code))){
              next()
            }else if(length(season_code) == 0L){
              next()
            }else{
              habs$result$season[j] <- season_code
            }

          }

          #reformatting suitability
          for(j in 1:length(habs$result$suitability)){
            suit_code <-
              suitability$Code[tolower(suitability$Name) == tolower(habs$result$suitability[j])]

            if(any(is.na(suit_code))){
              next()
            }else if(length(suit_code) == 0L){
              next()
            }else{
              habs$result$suitability[j] <- suit_code
            }

          }

          # Add codes for major importance
          for (j in 1:nrow(habs$result)){
            mi_code <-
              major_importance$Code[tolower(major_importance$Major_Importance) == tolower(habs$result$majorimportance[j])][1] # [1] deals with NA
            if(is.na(mi_code)){
              habs$result$majorimportance[j] <- major_importance$Code[is.na(major_importance$Major_Importance)]
            }else{
              habs$result$majorimportance[j] <- mi_code
            }
            habs$result$suitability[j] <-
              paste0(
                habs$result$suitability[j],
                habs$result$majorimportance[j]
                )
          }

          habs$result <-
            habs$result[order(habs$result$season), ]

          #adding habitat suitability values to relevant columns
          for (j in 1:length(unique(habs$result$season))) {
            this.season <- as.numeric(as.character(unique(habs$result$season)[j]))
            df[j, 2] = this.season
            temp <-
              habs$result[which(as.numeric(as.character(habs$result$season)) ==
                                  this.season), ]
            for (k in 1:nrow(temp)) {
              hab_column <-
                paste("iucn", paste(unlist(
                  strsplit(temp$code[k], split = "[.]")
                ), collapse = "_"), sep = "_")
              df[j, hab_column] = temp$suitability[k]
            }
          }
        }else{
          df<-df[1,]
          df[,2]=999
          df[,unlist(parent_columns)[1]:unlist(parent_columns)[length(unlist(parent_columns))]] <-
            66
        }

        #and now the rest
        #brief sleep
        Sys.sleep(sample(rnorm(100, sleep_dur, 0.1), 1))
        if (query == "name") {
          elevation <- rredlist::rl_search(name = species, key = key)
        } else if (query == "ID") {
          elevation <-
            rredlist::rl_search(id = as.numeric(as.character(species)), key = iucn_api_key)
        }
        if(length(elevation$result)>0){
          df$iucn_id = as.numeric(as.character(elevation$result$taxonid[1]))
          df$kingdom = as.character(elevation$result$kingdom[1])
          df$class = as.character(elevation$result$class[1])
          df$binomial = as.character(elevation$result$scientific_name[1])
          df$min_alt = as.numeric(as.character(elevation$result$elevation_lower[1]))
          df$max_alt = as.numeric(as.character(elevation$result$elevation_upper[1]))
          df$iucn_category=as.character(elevation$result$category[1])
        }else{
          if (query == "name") {
            df$binomial = species
          } else if (query == "ID") {
            df$iucn_id = species
          }
        }
        #if NA lets make it clear that's because of a lack of data
        if (is.na(df$min_alt[1])) {
          df$min_alt = -99999
        }
        if (is.na(df$max_alt[1])) {
          df$max_alt = 99999
        }
        #to stop the redlist api getting annoyed
        df <- df[which(!is.na(df$season)),, drop = F]
        df <- df
      },
      error = function(err) {
        df<-df[1,]
        #but if this fails then just fill all the hab columns with 6s
        df[,which(colnames(df) == "iucn_1_1"):length(df)] <-
          66
        if (query == "name") {
          df$binomial = species
        } else if (query == "ID") {
          df$iucn_id = species
        }
        df$min_alt=-99999
        df$max_alt=99999
        df[,2]=999
        df <- df
      })
      Sys.sleep(sample(rnorm(100, sleep_dur, 0.1), 1))
      if(verbose == 1){
        setTxtProgressBar(pb,i)
      }else if(verbose ==2){
        print(paste("task", i, "is complete"))
      }

      df
    }
    df <- do.call("cbind", ret)
    return(df)
  }
