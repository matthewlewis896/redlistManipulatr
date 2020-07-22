#' Obtain list of all species on Red List
#' @description Obtain a list and basic info about all species on the Red List. This is a wrapper function for rredlist::rl_sp() used for all species.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param key Character string. IUCN Red List API key - available from https://apiv3.iucnredlist.org/api/v3/token
#' @param retain.taxonomic.info (optional) Character. Options to retain all of the taxonomic info columns ("all"), only Kingdom and Class ("some"), or none ("none"). If "none", species binomial name is still provided. Defaults to "some".
#' @param retain.population (optional) Logical. Retain the population information? Defaults to FALSE.
#' @param retain.category (optional) Logical. Retain the assessment category? Defaults to TRUE.
#' @return A dataframe with one row per species containing IUCN ID (column taxonid), taxonomic information, and Red List category.
#' @export

RL_sp_list <-
  function(
    key,
    retain.taxonomic.info = FALSE,
    retain.population = FALSE,
    retain.category = TRUE
  ){
    packTest("rredlist")

    if(!is.character(retain.taxonomic.info)){
      stop("Please supply a valid logical input for 'retain.taxonomic.info'.")
    }else if(!(retain.taxonomic.info) %in% c("all", "some", "none")){
      stop("Please supply a valid logical input for 'retain.taxonomic.info'.")
    }
    if(!is.logical(retain.population)){
      stop("Please supply a valid logical input for 'retain.population'.")
    }
    if(!is.logical(retain.category)){
      stop("Please supply a valid logical input for 'retain.category'.")
    }

    message("Using Red List version ", rredlist::rl_version(key = key))

    out <- rredlist::rl_sp(
      all = TRUE,
      key = key
      )

    species_df <-
      do.call(
        rbind,
        lapply(
          out,
          "[[",
          "result"
          )
        ) %>%
      as.data.frame()

    if(retain.taxonomic.info == "some"){
      species_df <- species_df[,-c(3, 5:7, 9:10)]
    }else if(retain.taxonomic.info == "none"){
      species_df <- species_df[,-c(2:7, 9:10)]
    }

    if(retain.population == FALSE){
      species_df <- species_df[,-which(colnames(species_df) == "population")]
    }

    if(retain.category == FALSE){
      species_df <- species_df[,-which(colnames(species_df) == "category")]
    }

    colnames(species_df)[colnames(species_df) == "scientific_name"] <- "binomial"

    return(species_df)
  }
