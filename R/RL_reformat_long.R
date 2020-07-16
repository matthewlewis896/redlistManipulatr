#' Reformat wide format data to long format
#' @description Reformat wide format data (they way RL_fetch() retrieves data) to long format.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch() or RL_code_fill().
#' @param na.rm (optional). Logical. Remove NA rows to compress output. See Details. Defaults to TRUE.
#' @details If \code{na.rm} is \code{TRUE} (the default) then rows lacking habitat information will be removed. On pivoting, all habitat columns are retained - even those with no information - meaning that the output has many NA rows where a species is not found at all in a habitat (e.g. polar bears in Hot Desert). This is normally undesirable, so these rows are removed by default. If you would like to keep these rows then set \code{na.rm} to \code{FALSE}.
#' @return A dataframe in long format (one row per habitat type).
#' @export

RL_reformat_long <-
  function(
    x,
    na.rm = TRUE
    ){
    if(!is.logical(na.rm)){
      stop("Please supply a valid value for 'na.rm'.")
    }

    NatureMapRedList::packTest("tidyr")

    habitats <- NatureMapRedList::habitats
    seasons <- NatureMapRedList::seasons
    suitability <- NatureMapRedList::suitability
    major_importance <- NatureMapRedList::major_importance

    df <-
      x %>%
      tidyr::pivot_longer(
        cols =
          NatureMapRedList::hab_col_positions() %>%
          unlist() %>%
          sort(),
        names_to = "level_3",
        values_to = "suitability"
      )

    if(na.rm == TRUE){
      df <- df[!is.na(df$suitability),]
    }

    df$level_3 <-
      df$level_3 %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    vals <-
      df$suitability %>%
      lapply(.,
             function(x){
               if(!is.na(x)){
                 x <-
                   x %>%
                   strsplit("") %>%
                   unlist()
                 maj_imp <- x[length(x)]
                 suit <- x[1:(length(x)-1)]

                 x <-
                   c(
                     suit,
                     maj_imp
                   )
               }
              return(x)
             })

    df$season_text <- df$suitability_text <- df$major_importance <- df$major_importance_text <- df$level_1 <- df$level_2 <- NA
    for(i in 1:nrow(df)){
      # level1, 2, 3
      if(df$level_3[i] %in% habitats$Level1){
        df$level_1[i] <- df$level_3[i]
        df$level_2[i] <- df$level_3[i] <- NA
      }else if(df$level_3[i] %in% habitats$Level2){
        df$level_1[i] <- habitats$Level1[which(habitats$Level2 == df$level_3[i])][1]
        df$level_2[i] <- df$level_3[i]
        df$level_3[i] <- NA
      }else{
        df$level_1[i] <- habitats$Level1[which(habitats$Level3 == df$level_3[i])][1]
        df$level_2[i] <- habitats$Level2[which(habitats$Level3 == df$level_3[i])][1]
      }


      # season text
      if(df$season[i] == 999){
        df$season_text[i] <- NA
      }else{
        df$season_text[i] <-
          seasons$Seasonality[seasons$Code == df$season[i]]
      }
      #major importance & suitability
      if(all(!is.na(vals[[i]]))){
        df$suitability[i] <- vals[[i]][1]
        df$major_importance[i] <- vals[[i]][2]
      }

      #suitability text
      if(is.na(df$suitability[i])){
        df$suitability_text[i] <- NA
      }else if(df$suitability[i] %in% c(999, 6, 4)){
        df$suitability_text[i] <- NA
      }else{
        df$suitability_text[i] <-
          suitability$Name[suitability$Code == df$suitability[i]]
      }

      #major importance text
      if(is.na(df$major_importance[i])){
        df$major_importance_text[i] <- NA
      }else if(df$major_importance[i] == 3){
        df$major_importance_text[i] <- NA
      }else{
        df$major_importance_text[i] <-
          major_importance$Major_Importance[major_importance$Code == df$major_importance[i] &
                                !is.na(major_importance$Code)]
      }

    }

    df <-
      df[,c(colnames(df)[1:which(colnames(df) == "max_alt")],
            "season_text","level_1","level_2", "level_3","suitability","suitability_text","major_importance","major_importance_text")]

    return(df)
  }
