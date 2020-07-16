#' Level 1 and Level2 habitat column positions
#' @description Work out positions of level 1 and 2 columns from wide form data frame
#' @keywords internal
#'
#' @param x Dataframe with column names in the form iucn_n or iucn_n_n where n is an integer habitat code
#' @return A list of column positions for level 1 and level 2 habitat categories

hab_col_positions <-
  function(
    x = wideform()
  ){
    cols_to_check <- which(colnames(x) == "iucn_1"):length(x)
    level1_cols <-
      colnames(x)[cols_to_check] %>%
      lapply(.,
             function(x){
               x <-
                 x %>%
                 strsplit("_") %>%
                 unlist()
               if(length(x) == 2){
                 return(1)
               }else{
                 return(NA)
               }
             }) %>%
      unlist()
    level1_cols <-
      cols_to_check[!is.na(level1_cols)] %>%
      as.list()

    level2_cols <- list()
    for(i in 1:length(level1_cols)){
      if(i < length(level1_cols)){
        level2_cols[[i]] <- cols_to_check[cols_to_check > level1_cols[i] &
                                            cols_to_check < level1_cols[i+1]]
      }else{
        level2_cols[[i]] <- integer(0)
      }
    }

    all_hab_cols <-
      list(
        Level1 = level1_cols,
        Level2 = level2_cols
      )
  }
