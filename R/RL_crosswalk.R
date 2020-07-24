#' Crosswalk habitat classes to a look-up table of other values
#' @description Rename columns to new names from look-up table - e.g. iucn_1_1 becomes 110. Then optionally replaces suitability/major importance values with the column name or 1. Typically used to convert between habitat classes and land cover classes.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{dataframe} with one column per habitat category. As output by \code{RL_fetch()}.
#' @param lut (optional) A \code{dataframe} look-up table of values with two columns, one for new values, one for Red List habitat classes. This can be one-to-one, one-to-many, or many-to-many. If many-to-many, the highest suitability-major importance combination is selected as the preferred value for the new value - see \code{suitability_ordered}. See details for default.
#' @param new.code.colname (optional) Character. What is the name of the column of \code{lut} with the new values in? Defaults to being the first column.
#' @param old.code.colname (optional) Character. What is the name of the column of \code{lut} with the old (Red List habitat class) values in? Defaults to being the second column.
#' @param recode (optional) Character. Should values be recoded? "no" gives the most favourable suitability-major importance combinations, "one" codes all habitats with suitability-major importance values as 1, "colname" codes all habitats with suitability-major importance values as the column name value (the same as the new values in lut). Defaults to "no".
#' @details Use should follow that of RL_subset_acceptable() if only some habitat suitabilities/major importances are wanted and cell values are being recoded.
#' @details The default for \code{lut} is the \code{hab_conversion_lut} dataset which just converts habitat classes to integer values - e.g. 1.1 -> 110; 9.8.1 -> 981.
#' @return A \code{dataframe} in wide format (one column per new value of habitat type).
#' @export

RL_crosswalk <-
  function(
    x,
    lut = redlistManipulatr::hab_conversion_lut,
    new.code.colname = NULL,
    old.code.colname = NULL,
    recode = "no"
  ){


    if(is.null(new.code.colname)){
      new.col <- 1
    }else if(!is.character(new.code.colname)){
      stop("Please supply a valid character input for 'new.code.colname'.")
    }else if(!(new.code.colname) %in% colnames(lut)){
      stop("Your input for 'new.code.colname' is not a column name of lut.")
    }else{
      new.col <- which(colnames(lut) == new.code.colname)
    }

    if(is.null(old.code.colname)){
      old.col <- 2
    }else if(!is.character(old.code.colname)){
      stop("Please supply a valid character input for 'old.code.colname'.")
    }else if(!(old.code.colname) %in% colnames(lut)){
      stop("Your input for 'old.code.colname' is not a column name of lut.")
    }else{
      old.col <- which(colnames(lut) == old.code.colname)
    }

    if(!is.character(recode)){
      stop("Please supply a valid character input for 'recode'.")
    }else if(!(recode %in% c("no", "one", "colname"))){
      stop("Please supply a valid character input for 'recode'. 'recode' should be one of 'no', 'one' or 'colname'.")
    }

    lut[,old.col] <-
      lut[,old.col] %>%
      gsub("\\.", "_", .) %>%
      lapply(.,
             function(x){
               paste0("iucn_", x)
               })%>%
      unlist()

    # Change column names
    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    old_colnames <-
      colnames(x)[cols_to_check] %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    new_colnames <-
      lut[,new.col] %>%
      unique()

    output <-
      matrix(
        nrow = nrow(x),
        ncol = ((length(x) - length(old_colnames)) + length(new_colnames))
      ) %>%
      as.data.frame()

    colnames(output) <-
      c(
        colnames(x)[1:(cols_to_check[1]-1)],
        new_colnames
        )

    new_cols <-
      c(
        cols_to_check[1]:(cols_to_check[1] + (length(new_colnames)-1))
      )

    pref_order <- redlistManipulatr::suitability_ordered

    output[,1:(new_cols[1]-1)] <- x[,1:(new_cols[1]-1)]
    if(recode == "no"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), pref_order[which(!(pref_order %in% levels(output[,i])))])
      }
    }else if(recode == "one"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), "1")
      }
    }else if(recode == "colname"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), colnames(output)[i][which(!(colnames(output)[i] %in% levels(output[,i])))])
      }
    }

    for(i in 1:nrow(output)){
      for(j in new_cols){
        new_value <-
          colnames(output)[j]
        competing_cols <-
          lut[lut[,new.col] == new_value, old.col]
        vals <- x[i, which(colnames(x) %in% competing_cols)]
        vals <- vals[!is.na(vals)]
        if(length(vals) == 0L){
          output[i,j] <- NA
        }else{
          if(recode == "one"){
          output[i,j] <- 1
          } else if (recode == "colname") {
            output[i, j] <- new_value
          } else if (recode == "no") {
            if (length(competing_cols) == 1L) {
              output[i, j] <- x[i, which(colnames(x) == competing_cols)]
            } else{
              best_val <-
                vals %>%
                lapply(.,
                       function(z) {
                         which(pref_order == z)
                       }) %>%
                unlist() %>%
                which.min()
              output[i, j] <- vals[best_val]
            }
          }
        }
      }
    }

    return(output)
  }
