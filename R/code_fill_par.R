#' Fill Out Level 1 or 2 Habitat Codes
#' @description Optionally fill out Level 2 habitat codes if Level 1 codes exist and vice versa.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x A wide format dataframe with one column per habitat category. As output by RL_fetch().
#' @param level1.recode (optional) Logical. If Level 2 habitat info exists, should the corresponding Level 1 habitat be given the same info? See Details. Default is TRUE.
#' @param level2.recode (optional) Logical. If Level 1 habitat info exists, should the corresponding Level 2 habitats be given the same info? See Details. Default is TRUE.
#' @param subset (optional). Numeric. Specify proportion of x to run for between 0 and 1. Defaults to 1.
#' @param num.cores (optional) Numeric. Specify number of cores to use if running in parallel. Default is number of CPU cores available - 1.
#' @param verbose (optional) Numeric. If 0 gives no progress update, if 1 prints progress bar, if 2 prints 1 row per task completed. Default is 1.
#'
#' @details The \code{level1.recode} parameter will fill upstream Level 1 habitat categories if any Level 2 categories have habitat information. e.g. if habitat category 2.1 (Dry Savanna) is suitable and of major importance (coded as 31), habitat category 2 (Savanna) will be filled with this info too.
#' @details Similarly, \code{level2.recode} will fill all downstream Level 2 habitat categories if the corresponding Level 1 category is filled. e.g. if if habitat category 2 (Savanna) is suitable and of major importance (coded as 31), habitat categories 2.1 (Dry Savanna) and 2.2 (Moist Savanna) will be given the same information.
#' @return A dataframe in wide format (one column per habitat type and one row per species' season).

code_fill_par <-
  function(x,
           level1.recode,
           level2.recode,
           subset,
           num.cores,
           verbose) {
    if (verbose == 2) {
      progress <-
        function(n)
          cat(sprintf("task %d is complete\n", n))#print when each row is complete
      opts <- list(progress = progress)
    } else if (verbose == 1) {
      pb <- txtProgressBar(style = 3)
      progress <- function(n)
        setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    } else{
      opts <- NULL
    }

    invisible(lapply(c("parallel", "snow", "doSNOW"),
                     packTest
                     )
    )

    cl <-
      snow::makeCluster(num.cores,
                        outfile = "")
    doSNOW::registerDoSNOW(cl)
    on.exit(snow::stopCluster(cl))

    hab_cols <- hab_col_positions()
    cols_to_check <- as.vector(sort(unlist(hab_cols)))

    pref_order <- redlistManipulatr::suitability_ordered


    ret <- foreach::foreach(i = 1:subset,
                   .combine = "rbind",
                   .options.snow = opts
                   ) %dopar% {
                     df <- x[i,]
                     #which columns are occupied?
                     col_occupied <- c()
                     for (j in cols_to_check) {
                       levels(df[1, j]) <- c(levels(df[1, j]), as.character(pref_order))
                       if (!is.na(df[1, j])) {
                         col_occupied <- c(col_occupied, j)
                       }
                     }
                     #if there is more than 1
                     if (length(col_occupied) > 0L) {
                       for (j in 1:length(col_occupied)) {
                         if (level2.recode == TRUE) {
                           #if level1 codes occupied fill out level 2
                           if (col_occupied[j] %in% unlist(hab_cols$Level1)) {
                             lvl1_col <- which(unlist(hab_cols$Level1) == col_occupied[j])
                             #if it is 18 we don't need to do anything
                             if (lvl1_col != 18) {
                               for (k in unlist(hab_cols$Level2[lvl1_col])) {
                                 #if it's unoccupied
                                 if (is.na(df[1, k])) {
                                   df[1, k] <- as.character(df[1, col_occupied[j]])
                                 }
                               }
                             }
                           }
                         }
                       }
                       if (level1.recode == TRUE) {
                         for (j in 1:length(hab_cols$Level2)) {
                           if (is.na(df[1, unlist(hab_cols$Level1[j])])) {
                             if (any(unlist(hab_cols$Level2[j]) %in% col_occupied)) {
                               lvl2s <-
                                 col_occupied[which(col_occupied %in% unlist(hab_cols$Level2[j]))]
                               if (length(lvl2s) == 1L) {
                                 df[1, unlist(hab_cols$Level1[j])] <- df[1, lvl2s]
                               } else{
                                 vals <- df[1, lvl2s]
                                 vals_max_priority <-
                                   which.min(unlist(lapply(vals,
                                                           function(x) {
                                                             which(pref_order == x)
                                                           })))

                                 best_val <- vals[vals_max_priority]
                                 df[1, unlist(hab_cols$Level1[j])] <-
                                   best_val
                               }
                             }
                           }
                         }
                       }
                     }
                     df
                   }
    return(ret)
  }
