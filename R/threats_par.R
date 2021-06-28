#' Fetch Red List threats data (parallel)
#' @description Download Red List threats data from the Red List API.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x Vector of species names or IDs.
#' @param query Character string. 'name' if querying by species name; 'ID' if querying by IUCN Red List ID.
#' @param key Character string. IUCN Red List API key - available from https://apiv3.iucnredlist.org/api/v3/token
#' @param sleep_dur (optional) Numeric. Duration of sleep between API calls in seconds. Defaults to 2 seconds.
#' @param subset (optional). Numeric. Specify proportion of x to run for between 0 and 1. Defaults to 1.
#' @param num.cores (optional) Numeric. Specify number of cores to use if running in parallel. Default is number of CPU cores available - 1.
#' @param verbose (optional) Numeric. If 0 gives no progress update, if 1 prints progress bar, if 2 prints 1 row per task completed. Default is 1.
#'
#' @return A dataframe in long format (one row per species-threat)

threats_par <-
  function(x,
           key,
           query,
           subset,
           sleep_dur,
           num.cores,
           verbose) {


    if (verbose == 2){
      progress <-
        function(n)
          cat(sprintf("task %d is complete\n", n))#print when each row is complete
      opts <- list(progress = progress)
    } else if (verbose == 1){
      pb <- txtProgressBar(style = 3, max = length(subset))
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    }else{
      opts <- NULL
    }

    invisible(
      lapply(
        c("parallel", "snow", "doSNOW"),
        packTest
      )
    )

    cl <-
      snow::makeCluster(
        num.cores,
        outfile = ""
      )
    doSNOW::registerDoSNOW(cl)
    on.exit(snow::stopCluster(cl))

    temp_df <- threatform()

    ret <- foreach::foreach (
      i = subset,
      .combine = rbind,
      .options.snow = opts,
      .packages = c("rredlist")#packages needing maintaining in this loop
    ) %dopar% {

      df <- temp_df
      #what species we after
      species <- as.character(x[i])
      #trying with an error phase if needed
      df <- tryCatch({
        if (query == "name") {
          habs <- rredlist::rl_threats(name = species, key = key)

        } else if (query == "ID") {
          habs <-
            rredlist::rl_threats(id = as.numeric(as.character(species)), key = key)
        }
        if(length(habs$result)>0){
          df<-df[1:nrow(habs$result),]
          df[,3:4] <- habs$result[,1:2]
          df <- df[!is.na(df[,3]),]
          for(j in 1:nrow(df)){
            code <-
              df[j,3] %>%
              strsplit("[.]") %>%
              unlist()
            for(k in 1:length(code)){
              df[j,k+4] <- paste(code[1:k], collapse = ".")
            }
          }
          df[,8:length(df)] <- habs$result[,3:length(habs$result)]
        }else{
          df<-df[1,]
          df$code <- "999"
        }
        if(length(habs$name) >0L){
          df[,2] <- unlist(habs$name)
        }

        if(query == "ID"){
          df[,1] <- species
        }

        df<- df
      },
      error = function(err) {
        df<-df[1,]
        df$code <- "999"

        if (query == "name") {
          df$binomial = species
        } else if (query == "ID") {
          df$iucn_id = species
        }
        df <- df
      })
      Sys.sleep(sample(rnorm(100, sleep_dur, 0.1), 1))
      df
    }
    df <- do.call("cbind", ret)
    return(df)
  }
