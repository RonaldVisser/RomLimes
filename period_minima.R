#' Detecting periodicity
#'
#' @param trw_minima data frame with the minima resulting from the moving_window_minimum() function
#'
#' @return data frame with the distance between two subsequent minima. 
#' @export period_minima
#' 
#' @author Ronald Visser
#'
#' @examples
#' trw_wind5 <- moving_window_minimum(trw, window_size = 5)
#' trw_per_min <- period_minima(trw_wind5)
#' 
period_minima  <- function(trw_minima) {
  years <- as.integer(row.names(trw_minima))
  if (ncol(trw_minima) > 1 ) {
    for (i in 1:ncol(trw_minima)) {
      minima_yrs_tmp <- years[unlist(trw_minima[i])]
      nr_minima <- length(minima_yrs_tmp)
      period_min_temp <- minima_yrs_tmp[2:nr_minima]-minima_yrs_tmp[1:nr_minima-1]
      if (i==1) {
        period_min <- as.data.frame(period_min_temp)
      } else {
        max_l <- max(nrow(period_min), nrow(period_min_temp))
        period_min[nrow(period_min) + max_l-nrow(period_min),] <- NA
        length(period_min_temp) <- max_l
        period_min <- cbind(period_min, period_min_temp)
      }
    }
    colnames(period_min) <- colnames(trw_minima)
  } else { 
    if (ncol(trw_minima) == 1 ) {
      minima_yrs_tmp <- years[unlist(trw_minima[i])]
      nr_minima <- length(minima_yrs_tmp)
      period_min <- as.data.frame(minima_yrs_tmp[2:nr_minima]-minima_yrs_tmp[1:nr_minima-1])
      colnames(period_min) <- colnames(trw_minima)
    }
  }
  period_min
}