#' Moving window minima
#'
#' Function to determine minimum growth with a moving window (Büntgen et al 2009) 
#' See also: Büntgen, U, Frank, D, Liebhold, A, Johnson, D, Carrer, M, Urbinati, C, Grabner, M, Nicolussi, K, Levanic, T and Esper, J. 2009 Three centuries of insect outbreaks across the European Alps. New Phytologist 182(4): 929–941. DOI: https://doi.org/10.1111/j.1469-8137.2009.02825.x.
#'
#' @param trw tree ring data frame (as from dplR)
#' @param window_size lenght of the moving window
#'
#' @returns a TRUE/FALSE dataframe with the years that are the minimum of the moving window as TRUE
#'
#' @examples
#' 
#'   moving_window_minimum(treeringseries, window_size = 5)
#'
#' @author Ronald Visser
#'
#' @export moving_window_minimum
#' 

moving_window_minimum <- function(trw, window_size = 3) {
  trw_minima <- trw
  trw_minima[] <- FALSE
  mid_window_add <- floor(window_size / 2)
  for (i in 1:ncol(trw)) {
    for (j in 1:(nrow(trw)-window_size)) {
      if (!is.na(trw[j,i])) {
        min_window <- min(trw[j:(j+window_size-1),i], na.rm =TRUE)
        if (min_window == trw[j + mid_window_add,i] && !is.na(trw[j + mid_window_add,i])) {
          trw_minima[j + mid_window_add, i] <- TRUE
        }
      }
    }
  }
  trw_minima
}

