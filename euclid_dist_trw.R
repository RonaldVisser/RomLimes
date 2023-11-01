#' Euclidean distances of tree ring series
#'
#' Function to determine the Euclidean distances of a rwl-object (see dplR-package)
#'
#' @param trw tree ring data frame (as from dplR)
#' @param last_digit_radius set to TRUE is the last digit of the series name is the radius. This enables easy comparison of multiple radii on a single sample
#'
#' @returns a dataframe with several columns: series_A, series_B, min_ed (minimal Euclidean distance), max_ed (maximal Euclidean distance), sum_ed (total Euclidean distance), mean_ed (mean or average Euclidean distance), stdev_ed (standard deviation of the Euclidean distance)
#'
#' @examples
#' euclid_dist_trw(trw_series)
#'
#' @author Ronald Visser
#'
#' @export euclid_dist_trw

euclid_dist_trw <- function(trw, last_digit_radius = FALSE ) {
  n_series <- ncol(trw)
  for (i in 1:(n_series-1)) {
    diff_data <- abs(as.matrix(trw[(i+1):n_series]-trw[,i]))
    diff_data <- diff_data[!is.na(trw[,i]), , drop = FALSE] # reduce data.frame to the overlapping years
    diff_data <- diff_data[, !apply(is.na(diff_data), 2, all), drop = FALSE] # remove series with no overlap
    series_b <- colnames(diff_data)
    series_a <- rep(colnames(trw)[i],length(series_b))
    n_overlap <- matrixStats::colCounts(diff_data, na.rm = TRUE)
    mean_ed <- matrixStats::colMeans2(diff_data, na.rm = TRUE)
    sum_ed <- matrixStats::colSums2(diff_data, na.rm = TRUE)
    stdev_ed <- matrixStats::colSds(diff_data, na.rm = TRUE)
    max_ed <- matrixStats::colMaxs(diff_data, na.rm = TRUE)
    min_ed <- matrixStats::colMins(diff_data, na.rm = TRUE)
    temp_df <- data.frame(series_a, series_b, n_overlap, min_ed, max_ed, sum_ed, mean_ed, stdev_ed)
    colnames(temp_df) <- c("series_a", "series_a", "n_overlap", "min_ed", "max_ed", "sum_ed", "mean_ed", "stdev_ed")
    if (i==1) {
      euclid_dist_list <- temp_df
    } else {
      euclid_dist_list <- rbind(euclid_dist_list, temp_df)
    }
    print(i)
  }
  # create list with distances (duplicates but for directed combinations)
  euclid_dist_list2 <- euclid_dist_list[c("series_a", "series_a", "n_overlap", "min_ed", "max_ed", "sum_ed", "mean_ed", "stdev_ed")]
  colnames(euclid_dist_list2) <- c("series_a", "series_a","n_overlap", "min_ed", "max_ed", "sum_ed", "mean_ed", "stdev_ed")
  euclid_dist_list <- rbind(euclid_dist_list, euclid_dist_list2)
  if (last_digit_radius == TRUE) {
    # renaming of series; last character is radius, split name and radius letter/number
    euclid_dist_list <- cbind(stringr::str_sub(euclid_dist_list[,1], 0, -2), stringr::str_sub(euclid_dist_list[,1], -1),stringr::str_sub(euclid_dist_list[,2], 0, -2), stringr::str_sub(euclid_dist_list[,2], -1),euclid_dist_list[,3:7])
    colnames(euclid_dist_list) <- c("series_a", "radius_a", "series_b", "radius_b", "min_ed", "max_ed", "sum_ed", "mean_ed", "stdev_ed")
  }
  euclid_dist_list
}
