

#' Segmented SGC of all series
#' 
#' This function calculates the SGC over segmentes of a certain length. Each segment overlaps half with the previous segment. If for example the segment length is 50, then the first segment starts at the first year in the data frame with tree ring series and ends at 50. The next segment starts at 25 and ends at 75, etcetera.
#'
#' @param trw 
#' @param segment_length 
#'
#' @return segmented_sgc_ssgc as list with two dataframes: sgc_segments and sgc_segments
#' @export segmented_sgc
#'
#' @examples
#' 
#' seg_sgc <- segmented_sgc(tree_ring_series)
#' seg_sgc$sgc_segments # this will get you the segmented SGC
#' seg_sgc$ssgc_segments # this will get you the segmented SSGC
#' 
segmented_sgc <- function(trw, segment_length = 50) {
  n_years <- nrow(trw)
  n_series <- ncol(trw)
  n_segment <- floor(n_years/(segment_length/2))
  sgc_segments <- data.frame(matrix(NA, nrow = n_segment, ncol = n_series))
  colnames(sgc_segments) <- colnames(trw)
  ssgc_segments <- sgc_segments
  for (i in 1:n_segment) {
    temp_trw <- trw[((i-1)*(segment_length/2)):(i*(segment_length/2)+segment_length/2),]
    # segment period as row name
    rownames(sgc_segments)[i] <- paste0(as.numeric(rownames(trw[1,]))+(i-1)*(segment_length/2), "-", as.numeric(rownames(trw[1,]))+i*(segment_length/2)+segment_length/2)
    overlap_50 <- colSums(!is.na(temp_trw))>=50
    temp_trw <- temp_trw[overlap_50] # only series with 50 years in segment
    if (ncol(temp_trw)==2) {
      sgc_list <- sgc(temp_trw, overlap = segment_length-1) # always 1 growth change les then the length over the common tree rings
      sgc_value <- sgc_list$sgc_mat[upper.tri(sgc_list$sgc_mat)]
      ssgc_value <- sgc_list$ssgc_mat[upper.tri(sgc_list$sgc_mat)]
      sgc_segments[i, overlap_50] <- sgc_value
      ssgc_segments[i, overlap_50] <- ssgc_value
    } else if (ncol(temp_trw)>2) {
      # pre-allocate vectors for SGC and SSGC
      sgc_temp <- numeric(ncol(temp_trw))
      ssgc_temp <- numeric(ncol(temp_trw))
      for (j in 1:ncol(temp_trw)) {
        # create numberic mean of other series to compare this series with
        compare_trw_mean <- cbind(rowMeans(temp_trw[,-j]),temp_trw[,j])
        sgc_list <- sgc(compare_trw_mean, overlap = segment_length-1)
        sgc_value <- sgc_list$sgc_mat[upper.tri(sgc_list$sgc_mat)]
        ssgc_value <- sgc_list$ssgc_mat[upper.tri(sgc_list$sgc_mat)]
        # place in temporary vector
        sgc_temp[j] <- sgc_value
        ssgc_temp[j] <- ssgc_value
      }
      sgc_segments[i, overlap_50] <- sgc_temp
      ssgc_segments[i, overlap_50] <- ssgc_temp
    }
    rownames(ssgc_segments) <-  rownames(sgc_segments)
  }
  segmented_sgc_ssgc <- list(sgc_segments = sgc_segments,
                             ssgc_segments = ssgc_segments)
  segmented_sgc_ssgc
}