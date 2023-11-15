

#' Segmented SGC of all series
#' 
#' This function calculates the SGC over segmentes of a certain length. Each segment overlaps half with the previous segment. If for example the segment length is 50, then the first segment starts at the first year in the data frame with tree ring series and ends at 50. The next segment starts at 25 and ends at 75, etcetera.
#'
#' @param trw 
#' @param segment_length 
#'
#' @return segment_glk a datafram with segments and glk per segment
#' @export segmented_glk
#'
#' @examples
#' 
#' seg_glk <- segmented_glk(tree_ring_series)
#' 
segmented_glk <- function(trw, segment_length = 50) {
  n_years <- nrow(trw)
  n_series <- ncol(trw)
  n_segment <- floor(n_years/(segment_length/2))
  glk_segments <- data.frame(matrix(NA, nrow = n_segment, ncol = n_series))
  colnames(glk_segments) <- colnames(trw)
  for (i in 1:n_segment) {
    temp_trw <- trw[((i-1)*(segment_length/2)):(i*(segment_length/2)+segment_length/2),]
    # segment period as row name
    rownames(glk_segments)[i] <- paste0(as.numeric(rownames(trw[1,]))+(i-1)*(segment_length/2), "-", as.numeric(rownames(trw[1,]))+i*(segment_length/2)+segment_length/2)
    overlap_50 <- colSums(!is.na(temp_trw))>=50
    temp_trw <- temp_trw[overlap_50] # only series with 50 years in segment
    if (ncol(temp_trw)==2) {
      glk_list <- glk(temp_trw, overlap = segment_length-1, prob = FALSE) # always 1 growth change les then the length over the common tree rings
      glk_value <- glk_list$glk_mat[upper.tri(glk_list$glk_mat)]
      glk_segments[i, overlap_50] <- glk_value
    } else if (ncol(temp_trw)>2) {
      # pre-allocate vectors for SGC and SSGC
      glk_temp <- numeric(ncol(temp_trw))
      for (j in 1:ncol(temp_trw)) {
        # create numberic mean of other series to compare this series with
        compare_trw_mean <- cbind(rowMeans(temp_trw[,-j]),temp_trw[,j])
        glk_list <- glk(temp_trw, overlap = segment_length-1, prob = FALSE) # always 1 growth change les then the length over the common tree rings
        glk_value <- glk_list$glk_mat[upper.tri(glk_list$glk_mat)]
        # place in temporary vector
        glk_temp[j] <- glk_value
      }
      glk_segments[i, overlap_50] <- glk_temp
    }
  }
  glk_segments
}