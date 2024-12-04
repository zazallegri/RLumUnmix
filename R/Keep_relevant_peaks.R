#' Filter peaks out of all the automatically selected peaks
#'
#' @param local_extrema All local extrema
#' @param df Curve data
#' @param smoothing_data Smoothed curve data
#' @param min_max_intensity_threshold Threshold for [Consecutive_min_max_intensity_threshold_check()]
#' @param std_computation_window_size_degrees Window size for [Average_neighbouring_points_filtering()]
#' @param std_threshold Threshold for [Filter_peaks_using_std()]
#'
#' @return Updated peaks
#' @export
#'
#' @examples see vignettes
Keep_relevant_peaks <- function(local_extrema, df, smoothing_data,
                                min_max_intensity_threshold = 20,
                                std_computation_window_size_degrees = 75,
                                std_threshold = 0) {

  peaks <- hash()
  results <- hash()

  # Consecutive min/max should have a difference of at least 20
  results$peaks <- Consecutive_min_max_intensity_threshold_check(local_extrema,
                                                                 peaks,
                                                                 min_max_intensity_threshold = min_max_intensity_threshold)


  results$peaks <- Average_neighbouring_points_filtering(results$peaks, smoothing_data,
                                                         neighbour_temp_window_size = 20)

  # results <- Filter_peaks_using_std(local_extrema, results$peaks, df, std_computation_window_size_degrees, std_threshold)


  return(results)

}
