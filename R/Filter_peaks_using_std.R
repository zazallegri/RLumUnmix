#' Filter peaks using standard deviation
#'
#' @description
#' Filters peaks based on intensity std of window around peak.
#' Keeps only peaks that have std of intensity around them greater than std_threshold
#'
#'
#'
#' @param local_extrema All local extrema
#' @param peaks Peak information
#' @param df Curve data
#' @param std_computation_window_size_degrees Window size in °C
#' @param std_threshold Standard deviation threshold
#'
#' @return Updated peak information
#' @export
#'
#' @examples see vignettes
Filter_peaks_using_std <- function(local_extrema, peaks, df, std_computation_window_size_degrees, std_threshold) {

  if (length(peaks) == 0) {
    moving_window_local_extrema <- local_extrema
  }

  else {
    moving_window_local_extrema <- data.frame(Temperature = peaks$peaks_temp,
                                              Intensity = peaks$peaks_intensity,
                                              source = rep("Max", length(peaks$peaks_temp)))
  }

  # Std moving window for each local peak
  std_results <- Compute_intensity_std_moving_window(moving_window_local_extrema, df, std_computation_window_size_degrees)

  if (length(peaks) == 0) {

    peaks$peaks_temp <- std_results$Temperature
    peaks$peaks_intensity <- local_extrema[local_extrema$Temperature %in% std_results$Temperature, "Intensity"]
    peaks$nb_peaks <- length(peaks$peaks_temp)
    peaks$forced_additional_peak <- FALSE

  }

  temp_of_std_above_threshold <- std_results[std_results$Intensity_std_norm >= std_threshold, "Temperature"]
  position_num_to_keep <- which(std_results$Temperature %in% temp_of_std_above_threshold)
  peaks$peaks_temp <- peaks$peaks_temp[position_num_to_keep]
  peaks$peaks_intensity <- peaks$peaks_intensity[position_num_to_keep]

  return(list(peaks = peaks, std_results = std_results))

}
