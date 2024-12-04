#' Clean extrema found automatically
#'
#' @description
#' Attempt to clean extremas found automatically by deleting extremax that are too close to each other in
#' order to not fit noise buy actual extremas
#'
#'
#' @param local_extrema Data frame containing local extrema
#' @param peaks Peak information
#' @param min_max_intensity_threshold Distance between extremas
#'
#' @return Updated peak information
#' @export
#'
#' @examples see vignettes
Consecutive_min_max_intensity_threshold_check <- function(local_extrema, peaks, min_max_intensity_threshold = 30) {

  local_extrema <- rbind(data.frame(Temperature = 0, Intensity = 0, source = "First_point"), local_extrema)
  local_extrema <- local_extrema[order(local_extrema$Temperature), ]
  # Consecutive min/max should have a difference of at least 30
  desired_extrema <- abs(diff(local_extrema$Intensity)) > min_max_intensity_threshold
  desired_extrema <- data.frame(Position = seq_along(desired_extrema) + 1, Logical = desired_extrema)
  desired_extrema <- local_extrema[desired_extrema[desired_extrema$Logical,]$Position, ]
  desired_extrema <- desired_extrema[desired_extrema$source == "Max",]
  local_max <- desired_extrema[, c("Temperature", "Intensity")]

  peaks$peaks_temp = local_max$Temperature
  peaks$peaks_intensity = local_max$Intensity
  peaks$nb_peaks = length(peaks$peaks_temp)
  peaks$forced_additional_peak <- FALSE

  return(peaks)

}
