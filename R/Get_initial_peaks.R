#' Automatic peak detection (local max)
#'
#' @description
#' Automatic peak detection (local max)
#'
#'
#' @param df Data frame containing TL intensity and temperature
#' @param span Span for function inflect()
#'
#' @return Peak information
#' @export
#'
#' @examples see vignettes
Get_initial_peaks <- function(df, span) {


  local_min_max <- inflect(df[, "Intensity"], threshold = span)
  peaks <- hash()
  peaks$peaks_temp <- df[local_min_max$maxima, "Temperature"]
  peaks$peaks_intensity <- df[local_min_max$maxima, "Intensity"]
  peaks$nb_peaks <- length(peaks$peaks_temp)
  peaks$forced_additional_peak <- FALSE


  output <- Add_last_datapoint_for_potential_peak(last_datapoint = tail(df,v1),
                                                  last_local_max = tail(df[local_min_max$maxima,], 1),
                                                  last_local_min = tail(df[local_min_max$minima,], 1),
                                                  peaks)

  return(list(peaks_temp = output$peaks_temp, peaks_intensity = output$peaks_intensity, nb_peaks = output$nb_peaks, forced_additional_peak = output$forced_additional_peak))

}
