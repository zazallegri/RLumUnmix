#' Forced additional peak detection
#'
#' @description
#' Defines whether or not force_additional_peak is TRUE or FALSE in automatic peak detection
#'
#'
#' @param last_datapoint Last point of curve
#' @param last_local_max Last locl max found
#' @param last_local_min Last local min found
#' @param peaks Hash object with peak information
#'
#' @return Updated peak information
#' @export
#'
#' @examples see vignettes
Add_last_datapoint_for_potential_peak <- function(last_datapoint, last_local_max, last_local_min, peaks) {



  peaks_temp <- peaks$peaks_temp
  peaks_intensity <- peaks$peaks_intensity
  forced_additional_peak <- peaks$forced_additional_peak

  if (length(peaks_temp) > 0) {

    if ((last_local_min$Temperature < last_datapoint$Temperature) &&
        (last_local_max$Temperature < last_local_min$Temperature) &&
        (last_datapoint$Intensity > last_local_min$Intensity)) {

      peaks_temp <- c(peaks_temp, last_datapoint$Temperature)
      peaks_intensity <- c(peaks_intensity, last_datapoint$Intensity)
      forced_additional_peak <- TRUE
      print(glue::glue("Forced additional peak located at ({last_datapoint$Temperature}, {last_datapoint$Intensity}). Total peaks : {length(peaks_temp)}"))

    }
    else {

      print("No additional peak added")
    }
  }


  return(list(peaks_temp = peaks_temp, peaks_intensity = peaks_intensity, nb_peaks = length(peaks_temp), forced_additional_peak = forced_additional_peak))
}
