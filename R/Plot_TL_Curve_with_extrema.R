#' Plot TL curve with extrema
#'
#' @param model_output TL curve data
#' @param local_extrema All local extrema
#' @param record_num Record number ("Record_x")
#'
#' @return Plot of TL curve with extrema
#' @export
#'
#' @examples see vignettes
Plot_TL_Curve_with_extrema <- function(model_output, local_extrema, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  peaks_temp <- local_extrema[local_extrema$source == "Max", "Temperature"]
  peaks_intensity <- local_extrema[local_extrema$source == "Max", "Intensity"]
  valleys_temp <- local_extrema[local_extrema$source == "Min", "Temperature"]
  valleys_intensity <- local_extrema[local_extrema$source == "Min", "Intensity"]

  nb_peaks <- length(peaks_temp)
  nb_valleys <- length(valleys_temp)

  plot_RLum(object = model_output, log = "y", main = paste("Record ", record_num, "- Thermoluminescence (TL) curve with", nb_peaks, "peaks and", nb_valleys, "valleys"))
  if (nb_peaks != 0 | nb_valleys != 0) {
    points(peaks_temp - TEMPERATURE_CONVERSION_CONSTANT, peaks_intensity, col = "red", pch = 19)
    points(valleys_temp - TEMPERATURE_CONVERSION_CONSTANT, valleys_intensity, col = "blue", pch = 19)
    legend("bottomright", legend = c("Estimated peak locations", "Estimated valley locations"), col = c("red", "blue"), pch = 19)
  }

}
