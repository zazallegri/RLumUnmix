#' TL curve plotting
#'
#' @param model_output TL curve data
#' @param peaks Local peaks
#' @param record_num Record number ("Record_x")
#'
#' @return Plut of the TL curve
#' @export
#'
#' @examples see vignettes
Plot_TL_Curve <- function(model_output, peaks, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  Luminescence::plot_RLum(object = model_output,
            main = paste("Record", record_num, " - TL curve with", peaks$nb_peaks,
                         "peaks - FORCED_PEAK = ", peaks$forced_additional_peak), log = "y")

  if (peaks$nb_peaks != 0) {
    graphics::points(peaks$peaks_temp - TEMPERATURE_CONVERSION_CONSTANT, peaks$peaks_intensity, col = "red", pch = 19)
    # legend("topright", legend = "Estimated peak locations", col = "red", pch = 19)#"bottomright",
  }

}
