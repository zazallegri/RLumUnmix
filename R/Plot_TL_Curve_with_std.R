#' Plot normalised intensity standard deviation
#'
#' @param model_output TL curve data
#' @param temp_std_results Standard deviation data
#' @param record_num Record number ("Record_x")
#'
#' @return Plot of normalised intensity standard deviation
#' @export
#'
#' @examples see vignettes
Plot_TL_Curve_with_std  <- function(model_output, temp_std_results, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  temp_std_results$Temperature <- temp_std_results$Temperature - TEMPERATURE_CONVERSION_CONSTANT # Convert to Celsius

  # plot_RLum(object = model_output, log = "y", main = paste("Record", record_num, " - Thermoluminescence (TL) curve with", peaks$nb_peaks, "peaks"))

  if (length(temp_std_results$Temperature) != 0) {

    temperature <- temp_std_results$Temperature
    intensity_std <- temp_std_results$Intensity_std
    intensity_std_norm <- temp_std_results$Intensity_std_norm

    plot(temperature, intensity_std_norm, pch=16, xlab="Temperature (°C)",
         ylab="Normalised intensity std", type="b", col="black",
         main=paste("Record",record_num), log = "y")

  } else {
    print("No std to print")
  }

}
