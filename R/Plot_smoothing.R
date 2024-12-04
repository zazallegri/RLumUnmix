#' Plot TL curve smoothing
#'
#' @param df Curve data
#' @param smoothing_results Smoothing data
#' @param record_num Record number ("Record_x")
#'
#' @return Plot of smoothed TL curve above un-smoothed TL curve
#' @export
#'
#' @examples see vignettes
Plot_smoothing <- function(df, smoothing_results, record_num) {
  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  # Plot with y-axis log scale
  plot(df$Temperature - TEMPERATURE_CONVERSION_CONSTANT, df$Intensity,
       xlab = "Temperature (°C)",
       ylab = "Intensity",
       col = "black",
       main = glue("Record {record_num}"),
       log = "y")

  # Add the smoothed line
  lines(smoothing_results$Temperature - TEMPERATURE_CONVERSION_CONSTANT, smoothing_results$Intensity, col = "red")

  # Add a legend
  legend("topright", legend = c("Original", "Smoothed"), col = c("black", "red"), lty = 1)
}
