#' Compute intensity standard deviation
#'
#' @description
#' Retrieve standard deviation of TL intensity with window around local max
#'
#'
#' @param local_extrema All local extrema
#' @param df Curve data
#' @param std_computation_window_size_degrees Window size for intensity standard deviation computation (in °C)
#'
#' @return Intensity standard deviation window for every extrema
#' @export
#'
#' @examples see vignettes
Compute_intensity_std_moving_window <- function(local_extrema, df, std_computation_window_size_degrees) {

  local_extrema <- local_extrema[local_extrema$source == "Max",]
  local_extrema <- local_extrema[,c("Temperature", "Intensity")]
  df$Intensity_norm <- (df$Intensity - min(df$Intensity)) / (max(df$Intensity) - min(df$Intensity))

  # Create empty df to store the results
  std_results <- data.frame(Temperature = numeric(), Intensity_std = numeric(), Intensity_std_norm = numeric())

  for (extrema_temp in local_extrema$Temperature){

    window_data <- df[df$Temperature <= extrema_temp + std_computation_window_size_degrees/2 &
                        df$Temperature >= extrema_temp - std_computation_window_size_degrees/2,]

    std_results <- rbind(std_results, data.frame(Temperature = extrema_temp, Intensity_std = std(window_data$Intensity),
                                                 Intensity_std_norm = std(window_data$Intensity_norm)))
  }

  return(std_results)

}
