#' Peaks filtering
#'
#' @description
#' Discards peaks that aren't significantly higher than neighbouring points
#'
#'
#' @param peaks Peaks information
#' @param smoothing_data Smoothed data, output of TL_curve_smoothing()
#' @param neighbour_temp_window_size Window size
#'
#' @return Updated peak information
#' @export
#'
#' @examples see vignettes
Average_neighbouring_points_filtering <- function(peaks, smoothing_data, neighbour_temp_window_size) {

  if (length(peaks) == 0) {
    print("No peaks to filter. Exiting Average_neighbouring_points_filtering().")
    return(peaks)
  }

  OG_length <- length(peaks$peaks_temp)

  data <- smoothing_data

  peaks_temp <- peaks$peaks_temp
  peaks_intensity <- peaks$peaks_intensity

  # Create empty data frame to store the results
  peaks_neighbour_intensity_avg <- data.frame(Temperature = numeric(),
                                              Neighbours_avg_intensity = numeric())

  for (i in seq_along(peaks_temp)){

    peak_temp <- peaks_temp[i]
    peak_intensity <- peaks_intensity[i]

    window_data <- data[data$Temperature <= peak_temp + neighbour_temp_window_size/2 &
                          data$Temperature >= peak_temp - neighbour_temp_window_size/2,]

    peaks_neighbour_intensity_avg <- rbind(peaks_neighbour_intensity_avg,
                                           data.frame(Temperature = peak_temp,
                                                      Intensity = peak_intensity,
                                                      Intensity_avg = mean(window_data$Intensity)))
  }

  # Only keep lines of peaks_neighbour_intensity_avg that have Intensity_avg < Intensity
  peaks_neighbour_intensity_avg <- peaks_neighbour_intensity_avg[peaks_neighbour_intensity_avg$Intensity_avg < peaks_neighbour_intensity_avg$Intensity,]
  peaks$peaks_temp <- peaks_neighbour_intensity_avg$Temperature
  peaks$peaks_intensity <- peaks_neighbour_intensity_avg$Intensity

  print(glue::glue("Average_neighbouring_points_filtering() deleted {OG_length - length(peaks$peaks_temp)} peaks."))

  return(peaks)


}
