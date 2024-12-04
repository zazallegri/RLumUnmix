#' Peak detection
#'
#' @description
#' Automatic peak detection
#' Span is the number of points around the peak to consider
#' https://www.rdocumentation.org/packages/ggpmisc/versions/0.6.0/topics/find_peaks
#'
#'
#' @param df Data containing TL intensity and temperature
#' @param span Number of points around the peak to consider
#'
#' @return Peaks number and location
#' @export
#'
#' @examples see vignettes
Get_peaks <- function(df, span) {



  peak_loc_logical <- find_peaks(df[, "Intensity"], span = span)
  peaks_temp <- df[peak_loc_logical, "Temperature"]
  peaks_intensity <- df[peak_loc_logical, "Intensity"]
  nb_peaks <- length(peaks_temp)
  print(glue("Peaks temp :{peaks_temp} "))

  return(list(peaks_temp = peaks_temp, peaks_intensity = peaks_intensity, nb_peaks = nb_peaks))

}
