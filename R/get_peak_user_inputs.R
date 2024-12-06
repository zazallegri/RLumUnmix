#' Number of peaks user input
#'
#' @description
#' Query user to define total number of peaks.
#' Forced_additional_peak = TRUE means that at the end of the curve, there seems to be the beginning of an incomplete peak.
#' Needs to be selected (and forced_additional_peak = TRUE) in order to not disrupt the fitting of the previous curves. No metrics
#' are extracted from this incomplete peak
#'
#'
#' @param record_num
#'
#' @return Number of peaks and whether there is an incomplete (forced_additional_peak = TRUE) peak at the end
#' @export
#'
#' @examples see vignettes
get_peak_user_inputs <- function(record_num) {


  {
    nb_peaks = readline(glue::glue("Enter total number of peaks for record {record_num}: "))
    nb_peaks <- as.numeric(nb_peaks)

    if (nb_peaks == 0) {
      forced_additional_peak <- FALSE
    } else {
      forced_additional_peak <- readline(glue::glue("Forced additional last (incomplete) peaks for record {record_num} (TRUE or FALSE)? "))
      forced_additional_peak <- as.logical(forced_additional_peak)
    }
  }

  return(list(nb_peaks = nb_peaks, forced_additional_peak = forced_additional_peak))
}
