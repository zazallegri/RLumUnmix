#' Smoothing TL curve
#'
#' @description
#' Smoothens TL curve using [pracma::whittaker()].
#'
#'
#' @param df Curve data
#' @param record_num Record number ("Record_x")
#' @param lambda Smoothing parameter (rough: 50 / smooth: 1e4)
#' @param make_smooth_plot
#'
#' @return Smoothed TL curve
#' @export
#'
#' @examples see vignette
TL_curve_smoothing <- function(df, record_num, lambda, make_smooth_plot = FALSE) {

  smoothing_results <- df

  if (nrow(smoothing_results) == 0) {
    print("No data points to smooth. Exiting.")
    return(df)
  }

  smoothing_results$Intensity <- pracma::whittaker(y = smoothing_results$Intensity, lambda = lambda, d = 2)

  data <- data.frame(x = smoothing_results$Temperature, y = smoothing_results$Intensity)
  colnames(data) <- c("Temperature", "Intensity")

  if (make_smooth_plot) {
    Plot_smoothing(df, smoothing_results = data, record_num = record_num)
  }

  return(data)
}
