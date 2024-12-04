#' Find local extrema
#'
#' @description
#' Find local extrema based on <https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima>
#'
#'
#' @param x Data points
#' @param threshold Window size
#'
#' @return Local extrema
#' @export
#'
#' @examples see vignettes
inflect <- function(x, threshold = 1) {

  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA, abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x, up, down)
  list(minima = which(apply(a, 1, min) == a[, 1]), maxima = which(apply(a, 1, max) == a[, 1]))
}
