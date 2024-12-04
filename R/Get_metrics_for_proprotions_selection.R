#' Extract metrics from estimated proportions
#'
#' @description
#' This function updated data frame "metrics" with information from unmixing routine
#' such as the mean/median Manhattan distance (if expected population fraction provided),
#' number of tracers/score/error thresholds used to select tracers for unmixing, 95% CI, width of CI
#'
#' @param metrics Data frame contining unmixing metrics
#' @param filename Name of file
#' @param tracer_pair Pair extracted from [fingerPro::pairs()] output
#' @param ctsgeo Data frame containing scores and errors for the best tracers
#' @param source_list List of sources c("S1", "S2",...)
#' @param ratio_differences Mean/median differences between estimated and expected population fractions
#' @param proportions Information of every source contained in a df: filename	variable	mean	median	sd	n	Q25	Q75	lower.ci	upper.ci	error_threshold	score_threshold	nb_tracer	Expected_population_fraction
#' @param error_threshold Select only tracers that have an error below error_threshold
#' @param score_threshold Select only tracers that have a score above score_threshold
#' @param nb_tracers Number of tracers to be selected (ordered from smallest to largest error)
#'
#' @return Updated metrics data frame
#' @export
#'
#' @examples see vignettes
Get_metrics_for_proprotions_selection <- function(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer) {



  ## Add tracer_pair, tracers, Manhattan_dist_median, Manhattan_dist_mean, filename, CI width to metrics
  # Rename col "id" to "filename"
  colnames(metrics)[colnames(metrics) == "id"] <- "filename"
  metrics$filename <- filename

  if (!is.null(tracer_pair)) {
    metrics <- cbind(metrics, tracer_pair = tracer_pair)
  } else {
    metrics <- cbind(metrics, tracer_pair = NaN)
  }
  # metrics <- cbind(metrics, tracer_pair = tracer_pair)

  metrics <- cbind(metrics, tracers = paste0(ctsgeo$tracer, collapse = "-"))


  if (is.null(error_threshold)) {
    error_threshold <- NaN
  }
  if (is.null(score_threshold)) {
    score_threshold <- NaN
  }
  if (is.null(nb_tracer)) {
    nb_tracer <- NaN
  }


  metrics <- cbind(metrics, error_threshold = error_threshold)
  metrics <- cbind(metrics, score_threshold = score_threshold)
  metrics <- cbind(metrics, nb_tracer = nb_tracer)


  ## Add width of confidence interval
  for (source in source_list) {
    metrics[, paste0(source,"_CI_width")] <- tail(proportions[proportions$variable == source, "upper.ci"],1)[[1]] - tail(proportions[proportions$variable == source, "lower.ci"],1)[[1]]
  }
  # Add sum of witdth of confidence interval
  metrics <- cbind(metrics, CI_width_sum = rowSums(metrics[, grep("_CI_width", names(metrics))]))


  if (nrow(ratio_differences) > 0){

    ## Compute Manhattan distance
    # Get columns that start with "S" and contains _median_
    cols <- grep("^S.*_median_", names(ratio_differences), value = TRUE)
    # Get sum of absolute values in ratio_differences[, cols]
    Manhattan_dist_median <- rowSums(abs(ratio_differences[, cols]))

    # Get columns that start with "S" and contains _mean_
    cols <- grep("^S.*_mean_", names(ratio_differences), value = TRUE)
    # Get sum of absolute values in ratio_differences[, cols]
    Manhattan_dist_mean <- rowSums(abs(ratio_differences[, cols]))
    metrics <- cbind(metrics, Manhattan_dist_median, Manhattan_dist_mean)
  }

  return(metrics)
}
