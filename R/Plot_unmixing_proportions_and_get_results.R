#' Unmixing and plot results
#'
#' @param data_sol Data frame containing tracers used for unmixing. Output of function [Select_data_for_unmixing()]
#' @param source_list List of sources c("S1", "S2",...)
#' @param Population_fraction Expected population fraction
#' @param filename Name of file
#' @param error_threshold Select only tracers that have an error below error_threshold
#' @param score_threshold Select only tracers that have a score above score_threshold
#' @param nb_tracer Number of tracers to be selected (ordered from smallest to largest error)
#' @param tracer_pair Pair extracted from [fingerPro::pairs()] output
#'
#' @return Results of unmixing process + plots of estimated proportions
#' @export
#'
#' @examples see vignettes
Plot_unmixing_proportions_and_get_results <- function(data_sol, source_list, Population_fraction, filename, error_threshold, score_threshold, nb_tracer, tracer_pair = NULL) {

  proportions <- data.frame()
  ratio_differences <- data.frame()

  # Let's unmix the multiple solutions
  result_FP <- fingerPro::unmix(data_sol, Means = T)#samples = 200, iter = 200,

  P_FP <- fingerPro::plotResults(result_FP, y_high = 1)#, colors = c("#CC0000",  "#33CCFF", "#9933CC"))


  # Melt the dataframe to long format for ggplot2
  result_FP_long <- reshape2::melt(result_FP, measure.vars = source_list)# c("S1", "S2")


  # ## Group result_FP_long by "variable" and compute the mean and median of "value"
  # proportions <- result_FP_long %>%
  #                 group_by(variable) %>%
  #                 summarise(mean = mean(value),
  #                           median = median(value),
  #                           sd = sd(value),
  #                           n = n()) %>%
  #                 mutate(se = sd / sqrt(n),
  #                         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
  #                         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

  ## CI for proportion https://www.statology.org/confidence-interval-proportion/
  z_value<- 1.96# for 95% confidence interval/ 1.645 for 90% confidence interval / 2.58 for 99% confidence interval
  proportions_current <- result_FP_long %>%
    group_by(variable) %>%
    summarise(mean = mean(value),
              median = stats::median(value),
              sd = stats::sd(value),
              n = n(),
              Q25 = unname(stats::quantile(value, .25)),
              Q75 = unname(stats::quantile(value, .75))) %>%
    mutate(lower.ci = mean - z_value * sqrt(mean * (1 - mean) / n),
           upper.ci = mean + z_value * sqrt(mean * (1 - mean) / n))

  # add filename as first column of proportions
  if (!is.null(tracer_pair)) {
    proportions_current <- cbind(tracer_pair = tracer_pair, proportions_current)
  }
  proportions_current <- cbind(filename = filename, proportions_current)

  if (is.null(error_threshold)) {
    proportions_current <- cbind(proportions_current,
                                 error_threshold = rep(NaN, length(source_list)),
                                 score_threshold = rep(score_threshold, length(source_list)),
                                 nb_tracer = rep(nb_tracer, length(source_list)))

  } else if (is.null(nb_tracer)) {
    proportions_current <- cbind(proportions_current,
                                 error_threshold = rep(error_threshold, length(source_list)),
                                 score_threshold = rep(score_threshold, length(source_list)),
                                 nb_tracer = rep(NaN, length(source_list)))

  } else {
    print("ERROR in [Plot_unmixing_proportions_and_get_results()]")
    stop()
  }

  # add new empty column

  # proportions_current$Expected_population_fraction <- rep(NaN, length(source_list))
  proportions_current[-length(source_list):-1, "Expected_population_fraction"] <- rep(NaN, length(source_list))
  # proportions <- rbind(proportions, proportions_current)

  #-----------------------------
  ## Comparison of proportions between computed and expected
  output <- Compute_mean_median_proportions_differences(source_list, Population_fraction, proportions_current, filename, tracer_pair)
  proportions <- output$proportions
  ratio_differences <- rbind(ratio_differences, data.frame(output$temp_list_differences))

  #-----------------------------
  Plot_sources_proportions(result_FP_long, filename, data_sol, Population_fraction, source_list, tracer_pair)

  return(list(unmixing_results = result_FP, unmixing_results_long = result_FP_long ,proportions = proportions, ratio_differences = ratio_differences))

}
