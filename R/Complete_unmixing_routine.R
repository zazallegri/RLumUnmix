#' Complete unmixing routine
#'
#' @description
#' Performs the entire unmixing routine for 2 ou 3 sources. Additional sources should be easy to add
#'
#'
#' @param mgeo Data frame containing one of the dataset mixtures
#' @param sgeo Data frame containing the sediment sources from a dataset
#' @param crgeo Data frame containing the CR score for each tracer (from fingerPro::cr_ns())
#' @param source_list List of sources c("S1", "S2",...)
#' @param Population_fraction Expected population fraction
#' @param filename Name of file
#' @param error_threshold Select only tracers that have an error below error_threshold
#' @param score_threshold Select only tracers that have a score above score_threshold
#' @param nb_tracers Number of tracers to be selected (ordered from smallest to largest error)
#' @param tracer_pair Pair extracted from [fingerPro::pairs()] output
#'
#' @return Results of unmixing and metrics to be able to evaluate quality of unmixing
#' @export
#'
#' @examples see vignettes
Complete_unmixing_routine <- function(mgeo, sgeo, crgeo, source_list, Population_fraction, filename, error_threshold = NULL, score_threshold = NULL, nb_tracer = NULL, tracer_pair = NULL) {

  if (is.null(tracer_pair)) {

    output <- Select_unmixing_tracers_2_sources(sgeo = sgeo,
                                                mgeo = mgeo,
                                                crgeo = crgeo,
                                                score_threshold = score_threshold,
                                                nb_tracers = nb_tracer)
    ctsgeo <- output$ctsgeo
    tracer_pair <- output$tracer_pair
    error_threshold <- output$error_threshold
    score_threshold <- output$score_threshold
    nb_tracer <- output$nb_tracer

  } else if (!is.null(tracer_pair)) {


    output <- Perform_CTS_3_sources(tracer_pair = tracer_pair,
                                    mgeo = mgeo,
                                    sgeo = sgeo,
                                    crgeo = crgeo,
                                    error_threshold = error_threshold,
                                    score_threshold = score_threshold,
                                    nb_tracer = nb_tracer)

    ctsgeo <- output$ctsgeo
    tracer_pair <- output$tracer_pair
    error_threshold <- output$error_threshold
    score_threshold <- output$score_threshold
    nb_tracer <- output$nb_tracer
  } else { # function [Perform_CTS_4_sources()] should be easy to create and add based on [Perform_CTS_3_sources()]
    print("ERROR in [Complete_unmixing_routine()]")
    stop()
  }




  if (nrow(ctsgeo) == 0) {

    print("No tracers in function [Complete_unmixing_routine()]")

    return(list(metrics = NULL, proportions = NULL, ratio_differences = NULL))

  } else if ((nrow(ctsgeo) >= 1) & (is.null(tracer_pair))) {

    data_sol <- Select_data_for_unmixing(sgeo = sgeo,
                                         mgeo = mgeo,
                                         unmixing_tracers = ctsgeo$tracer)

    output <- Plot_unmixing_proportions_and_get_results(data_sol,
                                                        source_list,
                                                        Population_fraction,
                                                        filename,
                                                        error_threshold,
                                                        score_threshold,
                                                        nb_tracer,
                                                        tracer_pair)
    proportions <- output$proportions
    ratio_differences <- output$ratio_differences
    unmixing_results_long <- output$unmixing_results_long
    metrics <- do.call(data.frame, stats::aggregate(. ~ id, data = output$unmixing_results, function(x) c(mean = mean(x), SD = stats::sd(x))))
    metrics <- Get_metrics_for_proprotions_selection(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer)

  } else if ((nrow(ctsgeo) > 1) & (!is.null(tracer_pair))) {


    data_sol <- Select_data_for_unmixing(sgeo = sgeo,
                                         mgeo = mgeo,
                                         unmixing_tracers = ctsgeo$tracer)

    output <- Plot_unmixing_proportions_and_get_results(data_sol,
                                                        source_list,
                                                        Population_fraction,
                                                        filename,
                                                        error_threshold,
                                                        score_threshold,
                                                        nb_tracer,
                                                        tracer_pair)
    proportions <- output$proportions
    ratio_differences <- output$ratio_differences
    unmixing_results_long <- output$unmixing_results_long
    metrics <- do.call(data.frame, stats::aggregate(. ~ id, data = output$unmixing_results, function(x) c(mean = mean(x), SD = sd(x))))
    metrics <- Get_metrics_for_proprotions_selection(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer)


  } else {

    print(ctsgeo)
    print("ERROR in [Complete_unmixing_routine()]")
    stop()
  }


  return(list(metrics = metrics, proportions = proportions, ratio_differences = ratio_differences, ctsgeo = ctsgeo, unmixing_results_long=unmixing_results_long))
}
