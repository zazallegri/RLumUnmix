
#' Performs CTS method for three sources
#'
#' @description
#' Performs CTS method for 3 sources. It then Selects the tracers to be used for the unmixing (fingerPro) based on score_threshold (consensus ranking method) and
#' error_threshold or nb_tracers (use either nb_tracers and score_threshold or error_threshold and score_threshold). More information is available at <https://github.com/eead-csic-eesa/fingerPro/tree/master>.
#'
#'
#' @param tracer_pair Pair extracted from [fingerPro::pairs()] output
#' @param mgeo Data frame containing one of the dataset mixtures
#' @param sgeo Data frame containing the sediment sources from a dataset
#' @param crgeo Data frame containing the CR score for each tracer (from fingerPro::cr_ns())
#' @param error_threshold Select only tracers that have an error below error_threshold
#' @param score_threshold Select only tracers that have a score above score_threshold
#' @param nb_tracers Number of tracers to be selected (ordered from smallest to largest error)
#'
#' @return Data frame containing scores and errors for the best tracers
#' @export
#'
#' @examples see vignettes
#'
Perform_CTS_3_sources <- function(tracer_pair, mgeo, sgeo, crgeo, error_threshold = NULL, score_threshold = NULL, nb_tracers = NULL) {




  # Check if the number of tracers is NULL
  if ((is.null(nb_tracers)) & (!is.null(error_threshold)) & (!is.null(score_threshold))) {

    sol <- pgeo[pgeo$id==tracer_pair,]
    ctsgeo <- fingerPro::cts_3s(source = sgeo, mixture = mgeo, sol = c(sol$w1, sol$w2, sol$w3))
    ctsgeo <- ctsgeo %>% dplyr::right_join(crgeo, by = c("tracer"))
    complete_ctsgeo <- ctsgeo
    ctsgeo <- ctsgeo[ctsgeo$err <= error_threshold & ctsgeo$score >= score_threshold,]


  } else if ((!is.null(nb_tracers)) & (is.null(error_threshold)) & (!is.null(score_threshold))) {

    sol <- pgeo[pgeo$id==tracer_pair,]
    ctsgeo <- fingerPro::cts_3s(source = sgeo, mixture = mgeo, sol = c(sol$w1, sol$w2, sol$w3))
    ctsgeo <- ctsgeo %>% dplyr::right_join(crgeo, by = c("tracer"))
    complete_ctsgeo <- ctsgeo
    ctsgeo <- ctsgeo[ctsgeo$score >= score_threshold,]
    ctsgeo <- ctsgeo[order(ctsgeo$err),]
    ctsgeo <- ctsgeo[1:nb_tracers, ]


  } else {

    print("Use either nb_tracers and score_threshold or error_threshold and score_threshold")
  }

  if (nrow(ctsgeo) <= 1) {
    print("ERROR in [Perform_CTS_3_sources()]: Needs at least 2 tracers for 3 sources")
    return(NULL)
  }

  nb_tracers <- nb_tracers#nrow(ctsgeo)
  error_threshold <- error_threshold#ctsgeo$err[nb_tracers]
  score_threshold <- score_threshold#ctsgeo$score[nb_tracers]

  return(list(ctsgeo = ctsgeo[order(ctsgeo$err),], tracer_pair = tracer_pair, error_threshold = error_threshold, score_threshold = score_threshold, nb_tracers = nb_tracers, complete_ctsgeo = complete_ctsgeo))
}
