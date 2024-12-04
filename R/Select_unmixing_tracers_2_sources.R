#' Selection of tracers for unmixing of 2 sources
#'
#' @param mgeo Data frame containing one of the dataset mixtures
#' @param sgeo Data frame containing the sediment sources from a dataset
#' @param crgeo Data frame containing the CR score for each tracer (from fingerPro::cr_ns())
#' @param score_threshold Select only tracers that have a score above score_threshold
#' @param nb_tracers Number of tracers to be selected (ordered from smallest to largest error)
#'
#'
#' @return Data frame contained scored of best tracers selected
#' @export
#'
#' @examples see vignettes
Select_unmixing_tracers_2_sources <- function(mgeo, sgeo, crgeo, score_threshold = NULL, nb_tracers = NULL) {


  ## Order by score and filter by score_threshold
  ordered_crgeo <- crgeo[order(-crgeo$score),]
  ordered_crgeo <- ordered_crgeo[ordered_crgeo$score > score_threshold, ]



  ## Select tracers based on score and nb_tracers
  if ((!is.null(nb_tracers)) &  (!is.null(score_threshold))) {

    ordered_crgeo <- ordered_crgeo[1:nb_tracers, ]
    print(ordered_crgeo)


  } else if ((is.null(nb_tracers)) & (is.null(score_threshold))) {

    print("Provide at least 'score_threshold'")

    ## Select tracers solely based on score
  } else if ((is.null(nb_tracers)) & (!is.null(score_threshold))) {

    ordered_crgeo <- ordered_crgeo

  } else {

    print("ERROR in [Select_unmixing_tracers_2_sources()]")
    stop()
  }

  if (nrow(ordered_crgeo) <= 1) {
    print("ERROR in [Select_unmixing_tracers_2_sources()]: Need at least 2 tracers for 2 sources")
    stop()
  }

  # nb_tracers <- nrow(ordered_crgeo)
  # score_threshold <- ordered_crgeo$score[nb_tracers]

  return(list(ctsgeo = ordered_crgeo, tracer_pair = NULL, error_threshold = NULL, score_threshold = score_threshold, nb_tracers = nb_tracers))

}
