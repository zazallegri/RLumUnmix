#' Extract TL/OSL metrics
#'
#' @param TL_hash Hash object containing TL metrics
#' @param OSL_hash Hash object containing OSL metrics
#' @param source_number Source number ("S1" or "S2" or ...)
#' @param records_to_extract_data_from Records from which to extract data (c("Record_1", "Record_2")).
#'
#' @return OSL/TL metrics for all records
#' @export
#'
#' @examples see vignettes
Extract_OSL_TL_metrics_for_all_records <- function(TL_hash, OSL_hash, source_number, records_to_extract_data_from = NULL) {

  if (is.null(records_to_extract_data_from)) {
    records_to_extract_data_from <- hash::keys(TL_hash)
  }

  metrics <- data.frame(matrix(NA, nrow = 1, ncol = 0))
  for (record_num in records_to_extract_data_from) {
    metrics_temp <- Single_record_TL_OSL_metrics_df(TL_hash = TL_hash, OSL_hash = OSL_hash, record_num = record_num)

    if (!is.null(metrics_temp)){
      metrics <- cbind(metrics, metrics_temp)
    }
  }


  metrics <- cbind("sources" = rep(source_number, nrow(metrics)), metrics)

  return(metrics)
}
