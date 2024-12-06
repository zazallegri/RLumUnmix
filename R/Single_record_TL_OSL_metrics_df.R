#' Aggregates TL and OSL metrics for single record
#'
#' @param TL_hash Hash object containing TL metrics
#' @param OSL_hash Hash object containing OSL metrics
#' @param record_num Record number ("Record_x")
#'
#' @return Single line data frame containing all OSL and TL metrics
#' @export
#'
#' @examples see vignettes
Single_record_TL_OSL_metrics_df <- function(TL_hash, OSL_hash, record_num) {


  if ((record_num %in% hash::keys(TL_hash)) &
      (record_num %in% hash::keys(OSL_hash))) {

    single_record_TL_metrics <- TL_metrics_hash_to_dataframe(TL_hash, record_num)
    single_record_OSL_metrics <- OSL_metrics_hash_to_dataframe(OSL_hash, record_num)

    if (!is.null(single_record_TL_metrics)) {
      output <- cbind(single_record_TL_metrics, single_record_OSL_metrics)

    } else {
      output <- single_record_OSL_metrics
    }

  } else if ((record_num %in% hash::keys(TL_hash)) &
             !(record_num %in% hash::keys(OSL_hash))) {

    single_record_TL_metrics <- TL_metrics_hash_to_dataframe(TL_hash, record_num)

    if (!is.null(single_record_TL_metrics)) {
      output <- single_record_TL_metrics

    } else {
      return()
    }

    output <- single_record_TL_metrics


  } else if (!(record_num %in% hash::keys(TL_hash)) &
             (record_num %in% hash::keys(OSL_hash))) {

    single_record_OSL_metrics <- OSL_metrics_hash_to_dataframe(OSL_hash, record_num)
    output <- single_record_OSL_metrics

  } else {
    print("ERROR in [Single_record_TL_OSL_metrics_df()] - extra conditions should be added")
  }



  # Add "Record_x" to every column name
  colnames(output) <- paste0(record_num, "_", colnames(output))

  return(output)
}
