#' Converting hash to Data Frame
#'
#' @description
#' Converts hash object containing TL metris to data frame
#'
#'
#' @param TL_hash Hash object containing TL metrics
#' @param record_num TL record number ("Record_x")
#'
#' @return Data frame
#' @export
#'
#' @examples see vignettes
TL_metrics_hash_to_dataframe <- function(TL_hash, record_num) {

  # Uses only a single record (i.e at the most 1 TL in sequence)

  TL_hash <- TL_hash[[record_num]]

  if (TL_hash$Nb_peaks == 0) {
    return()
  }

  df <- data.frame(row.names = record_num)

  if (TL_hash$Forced_additional_peak) {
    Nb_peaks <- TL_hash$Nb_peaks - 1
  } else {
    Nb_peaks <- TL_hash$Nb_peaks
  }
  df[["Nb_peaks"]] <- Nb_peaks

  for (key in setdiff(names(TL_hash), c("Nb_peaks", "Forced_additional_peak"))) {
    for (peak_num in 1:Nb_peaks) {
      df[[paste0(key, "_peak_", peak_num)]] <- TL_hash[[key]][[paste0("Peak_", peak_num)]]
    }

  }

  return(df)
}
