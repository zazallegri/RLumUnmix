#' Convert hash object containing OSL metrics to data frame
#'
#' @param OSL_hash Hash object containing OSL metrics
#' @param record_num Record number ("Record_x")
#'
#' @return Data frame containing OSL metrics
#' @export
#'
#' @examples see vignettes
OSL_metrics_hash_to_dataframe <- function(OSL_hash, record_num) {

  # Only for a single record
  OSL_hash <- OSL_hash[[record_num]]

  df <- data.frame(row.names = record_num)
  Nb_components <- OSL_hash$Nb_components

  df[['Nb_components']] <- Nb_components

  for (key in setdiff(names(OSL_hash), c("Nb_components"))) {
    for (component_num in 1:Nb_components) {
      df[[paste0(key, "_component_", component_num)]] <- OSL_hash[[key]][[paste0("Component_",component_num)]]
    }

  }

  return(df)
}
