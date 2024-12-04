#' Select data used for unmixing
#'
#' @param sgeo Data frame containing the sediment sources from a dataset
#' @param mgeo Data frame containing one of the dataset mixtures
#' @param unmixing_tracers Tracers selected for unmixing
#'
#' @return Data frame containing all the information needed for the unmixing
#' @export
#'
#' @examples see vignettes
Select_data_for_unmixing <- function(sgeo, mgeo, unmixing_tracers) {


  data_extended <- bind_rows(sgeo, mgeo)
  # Add id as first column
  data_extended <- cbind(id = seq_len(nrow(data_extended)), data_extended)
  # Change colname
  colnames(data_extended)[2] <- "sources"

  cols_for_unmixing <- append(unmixing_tracers, paste("D", unmixing_tracers, sep = ""))
  cols_for_unmixing <- append(cols_for_unmixing, "n")
  cols_for_unmixing <- append(c("id", "sources"), cols_for_unmixing)
  data_sol <- data_extended[, cols_for_unmixing]

  return(data_sol)
}
