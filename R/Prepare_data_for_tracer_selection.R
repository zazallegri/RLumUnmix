
#' Data preparation for unmixing
#'
#' @description
#' The df stored in data_path should have the same structure as the df named 'unmixing_df' (in the data folder)
#' First column should be named "sources": each source in that column should be named  "S1", "S2", "S3", ... and "Mixed"
#' The following columns should be the potential tracers
#' The last column should be named "Pop_fraction" and should contain the population fraction of each source. Not mandatory
#'
#'
#' @param data_path Path to the dataframe containing data of sources and mixes
#'
#' @return Data needed for the unmixing routine as well as the original "stacked_df" loaded.
#' @export
#'
#' @examples see vignettes
Prepare_data_for_tracer_selection <- function(data_path) {


  # Read stacked_df
  stacked_df <- read.csv(file = data_path)

  # Delete columns that contains only zeros
  stacked_df <- stacked_df[, colSums(stacked_df != 0, na.rm = TRUE) > 0]

  stacked_df_OG <- stacked_df
  # Get all unique values in the column "sources" that starts with S
  source_list <- unique(stacked_df[grepl("^S", stacked_df$sources), "sources"])

  if (colnames(stacked_df)[ncol(stacked_df)] == "Pop_fraction") {

    # Get Population_fraction
    Population_fraction <- stacked_df[, c("sources", "Pop_fraction")]

    # Delete last column of stacked_df
    stacked_df <- stacked_df[, -ncol(stacked_df)]

    # groupby "sources" and keep mean value
    Population_fraction <- Population_fraction %>% group_by(sources) %>% summarise(Pop_fraction = mean(Pop_fraction))

  } else {
    Population_fraction <- NULL
  }



  #Delete all columns before "sources"
  stacked_df <- stacked_df[, grep("sources", colnames(stacked_df)):ncol(stacked_df)]

  # Add new first column names id
  data <- cbind(id = seq_len(nrow(stacked_df)), stacked_df)

  sgeo <- inputSource(data)
  mgeo <- inputSample(data)

  return(list(sgeo = sgeo, mgeo = mgeo, Population_fraction = Population_fraction, source_list = source_list, stacked_df_OG = stacked_df_OG))
}
