#' Get specific column by (partial-) name
#'
#' @description
#' Selects columns that start with a specific prefix and contain a specific component number
#'
#'
#' @param fit Output of function [Luminescence::fit_CWCurve()]
#' @param col_prefix Prefix that should be contained in the desired columns
#' @param components Component number
#'
#' @return Subset containing only the desired columns
#' @export
#'
#' @examples see vignettes
Get_specific_columns <- function(fit, col_prefix, components) {


  #Find column names that start with "cs"
  prefix_columns <- grep(paste0("^", col_prefix), colnames(fit$data), value = TRUE)

  #Subset the DataFrame to include only those columns
  prefix_df <- fit$data[, prefix_columns]


  #Keep only cs/cs.rel columns that contain the component number
  columns_to_keep <- prefix_columns[sapply(prefix_columns, function(x) any(sapply(components, function(y) grepl(y, x))))]

  #Subset the DataFrame to include only those columns
  prefix_df <- prefix_df[, columns_to_keep]

  return(prefix_df)
}
