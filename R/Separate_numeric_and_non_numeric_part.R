#' Separate numeric and non-numeric part of parameter name
#'
#' @description Separates 'osl_E_th1' into 'E_th' and 1
#'
#' @param parameter_name Name of parameter
#'
#' @return Numeric and non-numeric parts of parameter_name
#' @export
#'
#' @examples Separate_numeric_and_non_numeric_part('osl_E_th1')

Separate_numeric_and_non_numeric_part <- function(parameter_name) {

  # Changes osl_E_th1 to e_th and 1

  # Remove the "osl_" part
  parameter <- gsub("osl_", "", parameter_name)

  # Extract the numeric part
  numeric_part <- sub(".*?(\\d+)$", "\\1", parameter)

  # Extract the non-numeric part
  non_numeric_part <- sub("(.*?)(\\d+)$", "\\1", parameter)

  return(list(non_numeric_part = non_numeric_part, numeric_part = as.numeric(numeric_part)))
}
