#' Sets rule in RuleBook for all model parameters
#'
#' @description
#' Applies function [Update_parameter_in_rulebook()] to the first 7 parameters of model 'Bailey2001'
#'
#'
#' @param book Sandbox RuleBook
#' @param depth depth [numeric] [list], specifying the depths used for the interpolation.
#' @param populations_parameters Parameters of quartz luminescence models for every population
#'
#' @return Updated RuleBook
#' @export
#'
#' @examples see vignettes
set_Rule_for_all_model_parameters <- function(book, depth, populations_parameters) {

  var_names <- list()
  parameter_names <- names(RLumModel::.set_pars("Bailey2001")[1:7])

  # Format names in a way that they can be used in the rulebook
  for (parameter_name in parameter_names) {
    for (parameter_value in seq(1, length(.set_pars("Bailey2001")[[parameter_name]]))) {

      var_names <- append(unlist(var_names), unlist(paste0("osl_", parameter_name, parameter_value)))

    }
  }

  for (var_name in var_names) {

    book <- Update_parameter_in_rulebook(book = book,
                                         parameter_name = var_name,
                                         depth = depth,
                                         populations_parameters = populations_parameters)

  }

  return(book)
}
