

#' Updates parameter using [sandbox::set_Rule()]. Simple sets population x parameter value along the whole depth.
#'
#' @param book Sandbox RuleBook
#' @param parameter_name name of parameter to be changed
#' @param depth [numeric] [list], specifying the depths used for the
#'        interpolation.
#' @param populations_parameters Parameters of quartz luminescence models for every population
#'
#' @return updated RuleBook
#' @export
#'
#' @examples see vignettes
Update_parameter_in_rulebook <- function(book, parameter_name, depth, populations_parameters) {

  n_depth <- length(depth[[1]])
  par_value <- Get_param_normal_dist_list_of_values(parameter_name = parameter_name, populations_parameters, n_depth)

  updated_book <- sandbox::set_Rule(book = book,
                           parameter = parameter_name,
                           value = par_value,
                           depth = depth)

  return(updated_book)
}
