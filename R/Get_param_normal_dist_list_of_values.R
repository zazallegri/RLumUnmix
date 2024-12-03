
#' Prepare normal distribution of parameter for every depth value for sandbox rule book
#'
#' @description In sandbox rule book, at every depth step, the value of the parameters must be defined. It is often done by using a normal distribution.
#'  Note that in function [Get_param_normal_dist_list_of_values()], the sd = 0. The function basically does the same as (for parameter N1 and two populations):
#'
#'  par_N1 <- list(list(mean = seq(from = 1.5e+07, to = 1.5e+07, length.out = n_depth),
#'                   sd = seq(from = 0, to = 0, length.out = n_depth)),# Default N1 for pop1
#'                   list(mean = seq(from = 0, to = 0, length.out = n_depth),
#'                   sd = seq(from = 0, to = 0, length.out = n_depth)))# No N1 for pop2
#'
#'
#'
#' @param parameter_name Name of parameter to act upon
#' @param populations_parameters Parameters of quartz luminescence models for every population
#' @param n_depth Number of depth values
#'
#' @return list of list with normal distribution for 'parameter_name'
#' @export
#'
#' @examples see vignettes
Get_param_normal_dist_list_of_values <- function(parameter_name, populations_parameters, n_depth) {


  output <- Separate_numeric_and_non_numeric_part(parameter_name)
  parameter_letter <- output$non_numeric_part
  parameter_number <- output$numeric_part

  nb_populations <- length(populations_parameters)
  parameter_normal_distribution_values <- list()

  sd <- seq(from = 0, to = 0, length.out = n_depth)

  for (i in 1:nb_populations) {

    # print(glue("Updating parameter {parameter_name} for population {i}"))
    mean <- seq(from = populations_parameters[[i]][[parameter_letter]][parameter_number],
                to = populations_parameters[[i]][[parameter_letter]][parameter_number],
                length.out = n_depth)

    parameter_normal_distribution_values <- append(parameter_normal_distribution_values, list(list(mean = mean, sd = sd)))
  }

  return(parameter_normal_distribution_values)

}
