#' Computation of the differences between the mean/median fractions and the expected population fractions
#'
#' @description
#' If the expected population fraction is available, this function computes the differences between the mean/median and the expected population fractions
#'
#'
#' @param source_list List of sources c("S1", "S2",...)
#' @param Population_fraction Expected populations proportions
#' @param proportions Information of every source contained in a df: filename	variable	mean	median	sd	n	Q25	Q75	lower.ci	upper.ci	error_threshold	score_threshold	nb_tracer	Expected_population_fraction
#' @param filename Name of file
#' @param tracer_pair Pair of tracers used
#'
#' @return Differences
#' @export
#'
#' @examples see vignettes
Compute_mean_median_proportions_differences <- function(source_list, Population_fraction, proportions, filename, tracer_pair = NULL) {



  if (!is.null(Population_fraction)) {



    # Add the expected population fraction to the dataframe if available
    # proportions <- cbind(proportions, Population_fraction[Population_fraction$sources != "Mixed", "Pop_fraction"])
    # Fill last 3 rows (S1, S2, S3) with fraction
    proportions$Expected_population_fraction[(nrow(proportions)-length(source_list)+1):nrow(proportions)] <- unlist(Population_fraction[Population_fraction$sources != "Mixed", "Pop_fraction"])

    temp_proportions <- proportions[(nrow(proportions)-length(source_list)+1):nrow(proportions), ]

    # Compute the differences between the expected population fraction and the mean and median of the proportions
    temp_list_differences <- list()
    temp_list_differences_names <- list()
    for (source_name in source_list) {

      median_diff <- temp_proportions[temp_proportions$variable == source_name, "median"] - Population_fraction[Population_fraction$sources == source_name, "Pop_fraction"]
      mean_diff <- temp_proportions[temp_proportions$variable == source_name, "mean"] - Population_fraction[Population_fraction$sources == source_name, "Pop_fraction"]

      # Add median_diff and mean_diff to the list with "source_name" as name
      temp_list_differences <- append(temp_list_differences, c(median_diff, mean_diff))
      temp_list_differences_names <- append(temp_list_differences_names, c(paste0(source_name, "_median_diff"), paste0(source_name, "_mean_diff")))

    }
    # Rename list values
    names(temp_list_differences) <- temp_list_differences_names
    if (!is.null(tracer_pair)) {
      temp_list_differences <- append(c(tracer_pair = tracer_pair), temp_list_differences)
    }
    temp_list_differences <- append(c(filename = filename), temp_list_differences)

    return(list(proportions = proportions, temp_list_differences = temp_list_differences))

  } else {

    return(list(proportions = proportions, temp_list_differences = NULL))
  }

}
