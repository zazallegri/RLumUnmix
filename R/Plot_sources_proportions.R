#' Boxplot of estimated population proportions + true proportions if provided
#'
#' @param result_FP_long Result of unmixing
#' @param filename Name of file
#' @param data_sol Data frame containing tracers used for unmixing. Output of function [Select_data_for_unmixing()]
#' @param Population_fraction Expected population fraction
#' @param source_list List of sources c("S1", "S2",...)
#' @param tracer_pair Pair of tracers used
#'
#' @return Boxplot with estimated and expected population fractions
#' @export
#'
#' @examples see vignettes
Plot_sources_proportions <- function(result_FP_long, filename, data_sol, Population_fraction, source_list, tracer_pair = NULL) {

  # Get number of tracers used for the unmixing
  nb_tracers <- length(colnames(data_sol)[!(colnames(data_sol) %in% c("id", "sources", "n"))])/2

  graphics::par(bg = "white")

  if (!is.null(tracer_pair)) {
    # Add tracer pair to the plot
    title <- paste0("Boxplot for ", paste0(source_list, collapse = ", ")," - ratio: ", filename, " - Tracers: ", nb_tracers, "
    Tracer pair: ", tracer_pair)
  } else {
    title <- paste0("Boxplot for ", paste0(source_list, collapse = ", ")," - ratio: ", filename, " - Tracers: ", nb_tracers)
  }

  # Create boxplot
  graphics::boxplot(value ~ variable, data = result_FP_long,
                    main = title,
                    xlab = "Variable", ylab = "Value", col = "lightgray", ylim = c(-0.1, 1.1))

  graphics::grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

  if (!is.null(Population_fraction)) {
    # Add red points
    points_plot <- Population_fraction[Population_fraction$sources != 'Mixed', ]
    points_plot$sources_int <- as.integer(gsub("S", "", points_plot$sources))
    graphics::points(points_plot$sources_int, points_plot$Pop_fraction, col = "red", pch = 19, cex = 1.5)
  }

}
