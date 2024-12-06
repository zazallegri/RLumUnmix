#' Extract OSL metrics
#'
#' @param model.output [RLum.Analysis-class] for a single record
#' @param IRR_dose Irradiation dose
#' @param make_plot Logical. Define whether or not a plot should be made
#'
#' @return Number of OSL components, photoionisation cross section, intensity/sensitivity of each component
#' @export
#'
#' @examples see vignettes
Get_OSL_metrics <- function(model.output, IRR_dose, make_plot = TRUE) {

  if (make_plot) {

    ## Plot the OSL curve
    Luminescence::plot_RLum(object = model.output, log = "y")
  }


  fit <- Luminescence::fit_CWCurve(as.data.frame(Luminescence::get_RLum(model.output)), output.terminal = FALSE)

  if (length(fit$data) == 1 && is.na(fit$data)) {
    print("No fit found. Exiting.")
    return(list(nb_components = NaN, cs_df = NaN, I0_df = NaN, sensitivity = NaN))
  }

  else {
    #Get list of components
    nb_components <- fit$data$n.components
    components <- as.character(seq(1, nb_components))
    print(glue::glue("{nb_components} component(s) found in the OSL curve."))

    ## Get photoionisation cross section
    cs_df <- Get_specific_columns(fit, "cs", components)

    ## Get intensity of each component
    I0_df <- Get_specific_columns(fit, "I0", components)

    return(list(nb_components = nb_components, cs_df = cs_df, I0_df = I0_df, sensitivity = I0_df/IRR_dose))
  }

}
