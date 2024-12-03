#' @title Slightly modified copy of sandbox::measure_SAR_OSL()
#'
#' @description The function models the time-dependent photon counts of an aliquot
#' according to the specified CW SAR OSL (continuous wave, single aliquot
#' regenerative dose protocol for optically stimulated luminescence) sequence
#' and parameters. The modelling is done for each component and photon count
#' curves are summed to return an [Luminescence::RLum.Analysis-class] object as equivalent of
#' importing a real measurement data set to the R-package `Luminescence-package`.
#'
#' The function uses the package [RLumModel::RLumModel-package] to perform the simulation of the
#' photon count curves.
#'
#' It was modified to fit the need of RLumUnmix package. Modifications were made clear in code.
#'
#' @param aliquot [data.frame] or a [list] of it, a set of grains that are assigned to an
#' aliquot (sample subset used for measurement), i.e., the result of
#' [prepare_Aliquot].
#'
#' @param sequence [list], definition of the SAR protocol.
#'
#' @param dose_rate [numeric] value, Dose rate of the luminescence
#' reader, in Gy/s.
#'
#' @return [Luminescence::RLum.Analysis-class] object. Equivalent of the import result for
#' a real world measurement file. This object can be evaluated by functions
#' of the package `Luminescence-package`.
#'
#' @author Michael Dietze, GFZ Potsdam (Germany),
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom),
#' Modified by Balthazar Allegri
#'
#' @export
#'
#' @examples see vignettes
measure_SAR_OSL_copy <- function(
    aliquot,
    sequence,
    dose_rate = 0.1
) {

  # This function is a copy of [measure_SAR_OSL()] from the sandbox package

  # Self-call ---------------------------------------------------------------
  if (is(aliquot, "list"))
    return(lapply(aliquot, measure_SAR_OSL, sequence = sequence, dose_rate = dose_rate))

  # Check incoming ----------------------------------------------------------
  if (is.null(attributes(aliquot)$package) || attributes(aliquot)$package != "sandbox")
    stop("[measure_SAR_OSL()] the input for aliquot is not an object created by 'sandbox'!",
         call. = FALSE)

  if (!is(aliquot, "data.frame"))
    stop("[measure_SAR_OSL()] the input for aliquot is not of type data.frame!",
         call. = FALSE)


  ## PART 1 - separate OSL components -----------------------------------------
  ## get column IDs for further processing
  col_names <- colnames(aliquot)
  col_names <- col_names[grepl("osl_", col_names)]

  ## calculate column means (for the variables for interest)
  col_means <- colMeans(aliquot[,col_names])

  ## get unique variable names
  var_names <- unique(unlist(regmatches(col_names, regexec(
    "(?<=osl\\_)\\D+"
    , col_names, perl = TRUE))))

  ## fetch relevant values from the table and write into new variable
  parameters <- lapply(var_names, function(v){
    col_means[grepl(paste0("osl_", v,"\\d?"), col_names, perl = TRUE)]

  })

  ## add parameter names to the list
  names(parameters) <- var_names

  ## add model information
  parameters <- c(parameters, model = "customized")


  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  ##--------------- MODIFICATIONS START --------------------------------------------------------------


  #---------------------------------------------------------------------------------------------------
  # Discard original steps
  #---------------------------------------------------------------------------------------------------
  # ## calculate mean burial dose
  # burial_dose <- mean(aliquot$osl_doserate * aliquot$age)

  # ## update sequence
  # sequence$Irr_2recover <- burial_dose
  #---------------------------------------------------------------------------------------------------


  #---------------------------------------------------------------------------------------------------
  # Remove entries in E that start with "osl_E_th" - potential error (doesn't disturd the workflow)
  #---------------------------------------------------------------------------------------------------
  # print("Removing entries  in [measure_SAR_OSL_copy()]")
  # # Identify indices of entries that start with "osl_E_th"
  # indices_to_remove <- grep("^osl_E_th", names(parameters$E))

  # # Remove these entries
  # parameters$E <- parameters$E[-indices_to_remove]
  #---------------------------------------------------------------------------------------------------

  #---------------------------------------------------------------------------------------------------
  # Remove name of parameters. Just get list for each parameter (similar to what .set_pars() does)
  #---------------------------------------------------------------------------------------------------
  # print("Removing excessive parameter names in [measure_SAR_OSL_copy()]")
  # own_paramters <- list()

  # for (name in names(parameters)){
  #     own_paramters[[name]] <- unname(unlist(parameters[name]))
  # }
  # parameters <- own_paramters
  #---------------------------------------------------------------------------------------------------



  ##--------------- MODIFICATIONS END ----------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------


  ## PART 2 - model luminescence ----------------------------------------------
  osl_model <- RLumModel::model_LuminescenceSignals(
    model = parameters$model,
    sequence = sequence,
    lab.dose_rate = dose_rate,
    own_parameters = parameters,
    plot = FALSE,
    verbose = FALSE,
    simulate_sample_history = TRUE)


  #Doesn't return osl_model only. Also want to see what parameters were used in [RLumModel::model_LuminescenceSignals()]
  output <- hash()
  output$osl_model <- osl_model
  output$parameters <- parameters
  output$sequence <- sequence


  ## return function output
  return(output)
}
