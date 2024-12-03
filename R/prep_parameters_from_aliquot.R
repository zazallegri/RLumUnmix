#' First part of measure_SAR_OSL_copy()
#'
#' @param aliquot [data.frame] or a [list] of it, a set of grains that are assigned to an
#' aliquot (sample subset used for measurement), i.e., the result of
#' [prepare_Aliquot].
#'
#' @return parameters
#' @export
#'
#' @examples see vignettes
prep_parameters_from_aliquot <- function(
    aliquot
) {

  # First part of [measure_SAR_OSL_copy()]

  # Self-call ---------------------------------------------------------------
  if (is(aliquot, "list"))
    return(lapply(aliquot, prep_parameters_from_aliquot))

  # Check incoming ----------------------------------------------------------
  if (is.null(attributes(aliquot)$package) || attributes(aliquot)$package != "sandbox")
    stop("[prep_parameters_from_aliquot()] the input for aliquot is not an object created by 'sandbox'!",
         call. = FALSE)

  if (!is(aliquot, "data.frame"))
    stop("[prep_parameters_from_aliquot()] the input for aliquot is not of type data.frame!",
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
  # Remove entries in E that start with "osl_E_th" - potential error
  #---------------------------------------------------------------------------------------------------
  # print("Removing entries  in [prep_parameters_from_aliquot()]")
  # # Identify indices of entries that start with "osl_E_th"
  # indices_to_remove <- grep("^osl_E_th", names(parameters$E))

  # # Remove these entries
  # parameters$E <- parameters$E[-indices_to_remove]
  #---------------------------------------------------------------------------------------------------

  #---------------------------------------------------------------------------------------------------
  # Remove name of parameters. Just get list for each parameter (similar to what .set_pars() does)
  #---------------------------------------------------------------------------------------------------
  # print("Removing excessive parameter names in [prep_parameters_from_aliquot()]")
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


  return (parameters)
}
