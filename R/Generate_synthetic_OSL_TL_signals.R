#' Exact copy of [RLumModel::model_LuminescenceSignals()]
#'
#' @description
#' Initially contained more code after model_LuminescenceSignals() and used that function in various scripts. Ended only keeping
#' model_LuminescenceSignals() in that function but kept it in order to not have to modify already written scripts. For thourough
#' documentation, refer to [RLumModel::model_LuminescenceSignals()].
#'
#'
#' @param sequence Set sequence to model
#' @param model [character] (**required**): set model to be used. Available models are:
#' `"Bailey2001"`, `"Bailey2002"`, `"Bailey2004"`, `"Pagonis2007"`, `"Pagonis2008"`, `"Friedrich2017"`, `"Friedrich2018"`, `"Peng2022`" and for own models `"customized"` (or `"customised"`).
#'  Note: When model = `"customized"`/`"customised` is set, the argument `own_parameters` has to be set.
#'
#' @return This function returns an [Luminescence::RLum.Analysis-class] object with all TL, (LM-) OSL and RF/RL steps
#' in the sequence. Every entry is an [Luminescence::RLum.Data.Curve-class] object and can be plotted, analysed etc. with
#' further `RLum`-functions.
#' @export
#'
#' @examples see vignettes
Generate_synthetic_OSL_TL_signals <- function(sequence, model = "Bailey2001") {


  data <- model_LuminescenceSignals(
    sequence = sequence,
    model = model,
    plot = FALSE,
    verbose = FALSE
  )
  return(data)

}
