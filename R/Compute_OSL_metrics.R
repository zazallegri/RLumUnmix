#' Compute and store OSL metrics
#'
#' @description
#' Computes OSL metrics using Get_OSL_metrics() and stores the results in hash object
#'
#'
#' @param sequence Set sequence to model
#' @param data [RLum.Analysis-class] containing OSL record
#'
#' @return Hash object containing OSL metrics (cross-section, relative cross-section, intensity, sensitivity)
#' @export
#'
#' @examples see vignettes
Compute_OSL_metrics <- function(sequence, data) {


  # Example for argument data:

  #  [RLum.Analysis-class]
  # 	 originator: model_LuminescenceSignals()
  # 	 protocol: Bailey2001
  # 	 additional info elements:  0
  # 	 number of records: 2
  # 	 .. : RLum.Data.Curve : 2
  # 	 .. .. : #1 OSL | #2 OSL





  SEQ_sequence_renamed <- Rename_sequence(sequence)
  nb_OSL_records <- length(data)
  print(glue::glue("{nb_OSL_records} OSL records"))

  OSL_hash <- hash::hash()

  for (record_num in 1:nb_OSL_records) {



    ## TODO: AUTOMATE IRR_DOSE
    # IRR_dose <- SEQ_sequence_renamed[[paste0("IRR_", 1)]][2] - not always the correct one
    output <- Get_OSL_metrics(data[[record_num]], IRR_dose = 50)

    if (is.na(output$nb_components)) {
      OSL_hash[[paste0("Record_",record_num)]][["Nb_components"]] <- NaN
    } else {

      OSL_hash[[paste0("Record_",record_num)]][["Nb_components"]] <- output$nb_components

      for (component_num in 1:output$nb_components) {

        OSL_hash[[paste0("Record_", record_num)]][["Cross_section"]][[paste0("Component_", component_num)]] <- output$cs_df[,paste0("cs", component_num)]
        OSL_hash[[paste0("Record_", record_num)]][["Cross_section_relative"]][[paste0("Component_", component_num)]] <- output$cs_df[, paste0("cs", component_num, ".rel")]
        OSL_hash[[paste0("Record_", record_num)]][["Intensity"]][[paste0("Component_", component_num)]] <- output$I0_df[, paste0("I0", component_num)]
        OSL_hash[[paste0("Record_", record_num)]][["Sensitivity"]][[paste0("Component_", component_num)]] <- output$sensitivity[, paste0("I0", component_num)]

      }

    }
  }

  return(OSL_hash)

}
