#' Compute and store TL metrics
#'
#' @description
#' Computes and stores TL metrics of all TL records of sequence
#'
#'
#' @param sequence Set sequence to model
#' @param data [RLum.Analysis-class] of TL records
#' @param automatic_peak_finder Define if the TL peak detection should be done manually (FALSE) or automatically (TRUE).
#' Automatic peak detection performs well on synthetically generated TL curves. For measured TL curves, verification is needed to make sure peaks were detected correctly.
#' If not, set automatic_peak_finder = FALSE and select TL peaks manually.
#'
#' @return Hash object containing TL metrics
#' @export
#'
#' @examples see vignettes
Compute_TL_metrics <- function(sequence, data, automatic_peak_finder) {


  # Example of argument data
  #  [RLum.Analysis-class]
  # 	 originator: model_LuminescenceSignals()
  # 	 protocol: Bailey2001
  # 	 additional info elements:  0
  # 	 number of records: 5
  # 	 .. : RLum.Data.Curve : 5
  # 	 .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL


  SEQ_sequence_renamed <- Rename_sequence(sequence)
  nb_TL_records <- length(data)
  print(glue("{nb_TL_records} TL records"))


  TL_hash <- hash()

  for(record_num in 1:nb_TL_records){


    print(glue("Record number: {record_num}"))

    Heat_rate <- SEQ_sequence_renamed[[paste0("TL_", record_num)]][3] # Get hear rate from SEQ file
    # print(glue("Heat rate: {Heat_rate}"))

    output <- Get_TL_metrics(record_num, get_RLum(data, recordType ='TL$' , drop = FALSE)[record_num],
                             Heat_rate = Heat_rate, span = 5, automatic_peak_finder = automatic_peak_finder)

    if (is.null(output$nb_peaks)) {
      TL_hash[[paste0("Record_", record_num)]][["Nb_peaks"]] <- 0
      next
    }

    TL_hash[[paste0("Record_", record_num)]][["Forced_additional_peak"]] <- output$forced_additional_peak
    TL_hash[[paste0("Record_", record_num)]][["Nb_peaks"]] <- output$nb_peaks

    if (output$forced_additional_peak) {
      unforced_nb_peaks <- output$nb_peaks - 1
    } else {
      unforced_nb_peaks <- output$nb_peaks
    }

    for(peak_num in 1:unforced_nb_peaks) {

      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_max"]][[paste0("Peak_", peak_num)]] <- output$TL_tgcd_results$pars[paste0(glue("{peak_num}th-Peak")), "INTENS(Im)"]
      TL_hash[[paste0("Record_", record_num)]][["Frequency_factor"]][[paste0("Peak_", peak_num)]] <- output$TL_tgcd_results$ff[[paste0(glue("{peak_num}th-Peak"))]]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_integrated"]][[paste0("Peak_", peak_num)]] <- output$Peak_intensities[paste0(glue("Peak.{peak_num}")), "Peak_intensities"]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_integrated_relative"]][[paste0("Peak_", peak_num)]] <- output$Peak_intensities[paste0(glue("Peak.{peak_num}")), "Peak_intensities_relative"]

      TL_hash[[paste0("Record_", record_num)]][["Peak_temperature"]][[paste0("Peak_", peak_num)]] <- output$peaks_temp[peak_num]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity"]][[paste0("Peak_", peak_num)]] <- output$peak_intensity[peak_num]

    }

  }

  return(TL_hash)

}

