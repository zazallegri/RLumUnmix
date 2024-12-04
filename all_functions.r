





Plot_TL_Curve_with_extrema <- function(model_output, local_extrema, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  peaks_temp <- local_extrema[local_extrema$source == "Max", "Temperature"]
  peaks_intensity <- local_extrema[local_extrema$source == "Max", "Intensity"]
  valleys_temp <- local_extrema[local_extrema$source == "Min", "Temperature"]
  valleys_intensity <- local_extrema[local_extrema$source == "Min", "Intensity"]

  nb_peaks <- length(peaks_temp)
  nb_valleys <- length(valleys_temp)

  plot_RLum(object = model_output, log = "y", main = paste("Record ", record_num, "- Thermoluminescence (TL) curve with", nb_peaks, "peaks and", nb_valleys, "valleys"))
  if (nb_peaks != 0 | nb_valleys != 0) {
  points(peaks_temp - TEMPERATURE_CONVERSION_CONSTANT, peaks_intensity, col = "red", pch = 19)
  points(valleys_temp - TEMPERATURE_CONVERSION_CONSTANT, valleys_intensity, col = "blue", pch = 19)
  legend("bottomright", legend = c("Estimated peak locations", "Estimated valley locations"), col = c("red", "blue"), pch = 19)
  }

}

Plot_TL_Peaks <- function(df, npeak) {

  for (peak in 1:npeak) {

    peak_name <- paste0("Peak.", peak)

    df_subset <- df[,c("Temperature","Obs.Signal",peak_name)]


    # Define different line types and colors
    line_types <- 1:ncol(df[, -1])
    colors <- rainbow(ncol(df[, -1]))
    colors = c("black", "red")

    # Plot all columns against the first column with different styles
    matplot(df_subset[, 1], df_subset[, -1], type = "l", lty = line_types, col = colors, lwd = 3,
            xlab = paste(colnames(df_subset)[1], "(K)"), ylab = "TL intensity (counts)")

    # Add a legend
    legend("topright", legend = colnames(df_subset)[-1], col = colors, lty = line_types)

  }

}

DIY_TL_peak_intensities_integration <- function(TL_tgcd_results) {

  if (length(grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)) == 1) {

    # Col have names Peak.1, Peak.2, etc.
    TL_tgcd_peak_intensity_cols_only <- as.data.frame(TL_tgcd_results$comp.sig[, grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)])
    colnames(TL_tgcd_peak_intensity_cols_only) <- "Peak.1"
  } else {

    # Col have names Peak.1, Peak.2, etc.
    TL_tgcd_peak_intensity_cols_only <- TL_tgcd_results$comp.sig[, grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)]
  }


    # Single values
    temperature_resolution <- mean(diff(TL_tgcd_results$comp.sig[, "Temperature"]))
    Peak_intensities <- colSums(TL_tgcd_peak_intensity_cols_only*temperature_resolution)
    # Peak.1, Peak.2, etc. are now row names with col "Peak_intensities"
    Peak_intensities <- as.data.frame(Peak_intensities)
    # Compute the relative peak intensities (relative to peak 1)
    peak_1_value <- Peak_intensities["Peak.1", "Peak_intensities"]
    Peak_intensities$Peak_intensities_relative <- Peak_intensities$Peak_intensities / peak_1_value

    return(Peak_intensities)
}

Get_TL_metrics <- function(record_num, model.output, Heat_rate, span, automatic_peak_finder,
                            INITIAL_ACTIVATION_ENERGY = 1.4, INITIAL_KINETIC_ORDER = 1.5) {


  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  std_results <- hash()

  # Assuming the DataFrame is obtained from get_RLum
  df <- setNames(as.data.frame(get_RLum(get_RLum(model.output))), c("Temperature", "Intensity"))

  # Convert the temperature to Kelvin for the TGCD function
  df[, "Temperature"] <- df[, "Temperature"] + TEMPERATURE_CONVERSION_CONSTANT


  if (automatic_peak_finder) {

    if (unique(structure_RLum(model.output)['originator'] == ".Risoe.BINfileData2RLum.Data.Curve")) {

      print("Data from experimental signal")
      data <- TL_curve_smoothing(df, record_num, lambda = 50)
      local_extrema <- Get_initial_peaks_valleys(df = data, span = span)

      temp_results <- Keep_relevant_peaks(local_extrema = local_extrema, df = df, smoothing_data = data)

      if (!is.null(temp_results$std_results)) {
        std_results <- temp_results$std_results
        Plot_TL_Curve_with_std(model.output, std_results, record_num)
      }
      peaks <- Add_last_datapoint_for_potential_peak(last_datapoint = tail(df[df$Intensity != 0, ],1),
                                                    last_local_max = tail(local_extrema[local_extrema$source == "Max", ], 1),
                                                    last_local_min = tail(local_extrema[local_extrema$source == "Min", ], 1),
                                                    temp_results$peaks)

      # Plot_TL_Curve_with_extrema(model.output, local_extrema, record_num)

    }

    else if (unique(structure_RLum(model.output)['originator']) == ".local") {

      print("Data from synthetic signal")
      # Automatic peak detection
      peaks <- Get_initial_peaks(df, span)

    }

    else {
      print("Unknown originator. Exiting.")
      return()
    }




    # Make plot of TL with estimates of peak location
    Plot_TL_Curve(model.output, peaks, record_num)

    if (peaks$nb_peaks == 0) {
      print("No peaks detected. Exiting.")
        return()
      }



    # Rough estimates for the parameters then deconvolution
    knPars <-
      cbind(peaks$peaks_intensity, # Im
            rep(INITIAL_ACTIVATION_ENERGY, peaks$nb_peaks), # E
            peaks$peaks_temp,  # Tm
            rep(INITIAL_KINETIC_ORDER, peaks$nb_peaks))  # b ranges between 1-2

    TL_tgcd_results <- tgcd(df, npeak=length(peaks$peaks_intensity), model="g1",
                inisPAR=knPars, edit.inis=FALSE, hr=Heat_rate, plot = FALSE)

    #Plot peaks individually
    # Plot_TL_Peaks(TL_tgcd_results$comp.sig, peaks$nb_peaks)




    Peak_intensities <- DIY_TL_peak_intensities_integration(TL_tgcd_results)


    # ## Print desired results
    # print("Max peak intensities:")
    # print(TL_tgcd_results$pars)
    # print("Peak frequency factors:")
    # print(TL_tgcd_results$ff)
    # print("Integrated peak intensities:")
    # print(Peak_intensities)

    return(list(forced_additional_peak = peaks$forced_additional_peak, nb_peaks = peaks$nb_peaks, peaks_temp = peaks$peaks_temp, peak_intensity = peaks$peaks_intensity, TL_tgcd_results = TL_tgcd_results, Peak_intensities = Peak_intensities))
  } else {

    windows()
    plot_RLum(object = model.output,
                  main = paste("Record", record_num, " - TL curve with"), log = "y")

    peaks <- get_peak_user_inputs(record_num)

    # print(peaks$nb_peaks)
    if (peaks$nb_peaks == 0) {
      print("No peaks detected. Exiting.")
        return()
      }

    TL_tgcd_results <- tgcd(df, npeak=peaks$nb_peaks, model="g1",
                              hr=Heat_rate, edit.inis = FALSE, inisPAR = NULL,
                              pickp = "d01")#test this

    #Plot peaks individually
    # Plot_TL_Peaks(TL_tgcd_results$comp.sig, peaks$nb_peaks)

    Peak_intensities <- DIY_TL_peak_intensities_integration(TL_tgcd_results)

    return(list(forced_additional_peak = peaks$forced_additional_peak, nb_peaks = peaks$nb_peaks, peaks_temp = peaks$peaks_temp, peak_intensity = peaks$peaks_intensity, TL_tgcd_results = TL_tgcd_results, Peak_intensities = Peak_intensities))

    }
}


Rename_sequence <- function(list_of_lists) {

  # Get the names of the sublists
  sublists_names <- names(list_of_lists)

  # Create a new list with unique names
  unique_list_of_lists <- list()
  name_count <- list()

  for (i in seq_along(list_of_lists)) {
    name <- sublists_names[i]
    if (is.null(name_count[[name]])) {
      name_count[[name]] <- 1
      name <- paste0(name, "_1")
    } else {
      name_count[[name]] <- name_count[[name]] + 1
      name <- paste0(name, "_", name_count[[name]])
    }
    unique_list_of_lists[[name]] <- list_of_lists[[i]]
  }

  return(unique_list_of_lists)
}


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



