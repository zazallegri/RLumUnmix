#' Routine to obtain TL metrics
#'
#' @param record_num Record number ("Record_x")
#' @param model.output Single TL record of [RLum.Analysis-class]
#' @param Heat_rate Heat rate
#' @param span Span for function [Get_initial_peaks_valleys()]
#' @param automatic_peak_finder Define if the TL peak detection should be done manually (FALSE) or automatically (TRUE).
#' Automatic peak detection performs well on synthetically generated TL curves. For measured TL curves, verification is needed to make sure peaks were detected correctly.
#' If not, set automatic_peak_finder = FALSE and select TL peaks manually.
#' @param INITIAL_ACTIVATION_ENERGY Activation energy
#' @param INITIAL_KINETIC_ORDER Kinetic order
#'
#' @return TL peaks metrics
#' @export
#'
#' @examples see vignettes
Get_TL_metrics <- function(record_num, model.output, Heat_rate, span, automatic_peak_finder,
                           INITIAL_ACTIVATION_ENERGY = 1.4, INITIAL_KINETIC_ORDER = 1.5) {


  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  std_results <- hash::hash()

  # Assuming the DataFrame is obtained from get_RLum
  df <- stats::setNames(as.data.frame(Luminescence::get_RLum(Luminescence::get_RLum(model.output))), c("Temperature", "Intensity"))

  # Convert the temperature to Kelvin for the TGCD function
  df[, "Temperature"] <- df[, "Temperature"] + TEMPERATURE_CONVERSION_CONSTANT


  if (automatic_peak_finder) {

    if (unique(Luminescence::structure_RLum(model.output)['originator'] == ".Risoe.BINfileData2RLum.Data.Curve")) {

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

    else if (unique(Luminescence::structure_RLum(model.output)['originator']) == ".local") {

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

    TL_tgcd_results <- tgcd::tgcd(df, npeak=length(peaks$peaks_intensity), model="g1",
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

    grDevices::windows()
    Luminescence::plot_RLum(object = model.output,
              main = paste("Record", record_num, " - TL curve with"), log = "y")

    peaks <- get_peak_user_inputs(record_num)

    # print(peaks$nb_peaks)
    if (peaks$nb_peaks == 0) {
      print("No peaks detected. Exiting.")
      return()
    }

    TL_tgcd_results <- tgcd::tgcd(df, npeak=peaks$nb_peaks, model="g1",
                            hr=Heat_rate, edit.inis = FALSE, inisPAR = NULL,
                            pickp = "d01")#test this

    #Plot peaks individually
    # Plot_TL_Peaks(TL_tgcd_results$comp.sig, peaks$nb_peaks)

    Peak_intensities <- DIY_TL_peak_intensities_integration(TL_tgcd_results)

    return(list(forced_additional_peak = peaks$forced_additional_peak, nb_peaks = peaks$nb_peaks, peaks_temp = peaks$peaks_temp, peak_intensity = peaks$peaks_intensity, TL_tgcd_results = TL_tgcd_results, Peak_intensities = Peak_intensities))

  }
}
