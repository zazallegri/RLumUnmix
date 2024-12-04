#' Local extrema
#'
#' @description
#' Extract local extrema (min and max)
#'
#' @param df Data frame containing TL intensity and temperature
#' @param span Span for function inflect()
#'
#' @return Local extrema
#' @export
#'
#' @examples see vignettes
Get_initial_peaks_valleys <- function(df, span) {

  local_min_max <- inflect(df[, "Intensity"], threshold = span)


  maxima <- df[local_min_max$maxima,]
  minima <- df[local_min_max$minima,]

  if (length(local_min_max$maxima) == 0 & length(local_min_max$minima) > 0) {

    local_extrema <- rbind(
      data.frame(x = df[local_min_max$minima,"Temperature"], y = df[local_min_max$minima,"Intensity"], source = "Min")
    )
  } else if (length(local_min_max$minima) == 0 & length(local_min_max$maxima) > 0) {

    local_extrema <- rbind(
      data.frame(x = df[local_min_max$maxima,"Temperature"], y = df[local_min_max$maxima,"Intensity"], source = "Max")
    )
  } else if (length(local_min_max$maxima) > 0 & length(local_min_max$minima) > 0) {

    # Combine the dataframes
    local_extrema <- rbind(
      data.frame(x = df[local_min_max$maxima,"Temperature"], y = df[local_min_max$maxima,"Intensity"], source = "Max"),
      data.frame(x = df[local_min_max$minima,"Temperature"], y = df[local_min_max$minima,"Intensity"], source = "Min")
    )
  }

  else {
    print("No peaks or valleys found. Exiting.")
    return()
  }

  colnames(local_extrema) <- c("Temperature", "Intensity", "source")

  local_extrema <- local_extrema[order(local_extrema$Temperature),]


  return(local_extrema)

}
