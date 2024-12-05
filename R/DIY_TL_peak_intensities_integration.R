#' Manual integration of peak intensities
#'
#' @param TL_tgcd_results Output of [tgcd::tgcd()]
#'
#' @return Integrated peak intensities
#' @export
#'
#' @examples see vignettes
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
