#' Plot fitted peaks individually
#'
#' @description
#' Plot peaks fitted with [tgcd::tgcd()] individually
#'
#'
#' @param df Output of [tgcd::tgcd()]
#' @param npeak Total number of fitted peaks
#'
#' @return Plot of fitted peaks
#' @export
#'
#' @examples see vignettes
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
