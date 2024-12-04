#' Reads sequence
#'
#' @description
#'  Reads sequence file from .seq or .RData format.
#'  .RData fromat is created when using function [Create_sequence_manually()] to create a sequence manually (ex from .lseq files).
#'  Function [Get_sequence()] returns a list of lists. Each list corresponds to a sequence step.
#'  Order of the values in each sequence step is described at:
#'  https://r-lum.github.io/RLumModel/reference/model_LuminescenceSignals.html
#'
#'
#' @param file_path Path to sequence file
#' @param lab_dose_rate Dose rate of lab instrument
#'
#' @return Sequence
#' @export
#'
#' @examples see vignettes
Get_sequence <- function(file_path, lab_dose_rate) {




  print(glue("Reading sequence from {file_path}"))
  # If path ends with .SEQ
  if (grepl(".SEQ$", file_path)) {

    sequence <- read_SEQ2R(file = file_path,
                           lab.dose_rate = lab_dose_rate,
                           txtProgressBar = FALSE)

  } else if (grepl(".RData", file_path)) {

    sequence <- readRDS(file_path)

  } else if (grepl(".lseq", file_path)) {

    print("For .lseq files, firstly need to create sequence manually with [Create_sequence_manually()]")

  } else {
    stop("File format not implemented to be read.")
  }

  return(sequence)
}
