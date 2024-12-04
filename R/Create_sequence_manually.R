#' Create sequence manually
#'
#' @description
#' Create any sequence containing IRR, PH, TL and OSL steps manually. Additional steps can be easy added to this function.
#'
#'
#' @return Sequence with the same format as output of function [RLumModel::read_SEQ2R()]
#' @export
#'
#' @examples see vignettes
Create_sequence_manually <- function() {

  sequence <- list()
  names_list <- list()

  for (i in seq(get_user_number_of_sequence_steps_input())) {

    step_name <- get_user_sequence_step_input(i)
    if (step_name == ""){
      print("Exiting.")
      return()
    }

    print(step_name)
    names_list[[i]] <- step_name

    # A step can be easily added by creating a new [get_XXX_user_inputs()] function
    if (step_name == "PH") {
      sequence[[i]] <- get_PH_user_inputs()
    } else if (step_name == "IRR") {
      sequence[[i]] <- get_IRR_user_inputs()
    } else if (step_name == "OSL") {
      sequence[[i]] <- get_OSL_user_inputs()
    } else if (step_name == "TL") {
      sequence[[i]] <- get_TL_user_inputs()
    } else {
      print("Invalid sequence step name")
    }
  }

  names(sequence) <- names_list

  return(sequence)
}
