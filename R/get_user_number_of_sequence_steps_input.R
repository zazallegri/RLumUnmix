#' Get number of sequence steps from user
#'
#' @description
#' Get user input on the number of step in sequence when creating it manually
#'
#'
#' @return Number of steps in the manually created sequence
#' @importFrom tcltk tclvalue tclVar tkbutton tkdestroy tkentry tkgrid tklabel tktoplevel tkwait.window tkwm.title
#' @export
#'
#' @examples see vignettes
get_user_number_of_sequence_steps_input <- function() {

  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Sequence creater")

  # Create a variable to store the user input
  user_input <- tclVar("")

  # Create an entry widget for user input
  entry <- tkentry(tt, textvariable = user_input)

  # Create a submit button
  submit <- function() {
    tkdestroy(tt)
  }
  submit_button <- tkbutton(tt, text = "Submit", command = submit)

  # Arrange the widgets in the window
  tkgrid(tklabel(tt, text = "Enter number of steps in the sequence:"), entry)
  tkgrid(submit_button)

  # Wait for the user to submit the input
  tkwait.window(tt)

  # Return the user input
  return(as.numeric(tclvalue(user_input)))
}
