#' Define sequence steop
#'
#' @param i Step number in the sequence
#'
#' @return Sequence step PH, IRR, OSL or TL
#' @export
#'
#' @examples see vignettes
get_user_sequence_step_input <- function(i) {
  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Sequence creator")

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
  tkgrid(tklabel(tt, text = glue("Enter sequence step {i}:\nPH, IRR, OSL or TL")), entry)
  tkgrid(submit_button)

  # Wait for the user to submit the input
  tkwait.window(tt)

  # Return the user input
  return(as.character(tclvalue(user_input)))
}
