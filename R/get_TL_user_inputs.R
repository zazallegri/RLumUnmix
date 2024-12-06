
#' Get TL info
#'
#' @description
#' Get TL begin/end temperature (°C) and heat rate (°C/s)
#'
#'
#' @return TL begin/end temperature (°C) and heat rate (°C/s)
#' @importFrom tcltk tclvalue tclVar tkbutton tkdestroy tkentry tkgrid tklabel tktoplevel tkwait.window tkwm.title
#' @export
#'
#' @examples see vignettes
#'
get_TL_user_inputs <- function() {

  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Thermally stimulated luminescence (TL)")

  # Create variables to store the user inputs
  temp <- tclVar("")
  duration <- tclVar("")
  heating_rate <- tclVar("")

  # Create entry widgets for user inputs
  temp_entry <- tkentry(tt, textvariable = temp)
  duration_entry <- tkentry(tt, textvariable = duration)
  heating_rate_entry <- tkentry(tt, textvariable = heating_rate)

  # Create a submit button
  submit <- function() {
    tkdestroy(tt)
  }
  submit_button <- tkbutton(tt, text = "Submit", command = submit)
  # Arrange the widgets in the window
  tkgrid(tklabel(tt, text = "Enter TL begin temperature (ºC):"), temp_entry)
  tkgrid(tklabel(tt, text = "Enter TL end temperature (ºC):"), duration_entry)
  tkgrid(tklabel(tt, text = "Enter TL heating rate (ºC/s):"), heating_rate_entry)
  tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tkwait.window(tt)

  # Return the user inputs as a list
  # return(list(begin_temp = as.numeric(tclvalue(temp)), end_temp = as.numeric(tclvalue(duration)), heating_rate = as.numeric(tclvalue(heating_rate))))
  return(c(as.numeric(tclvalue(temp)), as.numeric(tclvalue(duration)), as.numeric(tclvalue(heating_rate))))

}
