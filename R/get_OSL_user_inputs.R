#' Get OSL info
#'
#' @description
#' Get OSL temperature (°C) / durations (s) / optical power (%)
#'
#'
#' @return OSL temperature (°C) / durations (s) / optical power (%)
#' @importFrom tcltk tclvalue tclVar tkbutton tkdestroy tkentry tkgrid tklabel tktoplevel tkwait.window tkwm.title
#' @export
#'
#'
#'
#' @examples see vignettes
get_OSL_user_inputs <- function() {

  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Optically stimulated luminescence (OSL)")

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
  tkgrid(tklabel(tt, text = "Enter OSL temperature (ºC):"), temp_entry)
  tkgrid(tklabel(tt, text = "Enter OSL duration (s):"), duration_entry)
  tkgrid(tklabel(tt, text = "Enter OSL optical power (%):"), heating_rate_entry)
  tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tkwait.window(tt)

  # Return the user inputs as a list
  # return(list(temp = as.numeric(tclvalue(temp)),
  #             duration = as.numeric(tclvalue(duration)),
  #             optical_power = as.numeric(tclvalue(heating_rate))))
  return(c(as.numeric(tclvalue(temp)),
           as.numeric(tclvalue(duration)),
           as.numeric(tclvalue(heating_rate))))

}
