#' Get Preheat (PH) step info
#'
#' @description
#' Get PH temperature (°C) / duration (s) / heating rate (°C/s)
#'
#'
#' @return PH temperature (°C) / duration (s) / heating rate (°C/s)
#' @export
#'
#' @examples see vignettes
get_PH_user_inputs <- function() {
  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Preheat (PH)")

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
  tkgrid(tklabel(tt, text = "Enter pre-heat temperature (ºC):"), temp_entry)
  tkgrid(tklabel(tt, text = "Enter pre-heat duration (s):"), duration_entry)
  tkgrid(tklabel(tt, text = "Enter pre-heat heating rate (ºC/s) or 'NaN':"), heating_rate_entry)
  tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tkwait.window(tt)

  temp <- as.numeric(tclvalue(temp))
  duration <- as.numeric(tclvalue(duration))

  if (is.na(as.numeric(tclvalue(heating_rate)))) {
    heating_rate <- NaN
  } else {
    heating_rate <- as.numeric(tclvalue(heating_rate))
  }


  # Return the user inputs as a list
  # return(list(temp = temp, duration = duration, heating_rate = heating_rate))
  return(c(temp, duration, heating_rate))
}
