#' Get irradiation (IRR) step info
#'
#' @description
#' Get IRR temperature (°C) / dose (Gy) / dose rate (Gy/s) / time (s)
#'
#'
#' @return IRR temperature (°C) / dose (Gy) / dose rate (Gy/s) / time (s)
#' @export
#'
#' @examples see vignettes
#'
get_IRR_user_inputs <- function() {
  # Create a new tcltk window
  tt <- tktoplevel()

  # Set the title of the window
  tkwm.title(tt, "Irradiation (IRR)")

  # Create variables to store the user inputs
  temp <- tclVar("")
  dose <- tclVar("")
  dose_rate <- tclVar("")
  time <- tclVar("")

  # Create entry widgets for user inputs
  temp_entry <- tkentry(tt, textvariable = temp)
  dose_entry <- tkentry(tt, textvariable = dose)
  dose_rate_entry <- tkentry(tt, textvariable = dose_rate)
  time_entry <- tkentry(tt, textvariable = time)

  # Create a submit button
  submit <- function() {
    tkdestroy(tt)
  }
  submit_button <- tkbutton(tt, text = "Submit", command = submit)
  # Arrange the widgets in the window
  tkgrid(tklabel(tt, text = "Enter irradiation temperature (ºC):"), temp_entry)
  tkgrid(tklabel(tt, text = "Enter irradiation dose (Gy) or 'NaN':"), dose_entry)
  tkgrid(tklabel(tt, text = "Enter irradiation dose rate (Gy/s) or 'NaN':"), dose_rate_entry)
  tkgrid(tklabel(tt, text = "Enter irradiation time (s) or 'NaN':"), time_entry)
  tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tkwait.window(tt)

  # Either we use the dose or the time and dose rate
  if (is.na(as.numeric(tclvalue(dose)))) {

    print("Using time and dose rate to compute dose.")
    time <- as.numeric(tclvalue(time))
    dose_rate <- as.numeric(tclvalue(dose_rate))
    dose <- time * dose_rate

  } else if (!is.na(as.numeric(tclvalue(dose)))) {
    print("Using input dose.")
    dose <- as.numeric(tclvalue(dose))
    dose_rate <- as.numeric(tclvalue(dose_rate))
  } else {
    print("No dose or time and dose rate entered. Exiting.")
    return()
  }

  # Return the user inputs as a list
  return(c(as.numeric(tclvalue(temp)), dose, dose_rate))
}
