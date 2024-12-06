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
  tt <- tcltk::tktoplevel()

  # Set the title of the window
  tcltk::tkwm.title(tt, "Irradiation (IRR)")

  # Create variables to store the user inputs
  temp <- tcltk::tclVar("")
  dose <- tcltk::tclVar("")
  dose_rate <- tcltk::tclVar("")
  time <- tcltk::tclVar("")

  # Create entry widgets for user inputs
  temp_entry <- tcltk::tkentry(tt, textvariable = temp)
  dose_entry <- tcltk::tkentry(tt, textvariable = dose)
  dose_rate_entry <- tcltk::tkentry(tt, textvariable = dose_rate)
  time_entry <- tcltk::tkentry(tt, textvariable = time)

  # Create a submit button
  submit <- function() {
    tcltk::tkdestroy(tt)
  }
  submit_button <- tcltk::tkbutton(tt, text = "Submit", command = submit)
  # Arrange the widgets in the window
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter irradiation temperature (ºC):"), temp_entry)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter irradiation dose (Gy) or 'NaN':"), dose_entry)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter irradiation dose rate (Gy/s) or 'NaN':"), dose_rate_entry)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter irradiation time (s) or 'NaN':"), time_entry)
  tcltk::tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tcltk::tkwait.window(tt)

  # Either we use the dose or the time and dose rate
  if (is.na(as.numeric(tcltk::tclvalue(dose)))) {

    print("Using time and dose rate to compute dose.")
    time <- as.numeric(tcltk::tclvalue(time))
    dose_rate <- as.numeric(tcltk::tclvalue(dose_rate))
    dose <- time * dose_rate

  } else if (!is.na(as.numeric(tcltk::tclvalue(dose)))) {
    print("Using input dose.")
    dose <- as.numeric(tcltk::tclvalue(dose))
    dose_rate <- as.numeric(tcltk::tclvalue(dose_rate))
  } else {
    print("No dose or time and dose rate entered. Exiting.")
    return()
  }

  # Return the user inputs as a list
  return(c(as.numeric(tcltk::tclvalue(temp)), dose, dose_rate))
}
