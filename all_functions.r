



Prepare_data_for_tracer_selection <- function(data_path) {

  ############################################################
  # The df stored in data_path should have the same structure as the df named 'unmixing_df' (in the example data folder)
  # First column should be named "sources": each source in that column should be named  "S1", "S2", "S3", ... and "Mixed"
  # The following columns should be the potential tracers
  # The last column should be named "Pop_fraction" and should contain the population fraction of each source. Not mandatory
  ############################################################

  # Read stacled_df
  stacked_df <- read.csv(file = data_path)

  # Delete columns that contains only zeros
  stacked_df <- stacked_df[, colSums(stacked_df != 0, na.rm = TRUE) > 0]

  stacked_df_OG <- stacked_df
  # Get all unique values in the column "sources" that starts with S
  source_list <- unique(stacked_df[grepl("^S", stacked_df$sources), "sources"])

  if (colnames(stacked_df)[ncol(stacked_df)] == "Pop_fraction") {

    # Get Population_fraction
    Population_fraction <- stacked_df[, c("sources", "Pop_fraction")]

    # Delete last column of stacked_df
    stacked_df <- stacked_df[, -ncol(stacked_df)]

    # groupby "sources" and keep mean value
    Population_fraction <- Population_fraction %>% group_by(sources) %>% summarise(Pop_fraction = mean(Pop_fraction))

  } else {
    Population_fraction <- NULL
  }



  #Delete all columns before "sources"
  stacked_df <- stacked_df[, grep("sources", colnames(stacked_df)):ncol(stacked_df)]

  # Add new first column names id
  data <- cbind(id = seq_len(nrow(stacked_df)), stacked_df)

  sgeo <- inputSource(data)
  mgeo <- inputSample(data)

  return(list(sgeo = sgeo, mgeo = mgeo, Population_fraction = Population_fraction, source_list = source_list, stacked_df_OG = stacked_df_OG))
}



Select_unmixing_tracers_2_sources <- function(mgeo, sgeo, crgeo, score_threshold = NULL, nb_tracers = NULL) {


  ## Order by score and filter by score_threshold
  ordered_crgeo <- crgeo[order(-crgeo$score),]
  ordered_crgeo <- ordered_crgeo[ordered_crgeo$score > score_threshold, ]



  ## Select tracers based on score and nb_tracers
  if ((!is.null(nb_tracers)) &  (!is.null(score_threshold))) {

      ordered_crgeo <- ordered_crgeo[1:nb_tracers, ]
      print(ordered_crgeo)


  } else if ((is.null(nb_tracers)) & (is.null(score_threshold))) {

        print("Provide at least 'score_threshold'")

  ## Select tracers solely based on score
  } else if ((is.null(nb_tracers)) & (!is.null(score_threshold))) {

      ordered_crgeo <- ordered_crgeo

  } else {

    print("ERROR in [Select_unmixing_tracers_2_sources()]")
    stop()
  }

  if (nrow(ordered_crgeo) <= 1) {
    print("ERROR in [Select_unmixing_tracers_2_sources()]: Need at least 2 tracers for 2 sources")
    stop()
  }

  # nb_tracers <- nrow(ordered_crgeo)
  # score_threshold <- ordered_crgeo$score[nb_tracers]

  return(list(ctsgeo = ordered_crgeo, tracer_pair = NULL, error_threshold = NULL, score_threshold = score_threshold, nb_tracers = nb_tracers))

}




Select_data_for_unmixing <- function(sgeo, mgeo, unmixing_tracers) {


  data_extended <- bind_rows(sgeo, mgeo)
  # Add id as first column
  data_extended <- cbind(id = seq_len(nrow(data_extended)), data_extended)
  # Change colname
  colnames(data_extended)[2] <- "sources"

  cols_for_unmixing <- append(unmixing_tracers, paste("D", unmixing_tracers, sep = ""))
  cols_for_unmixing <- append(cols_for_unmixing, "n")
  cols_for_unmixing <- append(c("id", "sources"), cols_for_unmixing)
  data_sol <- data_extended[, cols_for_unmixing]

  return(data_sol)
}



Make_luminescence_rulebook <- function(populations_parameters, pop_ratios) {

  # Sets sandbox rulebook with luminescence properties as rules + strict minimum properties mandatory in the rule book (grainsize distribution, age).
  # Rules should be changed/added according to specific needs.

  if (length(populations_parameters) != length(pop_ratios)) {
    print("ERROR in [Make_luminescence_rulebook()]: Number of populations extracted from aruments 'population_parameters' and 'pop_ratios' do not match")
    stop()
  }

  nb_populations <- length(pop_ratios)

  ## getemptyrulebook
  # The following add rules for age, population, grainsize, packing and density
  # In addition, also adds the 7 parameters of Bailey2001 to the rule book
  # -> (N, E, s, A B, Th, E_th) preceded by "osl_". (1-9 for N, E, s, A B and 1-5 for Th, E_th)
  # Also adds osl_doserate and osl_R
  book_osl<-get_RuleBook(book="empty",
                      osl="Bailey2001")

  ## add another populations
  book_osl <- add_Population(book = book_osl,
                              populations = nb_populations-1)#first pop is added by default in get_RuleBook()

  ## Change age depth data
  depth_true<-list(seq(from=0.5, to=10.5, by=1))

  ## getnumberofdepthintervals
  n_depth<-length(depth_true[[1]])

  # settrueage
  age_true<-list(seq(from=0, to=10500, length.out=n_depth))

  # setgrain-sizedistribution
  # gsd_osl<-list(list(mean=rep(2.5,n_depth),
  #                     sd=rep(0.05,n_depth)),

  #                     list(mean=rep(2.5,n_depth),
  #                     sd=rep(0.05,n_depth)),

  #                     list(mean=rep(2.5,n_depth),
  #                     sd=rep(0.05,n_depth)))

  gsd_fill <- list()
  for (i in 1:nb_populations) {
      gsd_fill[[i]] <- list(mean=rep(2.5, n_depth), sd=rep(0.05,n_depth))
  }

  population_proportion_fill <- list()
  for (i in 1:nb_populations) {
      population_proportion_fill[[i]] <- list(rep(pop_ratios[i], n_depth))
  }


    ## addpopulationcontributionwithdepth
    book_osl<-set_Rule(book=book_osl,
                        parameter="population",
                        value=population_proportion_fill,
                        depth=depth_true)

    # updaterulebookwithtrueageanddepthdefinition
    book_osl<-set_Rule(book=book_osl,
                    parameter="age",
                    value=age_true,
                    depth=depth_true)



    ## updaterulebookwithdefaultluminescencemodelparameters
    # set_Rule() for all 7 model parameters + osl_R (mean = value from .set_pars(), sd = 0)
    # osl_doserate to be set seperately
    book_osl<-set_Rule(book=book_osl,
                    parameter="Bailey2001",
                    depth=depth_true)

    # set_Rule() for all 7 model parameters. Mean is based on original populations_parameters
    # and sd = 0. The same mean is given to all depth. Function [Get_param_normal_dist_list_of_values()] shoulde be
    # modified/replaced to give different values for each depth.
    book_osl <- set_Rule_for_all_model_parameters(book = book_osl,
                                                depth = depth_true,
                                                populations_parameters = populations_parameters)




    book_osl<-set_Rule(book=book_osl,
                    parameter="grainsize",
                    value=gsd_fill,
                    depth=depth_true)


  return(book_osl)

}


Compute_mean_median_proportions_differences <- function(source_list, Population_fraction, proportions, filename, tracer_pair = NULL) {

  ############################################################
  # If the expected population fraction is available,
  # this function computes the differences between the mean/median
  # and the expected population fractions
  ############################################################


  if (!is.null(Population_fraction)) {



    # Add the expected population fraction to the dataframe if available
    # proportions <- cbind(proportions, Population_fraction[Population_fraction$sources != "Mixed", "Pop_fraction"])
    # Fill last 3 rows (S1, S2, S3) with fraction
    proportions$Expected_population_fraction[(nrow(proportions)-length(source_list)+1):nrow(proportions)] <- unlist(Population_fraction[Population_fraction$sources != "Mixed", "Pop_fraction"])

    temp_proportions <- proportions[(nrow(proportions)-length(source_list)+1):nrow(proportions), ]

    # Compute the differences between the expected population fraction and the mean and median of the proportions
    temp_list_differences <- list()
    temp_list_differences_names <- list()
    for (source_name in source_list) {

      median_diff <- temp_proportions[temp_proportions$variable == source_name, "median"] - Population_fraction[Population_fraction$sources == source_name, "Pop_fraction"]
      mean_diff <- temp_proportions[temp_proportions$variable == source_name, "mean"] - Population_fraction[Population_fraction$sources == source_name, "Pop_fraction"]

      # Add median_diff and mean_diff to the list with "source_name" as name
      temp_list_differences <- append(temp_list_differences, c(median_diff, mean_diff))
      temp_list_differences_names <- append(temp_list_differences_names, c(paste0(source_name, "_median_diff"), paste0(source_name, "_mean_diff")))

    }
    # Rename list values
    names(temp_list_differences) <- temp_list_differences_names
    if (!is.null(tracer_pair)) {
      temp_list_differences <- append(c(tracer_pair = tracer_pair), temp_list_differences)
    }
    temp_list_differences <- append(c(filename = filename), temp_list_differences)

    return(list(proportions = proportions, temp_list_differences = temp_list_differences))

    } else {

      return(list(proportions = proportions, temp_list_differences = NULL))
    }

}



Plot_sources_proportions <- function(result_FP_long, filename, data_sol, Population_fraction, source_list, tracer_pair = NULL) {

  # Get number of tracers used for the unmixing
  nb_tracers <- length(colnames(data_sol)[!(colnames(data_sol) %in% c("id", "sources", "n"))])/2

  par(bg = "white")

  if (!is.null(tracer_pair)) {
    # Add tracer pair to the plot
    title <- paste0("Boxplot for ", paste0(source_list, collapse = ", ")," - ratio: ", filename, " - Tracers: ", nb_tracers, "
    Tracer pair: ", tracer_pair)
  } else {
    title <- paste0("Boxplot for ", paste0(source_list, collapse = ", ")," - ratio: ", filename, " - Tracers: ", nb_tracers)
  }

  # Create boxplot
  graphics::boxplot(value ~ variable, data = result_FP_long,
          main = title,
          xlab = "Variable", ylab = "Value", col = "lightgray", ylim = c(-0.1, 1.1))

  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

  if (!is.null(Population_fraction)) {
    # Add red points
    points_plot <- Population_fraction[Population_fraction$sources != 'Mixed', ]
    points_plot$sources_int <- as.integer(gsub("S", "", points_plot$sources))
    points(points_plot$sources_int, points_plot$Pop_fraction, col = "red", pch = 19, cex = 1.5)
  }

}


Plot_unmixing_proportions_and_get_results <- function(data_sol, source_list, Population_fraction, filename, error_threshold, score_threshold, nb_tracer, tracer_pair = NULL) {

  proportions <- data.frame()
  ratio_differences <- data.frame()

  # Let's unmix the multiple solutions
  result_FP <- unmix(data_sol, Means = T)#samples = 200, iter = 200,

  P_FP <- plotResults(result_FP, y_high = 1)#, colors = c("#CC0000",  "#33CCFF", "#9933CC"))


  # Melt the dataframe to long format for ggplot2
  result_FP_long <- reshape2::melt(result_FP, measure.vars = source_list)# c("S1", "S2")


  # ## Group result_FP_long by "variable" and compute the mean and median of "value"
  # proportions <- result_FP_long %>%
  #                 group_by(variable) %>%
  #                 summarise(mean = mean(value),
  #                           median = median(value),
  #                           sd = sd(value),
  #                           n = n()) %>%
  #                 mutate(se = sd / sqrt(n),
  #                         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
  #                         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

  ## CI for proportion https://www.statology.org/confidence-interval-proportion/
  z_value<- 1.96# for 95% confidence interval/ 1.645 for 90% confidence interval / 2.58 for 99% confidence interval
  proportions_current <- result_FP_long %>%
                  group_by(variable) %>%
                  summarise(mean = mean(value),
                            median = median(value),
                            sd = sd(value),
                            n = n(),
                            Q25 = unname(quantile(value, .25)),
                            Q75 = unname(quantile(value, .75))) %>%
                  mutate(lower.ci = mean - z_value * sqrt(mean * (1 - mean) / n),
                          upper.ci = mean + z_value * sqrt(mean * (1 - mean) / n))

  # add filename as first column of proportions
  if (!is.null(tracer_pair)) {
    proportions_current <- cbind(tracer_pair = tracer_pair, proportions_current)
  }
  proportions_current <- cbind(filename = filename, proportions_current)

  if (is.null(error_threshold)) {
    proportions_current <- cbind(proportions_current,
                              error_threshold = rep(NaN, length(source_list)),
                              score_threshold = rep(score_threshold, length(source_list)),
                              nb_tracer = rep(nb_tracer, length(source_list)))

    } else if (is.null(nb_tracer)) {
      proportions_current <- cbind(proportions_current,
                              error_threshold = rep(error_threshold, length(source_list)),
                              score_threshold = rep(score_threshold, length(source_list)),
                              nb_tracer = rep(NaN, length(source_list)))

    } else {
      print("ERROR in [Plot_unmixing_proportions_and_get_results()]")
      stop()
    }

  # add new empty column

  # proportions_current$Expected_population_fraction <- rep(NaN, length(source_list))
  proportions_current[-length(source_list):-1, "Expected_population_fraction"] <- rep(NaN, length(source_list))
  # proportions <- rbind(proportions, proportions_current)

  #-----------------------------
  ## Comparison of proportions between computed and expected
  output <- Compute_mean_median_proportions_differences(source_list, Population_fraction, proportions_current, filename, tracer_pair)
  proportions <- output$proportions
  ratio_differences <- rbind(ratio_differences, data.frame(output$temp_list_differences))

  #-----------------------------
  Plot_sources_proportions(result_FP_long, filename, data_sol, Population_fraction, source_list, tracer_pair)

  return(list(unmixing_results = result_FP, unmixing_results_long = result_FP_long ,proportions = proportions, ratio_differences = ratio_differences))

}


Get_metrics_for_proprotions_selection <- function(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer) {



    ## Add tracer_pair, tracers, Manhattan_dist_median, Manhattan_dist_mean, filename, CI width to metrics
    # Rename col "id" to "filename"
    colnames(metrics)[colnames(metrics) == "id"] <- "filename"
    metrics$filename <- filename

    if (!is.null(tracer_pair)) {
      metrics <- cbind(metrics, tracer_pair = tracer_pair)
    } else {
      metrics <- cbind(metrics, tracer_pair = NaN)
    }
    # metrics <- cbind(metrics, tracer_pair = tracer_pair)

    metrics <- cbind(metrics, tracers = paste0(ctsgeo$tracer, collapse = "-"))


    if (is.null(error_threshold)) {
        error_threshold <- NaN
    }
    if (is.null(score_threshold)) {
        score_threshold <- NaN
    }
    if (is.null(nb_tracer)) {
        nb_tracer <- NaN
    }


    metrics <- cbind(metrics, error_threshold = error_threshold)
    metrics <- cbind(metrics, score_threshold = score_threshold)
    metrics <- cbind(metrics, nb_tracer = nb_tracer)


    ## Add width of confidence interval
    for (source in source_list) {
        metrics[, paste0(source,"_CI_width")] <- tail(proportions[proportions$variable == source, "upper.ci"],1)[[1]] - tail(proportions[proportions$variable == source, "lower.ci"],1)[[1]]
    }
    # Add sum of witdth of confidence interval
    metrics <- cbind(metrics, CI_width_sum = rowSums(metrics[, grep("_CI_width", names(metrics))]))


    if (nrow(ratio_differences) > 0){

        ## Compute Manhattan distance
        # Get columns that start with "S" and contains _median_
        cols <- grep("^S.*_median_", names(ratio_differences), value = TRUE)
        # Get sum of absolute values in ratio_differences[, cols]
        Manhattan_dist_median <- rowSums(abs(ratio_differences[, cols]))

        # Get columns that start with "S" and contains _mean_
        cols <- grep("^S.*_mean_", names(ratio_differences), value = TRUE)
        # Get sum of absolute values in ratio_differences[, cols]
        Manhattan_dist_mean <- rowSums(abs(ratio_differences[, cols]))
        metrics <- cbind(metrics, Manhattan_dist_median, Manhattan_dist_mean)
    }

    return(metrics)
}



Complete_unmixing_routine <- function(mgeo, sgeo, crgeo, source_list, Population_fraction, filename, error_threshold = NULL, score_threshold = NULL, nb_tracer = NULL, tracer_pair = NULL) {

  if (is.null(tracer_pair)) {

    output <- Select_unmixing_tracers_2_sources(sgeo = sgeo,
                                                mgeo = mgeo,
                                                crgeo = crgeo,
                                                score_threshold = score_threshold,
                                                nb_tracers = nb_tracer)
    ctsgeo <- output$ctsgeo
    tracer_pair <- output$tracer_pair
    error_threshold <- output$error_threshold
    score_threshold <- output$score_threshold
    nb_tracer <- output$nb_tracer

  } else if (!is.null(tracer_pair)) {


    output <- Perform_CTS_3_sources(tracer_pair = tracer_pair,
                              mgeo = mgeo,
                              sgeo = sgeo,
                              crgeo = crgeo,
                              error_threshold = error_threshold,
                              score_threshold = score_threshold,
                              nb_tracer = nb_tracer)

      ctsgeo <- output$ctsgeo
      tracer_pair <- output$tracer_pair
      error_threshold <- output$error_threshold
      score_threshold <- output$score_threshold
      nb_tracer <- output$nb_tracer
  } else { # function [Perform_CTS_4_sources()] should be easy to create and add based on [Perform_CTS_3_sources()]
      print("ERROR in [Complete_unmixing_routine()]")
      stop()
  }




    if (nrow(ctsgeo) == 0) {

      print("No tracers in function [Complete_unmixing_routine()]")

      return(list(metrics = NULL, proportions = NULL, ratio_differences = NULL))

    } else if ((nrow(ctsgeo) >= 1) & (is.null(tracer_pair))) {

        data_sol <- Select_data_for_unmixing(sgeo = sgeo,
                                        mgeo = mgeo,
                                        unmixing_tracers = ctsgeo$tracer)

        output <- Plot_unmixing_proportions_and_get_results(data_sol,
                                                            source_list,
                                                            Population_fraction,
                                                            filename,
                                                            error_threshold,
                                                            score_threshold,
                                                            nb_tracer,
                                                            tracer_pair)
        proportions <- output$proportions
        ratio_differences <- output$ratio_differences
        metrics <- do.call(data.frame, aggregate(. ~ id, data = output$unmixing_results, function(x) c(mean = mean(x), SD = sd(x))))
        metrics <- Get_metrics_for_proprotions_selection(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer)

    } else if ((nrow(ctsgeo) > 1) & (!is.null(tracer_pair))) {


        data_sol <- Select_data_for_unmixing(sgeo = sgeo,
                                        mgeo = mgeo,
                                        unmixing_tracers = ctsgeo$tracer)

        output <- Plot_unmixing_proportions_and_get_results(data_sol,
                                                            source_list,
                                                            Population_fraction,
                                                            filename,
                                                            error_threshold,
                                                            score_threshold,
                                                            nb_tracer,
                                                            tracer_pair)
        proportions <- output$proportions
        ratio_differences <- output$ratio_differences
        metrics <- do.call(data.frame, aggregate(. ~ id, data = output$unmixing_results, function(x) c(mean = mean(x), SD = sd(x))))
        metrics <- Get_metrics_for_proprotions_selection(metrics, filename, tracer_pair, ctsgeo, source_list, ratio_differences, proportions, error_threshold, score_threshold, nb_tracer)


    } else {

        print(ctsgeo)
        print("ERROR in [Complete_unmixing_routine()]")
        stop()
    }


    return(list(metrics = metrics, proportions = proportions, ratio_differences = ratio_differences, ctsgeo = ctsgeo))
}





#-------------------- OSL_TL_functions.r

Get_sequence <- function(file_path, lab_dose_rate) {

    ###############################################################################################
    # Reads sequence file from .seq or .RData format. .RData fromat is created when using function
    # [Create_sequence_manually()] to create a sequence manually (ex from .lseq files).

    # dose_rate corresponds to the lab dose rate

    # Function [read_SEQ2R()] returns a list of lists. Each list corresponds to a sequence step.
    # Order of the values in each sequence step is described at:
    # https://r-lum.github.io/RLumModel/reference/model_LuminescenceSignals.html
    ###############################################################################################


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

OSL_metrics_hash_to_dataframe <- function(OSL_hash, record_num) {

  # Only for a single record
  OSL_hash <- OSL_hash[[record_num]]

    df <- data.frame(row.names = record_num)
    Nb_components <- OSL_hash$Nb_components

    df[['Nb_components']] <- Nb_components

    for (key in setdiff(names(OSL_hash), c("Nb_components"))) {
        for (component_num in 1:Nb_components) {
            df[[paste0(key, "_component_", component_num)]] <- OSL_hash[[key]][[paste0("Component_",component_num)]]
        }

    }

    return(df)
}

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

get_user_sequence_step_input <- function(i) {
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
  tkgrid(tklabel(tt, text = glue("Enter sequence step {i}:\nPH, IRR, OSL or TL")), entry)
  tkgrid(submit_button)

  # Wait for the user to submit the input
  tkwait.window(tt)

  # Return the user input
  return(as.character(tclvalue(user_input)))
}

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
  tkgrid(tklabel(tt, text = "Enter pre-heat heating rate (ºC)/s or 'NaN':"), heating_rate_entry)
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
  tkgrid(tklabel(tt, text = "Enter TL heating rate (ºC)/s:"), heating_rate_entry)
  tkgrid(submit_button)

  # Wait for the user to submit the inputs
  tkwait.window(tt)

  # Return the user inputs as a list
  # return(list(begin_temp = as.numeric(tclvalue(temp)), end_temp = as.numeric(tclvalue(duration)), heating_rate = as.numeric(tclvalue(heating_rate))))
  return(c(as.numeric(tclvalue(temp)), as.numeric(tclvalue(duration)), as.numeric(tclvalue(heating_rate))))

}

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

Get_specific_columns <- function(fit, col_prefix, components) {

    ###
    #Selects columns that start with a specific prefix and contain a specific component number
    ###

    #Find column names that start with "cs"
    prefix_columns <- grep(paste0("^", col_prefix), colnames(fit$data), value = TRUE)

    #Subset the DataFrame to include only those columns
    prefix_df <- fit$data[, prefix_columns]


    #Keep only cs/cs.rel columns that contain the component number
    columns_to_keep <- prefix_columns[sapply(prefix_columns, function(x) any(sapply(components, function(y) grepl(y, x))))]

    #Subset the DataFrame to include only those columns
    prefix_df <- prefix_df[, columns_to_keep]

  return(prefix_df)
}

Get_OSL_metrics <- function(model.output, IRR_dose, make_plot = TRUE) {

  if (make_plot) {

    ## Plot the OSL curve
    plot_RLum(object = model.output, log = "y")
  }


  fit <- fit_CWCurve(as.data.frame(get_RLum(model.output)), output.terminal = FALSE)

  if (length(fit$data) == 1 && is.na(fit$data)) {
    print("No fit found. Exiting.")
    return(list(nb_components = NaN, cs_df = NaN, I0_df = NaN, sensitivity = NaN))
  }

  else {
    #Get list of components
    nb_components <- fit$data$n.components
    components <- as.character(seq(1, nb_components))
    print(glue("{nb_components} component(s) found in the OSL curve."))

    ## Get photoionisation cross section
    cs_df <- Get_specific_columns(fit, "cs", components)

    ## Get intensity of each component
    I0_df <- Get_specific_columns(fit, "I0", components)

    return(list(nb_components = nb_components, cs_df = cs_df, I0_df = I0_df, sensitivity = I0_df/IRR_dose))
    }

}

Generate_synthetic_OSL_TL_signals <- function(sequence, model = "Bailey2001") {


  data <- model_LuminescenceSignals(
          sequence = sequence,
          model = model,
          plot = FALSE,
          verbose = FALSE
              )
  return(data)

}

Compute_OSL_metrics <- function(sequence, data) {


  # Example for argument data:

  #  [RLum.Analysis-class]
  # 	 originator: model_LuminescenceSignals()
  # 	 protocol: Bailey2001
  # 	 additional info elements:  0
  # 	 number of records: 2
  # 	 .. : RLum.Data.Curve : 2
  # 	 .. .. : #1 OSL | #2 OSL





    SEQ_sequence_renamed <- Rename_sequence(sequence)
    nb_OSL_records <- length(data)
    print(glue("{nb_OSL_records} OSL records"))

    OSL_hash <- hash()

    for (record_num in 1:nb_OSL_records) {



      ## TODO: AUTOMATE IRR_DOSE
      # IRR_dose <- SEQ_sequence_renamed[[paste0("IRR_", 1)]][2] - not always the correct one
      output <- Get_OSL_metrics(data[[record_num]], IRR_dose = 50)

      if (is.na(output$nb_components)) {
            OSL_hash[[paste0("Record_",record_num)]][["Nb_components"]] <- NaN
        } else {

          OSL_hash[[paste0("Record_",record_num)]][["Nb_components"]] <- output$nb_components

      for (component_num in 1:output$nb_components) {

          OSL_hash[[paste0("Record_", record_num)]][["Cross_section"]][[paste0("Component_", component_num)]] <- output$cs_df[,paste0("cs", component_num)]
          OSL_hash[[paste0("Record_", record_num)]][["Cross_section_relative"]][[paste0("Component_", component_num)]] <- output$cs_df[, paste0("cs", component_num, ".rel")]
          OSL_hash[[paste0("Record_", record_num)]][["Intensity"]][[paste0("Component_", component_num)]] <- output$I0_df[, paste0("I0", component_num)]
          OSL_hash[[paste0("Record_", record_num)]][["Sensitivity"]][[paste0("Component_", component_num)]] <- output$sensitivity[, paste0("I0", component_num)]

        }

      }
    }

    return(OSL_hash)

}


TL_metrics_hash_to_dataframe <- function(TL_hash, record_num) {

  # Uses only a single record (i.e at the most 1 TLin sequence)

  TL_hash <- TL_hash[[record_num]]

  if (TL_hash$Nb_peaks == 0) {
      return()
  }

  df <- data.frame(row.names = record_num)

  if (TL_hash$Forced_additional_peak) {
      Nb_peaks <- TL_hash$Nb_peaks - 1
  } else {
      Nb_peaks <- TL_hash$Nb_peaks
  }
  df[["Nb_peaks"]] <- Nb_peaks

  for (key in setdiff(names(TL_hash), c("Nb_peaks", "Forced_additional_peak"))) {
      for (peak_num in 1:Nb_peaks) {
          df[[paste0(key, "_peak_", peak_num)]] <- TL_hash[[key]][[paste0("Peak_", peak_num)]]
      }

  }

  return(df)
}

Single_record_TL_OSL_metrics_df <- function(TL_hash, OSL_hash, record_num) {


  if ((record_num %in% keys(TL_hash)) &
      (record_num %in% keys(OSL_hash))) {

      single_record_TL_metrics <- TL_metrics_hash_to_dataframe(TL_hash, record_num)
      single_record_OSL_metrics <- OSL_metrics_hash_to_dataframe(OSL_hash, record_num)

      if (!is.null(single_record_TL_metrics)) {
        output <- cbind(single_record_TL_metrics, single_record_OSL_metrics)

      } else {
        output <- single_record_OSL_metrics
      }

  } else if ((record_num %in% keys(TL_hash)) &
              !(record_num %in% keys(OSL_hash))) {

      single_record_TL_metrics <- TL_metrics_hash_to_dataframe(TL_hash, record_num)

      if (!is.null(single_record_TL_metrics)) {
        output <- single_record_TL_metrics

      } else {
        return()
      }

      output <- single_record_TL_metrics


  } else if (!(record_num %in% keys(TL_hash)) &
              (record_num %in% keys(OSL_hash))) {

      single_record_OSL_metrics <- OSL_metrics_hash_to_dataframe(OSL_hash, record_num)
      output <- single_record_OSL_metrics

  } else {
    print("ERROR in [Single_record_TL_OSL_metrics_df()] - extra conditions should be added")
  }



  # Add "Record_x" to every column name
  colnames(output) <- paste0(record_num, "_", colnames(output))

  return(output)
}

Extract_OSL_TL_metrics_for_all_records <- function(TL_hash, OSL_hash, source_number, records_to_extract_data_from = NULL) {

    if (is.null(records_to_extract_data_from)) {
        records_to_extract_data_from <- keys(TL_hash)
    }

    metrics <- data.frame(matrix(NA, nrow = 1, ncol = 0))
    for (record_num in records_to_extract_data_from) {
        metrics_temp <- Single_record_TL_OSL_metrics_df(TL_hash = TL_hash, OSL_hash = OSL_hash, record_num = record_num)

        if (!is.null(metrics_temp)){
          metrics <- cbind(metrics, metrics_temp)
        }
    }


    metrics <- cbind("sources" = rep(source_number, nrow(metrics)), metrics)

    return(metrics)
}


get_peak_user_inputs <- function(record_num) {

  ##################################################
  # forced_additional_peak = TRUE means that at the end of the curve, it seems like there is the beginning of an incomplete Peak.
  # Needs to be selected (and forced_additional_peak = TRUE) in order to not disrupt the fitting of the previous curves. No metrics
  # are extracted from this incomplete peak
  ##################################################

  {
    nb_peaks = readline(glue("Enter total number of peaks for record {record_num}: "))
    nb_peaks <- as.numeric(nb_peaks)

    if (nb_peaks == 0) {
      forced_additional_peak <- FALSE
    } else {
    forced_additional_peak <- readline(glue("Forced additional last (incomplete) peaks for record {record_num} (TRUE or FALSE)? "))
    forced_additional_peak <- as.logical(forced_additional_peak)
    }
  }

  return(list(nb_peaks = nb_peaks, forced_additional_peak = forced_additional_peak))
}

inflect <- function(x, threshold = 1) {

  #https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima

    up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
    down <-  sapply(-1:-threshold, function(n) c(rep(NA, abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
    a    <- cbind(x, up, down)
    list(minima = which(apply(a, 1, min) == a[, 1]), maxima = which(apply(a, 1, max) == a[, 1]))
}

Get_peaks <- function(df, span) {

  # Automatic peak detection
  #Span is the number of points around the peak to consider
  #https://www.rdocumentation.org/packages/ggpmisc/versions/0.6.0/topics/find_peaks

  peak_loc_logical <- find_peaks(df[, "Intensity"], span = span)
  peaks_temp <- df[peak_loc_logical, "Temperature"]
  peaks_intensity <- df[peak_loc_logical, "Intensity"]
  nb_peaks <- length(peaks_temp)
  print(glue("Peaks temp :{peaks_temp} "))

  return(list(peaks_temp = peaks_temp, peaks_intensity = peaks_intensity, nb_peaks = nb_peaks))

}

Add_last_datapoint_for_potential_peak <- function(last_datapoint, last_local_max, last_local_min, peaks) {

  # Defines whether or not force_additional_peak is TRUE or FALSE

  peaks_temp <- peaks$peaks_temp
  peaks_intensity <- peaks$peaks_intensity
  forced_additional_peak <- peaks$forced_additional_peak

  if (length(peaks_temp) > 0) {

    if ((last_local_min$Temperature < last_datapoint$Temperature) &&
      (last_local_max$Temperature < last_local_min$Temperature) &&
      (last_datapoint$Intensity > last_local_min$Intensity)) {

          peaks_temp <- c(peaks_temp, last_datapoint$Temperature)
          peaks_intensity <- c(peaks_intensity, last_datapoint$Intensity)
          forced_additional_peak <- TRUE
          print(glue("Forced additional peak located at ({last_datapoint$Temperature}, {last_datapoint$Intensity}). Total peaks : {length(peaks_temp)}"))

      }
      else {

        print("No additional peak added")
      }
}


  return(list(peaks_temp = peaks_temp, peaks_intensity = peaks_intensity, nb_peaks = length(peaks_temp), forced_additional_peak = forced_additional_peak))
}

Get_initial_peaks <- function(df, span) {


  local_min_max <- inflect(df[, "Intensity"], threshold = span)
  peaks <- hash()
  peaks$peaks_temp <- df[local_min_max$maxima, "Temperature"]
  peaks$peaks_intensity <- df[local_min_max$maxima, "Intensity"]
  peaks$nb_peaks <- length(peaks$peaks_temp)
  peaks$forced_additional_peak <- FALSE


  output <- Add_last_datapoint_for_potential_peak(last_datapoint = tail(df,v1),
                                                        last_local_max = tail(df[local_min_max$maxima,], 1),
                                                        last_local_min = tail(df[local_min_max$minima,], 1),
                                                        peaks)

  return(list(peaks_temp = output$peaks_temp, peaks_intensity = output$peaks_intensity, nb_peaks = output$nb_peaks, forced_additional_peak = output$forced_additional_peak))

}

Get_initial_peaks_valleys <- function(df, span) {

  local_min_max <- inflect(df[, "Intensity"], threshold = span)


  maxima <- df[local_min_max$maxima,]
  minima <- df[local_min_max$minima,]

  if (length(local_min_max$maxima) == 0 & length(local_min_max$minima) > 0) {

    local_extrema <- rbind(
    data.frame(x = df[local_min_max$minima,"Temperature"], y = df[local_min_max$minima,"Intensity"], source = "Min")
    )
  } else if (length(local_min_max$minima) == 0 & length(local_min_max$maxima) > 0) {

    local_extrema <- rbind(
    data.frame(x = df[local_min_max$maxima,"Temperature"], y = df[local_min_max$maxima,"Intensity"], source = "Max")
    )
  } else if (length(local_min_max$maxima) > 0 & length(local_min_max$minima) > 0) {

    # Combine the dataframes
    local_extrema <- rbind(
    data.frame(x = df[local_min_max$maxima,"Temperature"], y = df[local_min_max$maxima,"Intensity"], source = "Max"),
    data.frame(x = df[local_min_max$minima,"Temperature"], y = df[local_min_max$minima,"Intensity"], source = "Min")
    )
  }

  else {
    print("No peaks or valleys found. Exiting.")
    return()
  }

  colnames(local_extrema) <- c("Temperature", "Intensity", "source")

  local_extrema <- local_extrema[order(local_extrema$Temperature),]


  return(local_extrema)

}

Consecutive_min_max_intensity_threshold_check <- function(local_extrema, peaks, min_max_intensity_threshold = 30) {

  local_extrema <- rbind(data.frame(Temperature = 0, Intensity = 0, source = "First_point"), local_extrema)
  local_extrema <- local_extrema[order(local_extrema$Temperature), ]
  # Consecutive min/max should have a difference of at least 20
  desired_extrema <- abs(diff(local_extrema$Intensity)) > min_max_intensity_threshold
  desired_extrema <- data.frame(Position = seq_along(desired_extrema) + 1, Logical = desired_extrema)
  desired_extrema <- local_extrema[desired_extrema[desired_extrema$Logical,]$Position, ]
  desired_extrema <- desired_extrema[desired_extrema$source == "Max",]
  local_max <- desired_extrema[, c("Temperature", "Intensity")]

  peaks$peaks_temp = local_max$Temperature
  peaks$peaks_intensity = local_max$Intensity
  peaks$nb_peaks = length(peaks$peaks_temp)
  peaks$forced_additional_peak <- FALSE

  return(peaks)

}


Average_neighbouring_points_filtering <- function(peaks, df, smoothing_data, neighbour_temp_window_size) {

  if (length(peaks) == 0) {
    print("No peaks to filter. Exiting Average_neighbouring_points_filtering().")
    return(peaks)
  }

  OG_length <- length(peaks$peaks_temp)

  data <- smoothing_data

  peaks_temp <- peaks$peaks_temp
  peaks_intensity <- peaks$peaks_intensity

  # Create empty df to store the results
  peaks_neighbour_intensity_avg <- data.frame(Temperature = numeric(),
                                                Neighbours_avg_intensity = numeric())

  for (i in seq_along(peaks_temp)){

    peak_temp <- peaks_temp[i]
    peak_intensity <- peaks_intensity[i]

    window_data <- data[data$Temperature <= peak_temp + neighbour_temp_window_size/2 &
                      data$Temperature >= peak_temp - neighbour_temp_window_size/2,]

    peaks_neighbour_intensity_avg <- rbind(peaks_neighbour_intensity_avg,
                                      data.frame(Temperature = peak_temp,
                                      Intensity = peak_intensity,
                                      Intensity_avg = mean(window_data$Intensity)))
  }

  # Only keep lines of peaks_neighbour_intensity_avg that have Intensity_avg < Intensity
  peaks_neighbour_intensity_avg <- peaks_neighbour_intensity_avg[peaks_neighbour_intensity_avg$Intensity_avg < peaks_neighbour_intensity_avg$Intensity,]
  peaks$peaks_temp <- peaks_neighbour_intensity_avg$Temperature
  peaks$peaks_intensity <- peaks_neighbour_intensity_avg$Intensity

  print(glue("Average_neighbouring_points_filtering() deleted {OG_length - length(peaks$peaks_temp)} peaks."))

  return(peaks)


}

Compute_intensity_std_moving_window <- function(local_extrema, df, std_computation_window_size_degrees) {

  local_extrema <- local_extrema[local_extrema$source == "Max",]
  local_extrema <- local_extrema[,c("Temperature", "Intensity")]
  df$Intensity_norm <- (df$Intensity - min(df$Intensity)) / (max(df$Intensity) - min(df$Intensity))

  # Create empty df to store the results
  std_results <- data.frame(Temperature = numeric(), Intensity_std = numeric(), Intensity_std_norm = numeric())

  for (extrema_temp in local_extrema$Temperature){

    window_data <- df[df$Temperature <= extrema_temp + std_computation_window_size_degrees/2 &
                      df$Temperature >= extrema_temp - std_computation_window_size_degrees/2,]

    std_results <- rbind(std_results, data.frame(Temperature = extrema_temp, Intensity_std = std(window_data$Intensity),
                          Intensity_std_norm = std(window_data$Intensity_norm)))
  }

  return(std_results)

}

Filter_peaks_using_std <- function(local_extrema, peaks, df, std_computation_window_size_degrees, std_threshold) {

    if (length(peaks) == 0) {
    moving_window_local_extrema <- local_extrema
  }

  else {
    moving_window_local_extrema <- data.frame(Temperature = peaks$peaks_temp,
                                              Intensity = peaks$peaks_intensity,
                                              source = rep("Max", length(peaks$peaks_temp)))
  }

  # Std moving window for each local peak
  std_results <- Compute_intensity_std_moving_window(moving_window_local_extrema, df, std_computation_window_size_degrees)

  if (length(peaks) == 0) {

    peaks$peaks_temp <- std_results$Temperature
    peaks$peaks_intensity <- local_extrema[local_extrema$Temperature %in% std_results$Temperature, "Intensity"]
    peaks$nb_peaks <- length(peaks$peaks_temp)
    peaks$forced_additional_peak <- FALSE

  }

  temp_of_std_above_threshold <- std_results[std_results$Intensity_std_norm >= std_threshold, "Temperature"]
  position_num_to_keep <- which(std_results$Temperature %in% temp_of_std_above_threshold)
  peaks$peaks_temp <- peaks$peaks_temp[position_num_to_keep]
  peaks$peaks_intensity <- peaks$peaks_intensity[position_num_to_keep]

  return(list(peaks = peaks, std_results = std_results))

}

Keep_relevant_peaks <- function(local_extrema, df, smoothing_data,
                                min_max_intensity_threshold = 20,
                                std_computation_window_size_degrees = 75,
                                std_threshold = 0) {

  peaks <- hash()
  results <- hash()

  # Consecutive min/max should have a difference of at least 20
  results$peaks <- Consecutive_min_max_intensity_threshold_check(local_extrema,
                                                        peaks,
                                                        min_max_intensity_threshold = min_max_intensity_threshold)


  results$peaks <- Average_neighbouring_points_filtering(results$peaks, df, smoothing_data,
                                                          neighbour_temp_window_size = 20)

  # results <- Filter_peaks_using_std(local_extrema, results$peaks, df, std_computation_window_size_degrees, std_threshold)


  return(results)

}

Plot_smoothing <- function(df, smoothing_results, record_num) {
  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

    # Plot with y-axis log scale
  plot(df$Temperature - TEMPERATURE_CONVERSION_CONSTANT, df$Intensity,
      xlab = "Temperature (°C)",
      ylab = "Intensity",
      col = "black",
      main = glue("Record {record_num}"),
      log = "y")

  # Add the smoothed line
  lines(smoothing_results$Temperature - TEMPERATURE_CONVERSION_CONSTANT, smoothing_results$Intensity, col = "red")

  # Add a legend
  legend("topright", legend = c("Original", "Smoothed"), col = c("black", "red"), lty = 1)
}

TL_curve_smoothing <- function(df, record_num, lambda) {

  smoothing_results <- df

  if (nrow(smoothing_results) == 0) {
    print("No data points to smooth. Exiting.")
    return(df)
  }

  smoothing_results$Intensity <- whittaker(y = smoothing_results$Intensity, lambda = lambda, d = 2)

  data <- data.frame(x = smoothing_results$Temperature, y = smoothing_results$Intensity)
  colnames(data) <- c("Temperature", "Intensity")

  # Plot_smoothing(df, smoothing_results = data, record_num = record_num)

  return(data)
}

Plot_TL_Curve <- function(model_output, peaks, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  plot_RLum(object = model_output,
            main = paste("Record", record_num, " - TL curve with", peaks$nb_peaks,
            "peaks - FORCED_PEAK = ", peaks$forced_additional_peak), log = "y")

  if (peaks$nb_peaks != 0) {
  points(peaks$peaks_temp - TEMPERATURE_CONVERSION_CONSTANT, peaks$peaks_intensity, col = "red", pch = 19)
  # legend("topright", legend = "Estimated peak locations", col = "red", pch = 19)#"bottomright",
  }

}

Plot_TL_Curve_with_std  <- function(model_output, temp_std_results, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  temp_std_results$Temperature <- temp_std_results$Temperature - TEMPERATURE_CONVERSION_CONSTANT # Convert to Celsius

  # plot_RLum(object = model_output, log = "y", main = paste("Record", record_num, " - Thermoluminescence (TL) curve with", peaks$nb_peaks, "peaks"))

  if (length(temp_std_results$Temperature) != 0) {

    temperature <- temp_std_results$Temperature
    intensity_std <- temp_std_results$Intensity_std
    intensity_std_norm <- temp_std_results$Intensity_std_norm

    plot(temperature, intensity_std_norm, pch=16, xlab="Temperature (°C)",
        ylab="Normalised intensity std", type="b", col="black",
        main=paste("Record",record_num), log = "y")

  } else {
     print("No std to print")
  }

}


Plot_TL_Curve_with_extrema <- function(model_output, local_extrema, record_num) {

  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  peaks_temp <- local_extrema[local_extrema$source == "Max", "Temperature"]
  peaks_intensity <- local_extrema[local_extrema$source == "Max", "Intensity"]
  valleys_temp <- local_extrema[local_extrema$source == "Min", "Temperature"]
  valleys_intensity <- local_extrema[local_extrema$source == "Min", "Intensity"]

  nb_peaks <- length(peaks_temp)
  nb_valleys <- length(valleys_temp)

  plot_RLum(object = model_output, log = "y", main = paste("Record ", record_num, "- Thermoluminescence (TL) curve with", nb_peaks, "peaks and", nb_valleys, "valleys"))
  if (nb_peaks != 0 | nb_valleys != 0) {
  points(peaks_temp - TEMPERATURE_CONVERSION_CONSTANT, peaks_intensity, col = "red", pch = 19)
  points(valleys_temp - TEMPERATURE_CONVERSION_CONSTANT, valleys_intensity, col = "blue", pch = 19)
  legend("bottomright", legend = c("Estimated peak locations", "Estimated valley locations"), col = c("red", "blue"), pch = 19)
  }

}

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

DIY_TL_peak_intensities_integration <- function(TL_tgcd_results) {

  if (length(grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)) == 1) {

    # Col have names Peak.1, Peak.2, etc.
    TL_tgcd_peak_intensity_cols_only <- as.data.frame(TL_tgcd_results$comp.sig[, grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)])
    colnames(TL_tgcd_peak_intensity_cols_only) <- "Peak.1"
  } else {

    # Col have names Peak.1, Peak.2, etc.
    TL_tgcd_peak_intensity_cols_only <- TL_tgcd_results$comp.sig[, grep("^Peak", colnames(TL_tgcd_results$comp.sig), value = TRUE)]
  }


    # Single values
    temperature_resolution <- mean(diff(TL_tgcd_results$comp.sig[, "Temperature"]))
    Peak_intensities <- colSums(TL_tgcd_peak_intensity_cols_only*temperature_resolution)
    # Peak.1, Peak.2, etc. are now row names with col "Peak_intensities"
    Peak_intensities <- as.data.frame(Peak_intensities)
    # Compute the relative peak intensities (relative to peak 1)
    peak_1_value <- Peak_intensities["Peak.1", "Peak_intensities"]
    Peak_intensities$Peak_intensities_relative <- Peak_intensities$Peak_intensities / peak_1_value

    return(Peak_intensities)
}

Get_TL_metrics <- function(record_num, model.output, Heat_rate, span, automatic_peak_finder,
                            INITIAL_ACTIVATION_ENERGY = 1.4, INITIAL_KINETIC_ORDER = 1.5) {


  TEMPERATURE_CONVERSION_CONSTANT <- 273.15

  std_results <- hash()

  # Assuming the DataFrame is obtained from get_RLum
  df <- setNames(as.data.frame(get_RLum(get_RLum(model.output))), c("Temperature", "Intensity"))

  # Convert the temperature to Kelvin for the TGCD function
  df[, "Temperature"] <- df[, "Temperature"] + TEMPERATURE_CONVERSION_CONSTANT


  if (automatic_peak_finder) {

    if (unique(structure_RLum(model.output)['originator'] == ".Risoe.BINfileData2RLum.Data.Curve")) {

      print("Data from experimental signal")
      data <- TL_curve_smoothing(df, record_num, lambda = 50)
      local_extrema <- Get_initial_peaks_valleys(df = data, span = span)

      temp_results <- Keep_relevant_peaks(local_extrema = local_extrema, df = df, smoothing_data = data)

      if (!is.null(temp_results$std_results)) {
        std_results <- temp_results$std_results
        Plot_TL_Curve_with_std(model.output, std_results, record_num)
      }
      peaks <- Add_last_datapoint_for_potential_peak(last_datapoint = tail(df[df$Intensity != 0, ],1),
                                                    last_local_max = tail(local_extrema[local_extrema$source == "Max", ], 1),
                                                    last_local_min = tail(local_extrema[local_extrema$source == "Min", ], 1),
                                                    temp_results$peaks)

      # Plot_TL_Curve_with_extrema(model.output, local_extrema, record_num)

    }

    else if (unique(structure_RLum(model.output)['originator']) == ".local") {

      print("Data from synthetic signal")
      # Automatic peak detection
      peaks <- Get_initial_peaks(df, span)

    }

    else {
      print("Unknown originator. Exiting.")
      return()
    }




    # Make plot of TL with estimates of peak location
    Plot_TL_Curve(model.output, peaks, record_num)

    if (peaks$nb_peaks == 0) {
      print("No peaks detected. Exiting.")
        return()
      }



    # Rough estimates for the parameters then deconvolution
    knPars <-
      cbind(peaks$peaks_intensity, # Im
            rep(INITIAL_ACTIVATION_ENERGY, peaks$nb_peaks), # E
            peaks$peaks_temp,  # Tm
            rep(INITIAL_KINETIC_ORDER, peaks$nb_peaks))  # b ranges between 1-2

    TL_tgcd_results <- tgcd(df, npeak=length(peaks$peaks_intensity), model="g1",
                inisPAR=knPars, edit.inis=FALSE, hr=Heat_rate, plot = FALSE)

    #Plot peaks individually
    # Plot_TL_Peaks(TL_tgcd_results$comp.sig, peaks$nb_peaks)




    Peak_intensities <- DIY_TL_peak_intensities_integration(TL_tgcd_results)


    # ## Print desired results
    # print("Max peak intensities:")
    # print(TL_tgcd_results$pars)
    # print("Peak frequency factors:")
    # print(TL_tgcd_results$ff)
    # print("Integrated peak intensities:")
    # print(Peak_intensities)

    return(list(forced_additional_peak = peaks$forced_additional_peak, nb_peaks = peaks$nb_peaks, peaks_temp = peaks$peaks_temp, peak_intensity = peaks$peaks_intensity, TL_tgcd_results = TL_tgcd_results, Peak_intensities = Peak_intensities))
  } else {

    windows()
    plot_RLum(object = model.output,
                  main = paste("Record", record_num, " - TL curve with"), log = "y")

    peaks <- get_peak_user_inputs(record_num)

    # print(peaks$nb_peaks)
    if (peaks$nb_peaks == 0) {
      print("No peaks detected. Exiting.")
        return()
      }

    TL_tgcd_results <- tgcd(df, npeak=peaks$nb_peaks, model="g1",
                              hr=Heat_rate, edit.inis = FALSE, inisPAR = NULL,
                              pickp = "d01")#test this

    #Plot peaks individually
    # Plot_TL_Peaks(TL_tgcd_results$comp.sig, peaks$nb_peaks)

    Peak_intensities <- DIY_TL_peak_intensities_integration(TL_tgcd_results)

    return(list(forced_additional_peak = peaks$forced_additional_peak, nb_peaks = peaks$nb_peaks, peaks_temp = peaks$peaks_temp, peak_intensity = peaks$peaks_intensity, TL_tgcd_results = TL_tgcd_results, Peak_intensities = Peak_intensities))

    }
}


Rename_sequence <- function(list_of_lists) {

  # Get the names of the sublists
  sublists_names <- names(list_of_lists)

  # Create a new list with unique names
  unique_list_of_lists <- list()
  name_count <- list()

  for (i in seq_along(list_of_lists)) {
    name <- sublists_names[i]
    if (is.null(name_count[[name]])) {
      name_count[[name]] <- 1
      name <- paste0(name, "_1")
    } else {
      name_count[[name]] <- name_count[[name]] + 1
      name <- paste0(name, "_", name_count[[name]])
    }
    unique_list_of_lists[[name]] <- list_of_lists[[i]]
  }

  return(unique_list_of_lists)
}


Compute_TL_metrics <- function(sequence, data, automatic_peak_finder) {


  # Example of argument data
  #  [RLum.Analysis-class]
  # 	 originator: model_LuminescenceSignals()
  # 	 protocol: Bailey2001
  # 	 additional info elements:  0
  # 	 number of records: 5
  # 	 .. : RLum.Data.Curve : 5
  # 	 .. .. : #1 TL | #2 TL | #3 TL | #4 TL | #5 TL


  SEQ_sequence_renamed <- Rename_sequence(sequence)
  nb_TL_records <- length(data)
  print(glue("{nb_TL_records} TL records"))


  TL_hash <- hash()

  for(record_num in 1:nb_TL_records){


    print(glue("Record number: {record_num}"))

    Heat_rate <- SEQ_sequence_renamed[[paste0("TL_", record_num)]][3] # Get hear rate from SEQ file
    # print(glue("Heat rate: {Heat_rate}"))

    output <- Get_TL_metrics(record_num, get_RLum(data, recordType ='TL$' , drop = FALSE)[record_num],
                            Heat_rate = Heat_rate, span = 5, automatic_peak_finder = automatic_peak_finder)

    if (is.null(output$nb_peaks)) {
          TL_hash[[paste0("Record_", record_num)]][["Nb_peaks"]] <- 0
          next
      }

    TL_hash[[paste0("Record_", record_num)]][["Forced_additional_peak"]] <- output$forced_additional_peak
    TL_hash[[paste0("Record_", record_num)]][["Nb_peaks"]] <- output$nb_peaks

    if (output$forced_additional_peak) {
          unforced_nb_peaks <- output$nb_peaks - 1
      } else {
          unforced_nb_peaks <- output$nb_peaks
      }

    for(peak_num in 1:unforced_nb_peaks) {

      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_max"]][[paste0("Peak_", peak_num)]] <- output$TL_tgcd_results$pars[paste0(glue("{peak_num}th-Peak")), "INTENS(Im)"]
      TL_hash[[paste0("Record_", record_num)]][["Frequency_factor"]][[paste0("Peak_", peak_num)]] <- output$TL_tgcd_results$ff[[paste0(glue("{peak_num}th-Peak"))]]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_integrated"]][[paste0("Peak_", peak_num)]] <- output$Peak_intensities[paste0(glue("Peak.{peak_num}")), "Peak_intensities"]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity_integrated_relative"]][[paste0("Peak_", peak_num)]] <- output$Peak_intensities[paste0(glue("Peak.{peak_num}")), "Peak_intensities_relative"]

      TL_hash[[paste0("Record_", record_num)]][["Peak_temperature"]][[paste0("Peak_", peak_num)]] <- output$peaks_temp[peak_num]
      TL_hash[[paste0("Record_", record_num)]][["Peak_intensity"]][[paste0("Peak_", peak_num)]] <- output$peak_intensity[peak_num]

      }

  }

  return(TL_hash)

}



