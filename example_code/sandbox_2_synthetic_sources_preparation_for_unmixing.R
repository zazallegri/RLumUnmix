library(RLumUnmix)
library(sandbox)
library(dplyr)
library(RLumModel)

data_path <- system.file("extdata", package="RLumUnmix")

# --------- Set parameters for each source population ---------

population_1_parameters <- RLumModel::.set_pars("Bailey2001")
population_1_parameters$N[2] <- population_1_parameters$N[2]*10

population_2_parameters <- RLumModel::.set_pars("Bailey2001")
population_2_parameters$N[1] <- 0

populations_parameters <- list(population_1_parameters, population_2_parameters)

# Setup lab dose rate + sequence
dose_rate = 0.1

sequence <- Get_sequence(file_path = paste0(data_path, "/SEQ_file.SEQ"),
                         lab_dose_rate = dose_rate)[1:8]

# Display parameters of population 1
print(populations_parameters[1])


# --------- Generate OSL-TL curves and retrive metrics ---------

metrics_output <- Make_custom_model_and_retrieve_OSL_TL_metrics(
  sequence,
  dose_rate,
  own_parameters = population_1_parameters,
  own_state_parameters = RLumModel::.set_pars("Bailey2001")$n)


metrics_1 <- Extract_OSL_TL_metrics_for_all_records(TL_hash = metrics_output$TL_metrics,
                                                    OSL_hash = metrics_output$OSL_metrics,
                                                    source_number = "S1",
                                                    records_to_extract_data_from = NULL)


# Source 2
metrics_output <- Make_custom_model_and_retrieve_OSL_TL_metrics(
  sequence,
  dose_rate,
  own_parameters = population_2_parameters,
  own_state_parameters = RLumModel::.set_pars("Bailey2001")$n)


metrics_2 <- Extract_OSL_TL_metrics_for_all_records(TL_hash = metrics_output$TL_metrics,
                                                    OSL_hash = metrics_output$OSL_metrics,
                                                    source_number = "S2",
                                                    records_to_extract_data_from = NULL)




# --------- Define population ratios in the mix sample + mix them using sandbox package ---------

pop_ratios_iteration <- c(0.25, 0.75)

## Create rulebook with population ratios and model parameters as rules in RuleBook
book_osl <- Make_luminescence_rulebook(populations_parameters = populations_parameters,
                                       pop_ratios = pop_ratios_iteration)





# --------- Make samples of the mixtures (mix of source 1 and 2 with ratios defined in "pop_ratios_iteration") ---------

# Set seed for reproducibility
set.seed(2021)

# Take a sample
sample_osl <- sandbox::make_Sample(book=book_osl,
                                   depth=2,
                                   geometry="cylinder",
                                   radius=0.0005,
                                   length=0.001)


# Make aliquots of sample
sample_osl_aliquots <- sandbox::prepare_Aliquot(sample = sample_osl, diameter = 0.5)

# Make a single df from all aliquots
stacked_aliquots <- data.frame()
for (name in names(sample_osl_aliquots)) {

  stacked_aliquots <- rbind(stacked_aliquots, sample_osl_aliquots[[name]])
}

# Generate signals based on parameters of mixture
metrics_output <- Make_custom_model_and_retrieve_OSL_TL_metrics(
  sequence,
  dose_rate,
  own_parameters = prep_parameters_from_aliquot(aliquot = stacked_aliquots),
  own_state_parameters = RLumModel::.set_pars("Bailey2001")$n)


metrics_mixed <- Extract_OSL_TL_metrics_for_all_records(TL_hash = metrics_output$TL_metrics,
                                                        OSL_hash = metrics_output$OSL_metrics,
                                                        source_number = "Mixed",
                                                        records_to_extract_data_from = NULL)


nb_pop_1 <- nrow(stacked_aliquots[stacked_aliquots$population == 1, c('population', 'osl_N1', 'osl_N2', 'osl_N3', 'osl_N4')])
nb_pop_2 <- nrow(stacked_aliquots[stacked_aliquots$population == 2, c('population', 'osl_N1', 'osl_N2', 'osl_N3', 'osl_N4')])
frac_pop_1 <- nb_pop_1/(nb_pop_1 + nb_pop_2)
frac_pop_2 <- nb_pop_2/(nb_pop_1 + nb_pop_2)

print(frac_pop_1)
print(frac_pop_2)


# --------- Prepare results for unmixing ---------

# Stack the Data Frames on top of each other, filling missing columns with NA
stacked_df <- dplyr::bind_rows(rbind(metrics_1, metrics_1), rbind(metrics_2, metrics_2), metrics_mixed)

# Replace NA values with 0
stacked_df[is.na(stacked_df)] <- 0

# fingerPro (used for unmixing) expects at least two different samples from each source. Artificially create two almost identical ones
stacked_df[2, -1] <- stacked_df[2, -1]*1.001
stacked_df[4, -1] <- stacked_df[4, -1]*1.001

# Add expected population fractions (stemming from the mixing of both sources through sandbox)
stacked_df$Pop_fraction <- c(rep(frac_pop_1, 2), rep(frac_pop_2, 2), rep(NaN, 1))

print(stacked_df)

# Save the Data Frame
#filename <- paste0(as.character(pop_ratios_iteration[1]),"-", as.character(pop_ratios_iteration[2]))
#write.csv(stacked_df, file = paste0(data_path, "/", filename, ".csv"))





# --------- If you want to make a single df for each aliquot, change the above code accordingly: ---------

## make aliquots of sample
sample_osl_aliquots <- prepare_Aliquot(sample = sample_osl, diameter = 1.6)
print(glue("Total number of aliquots: {length(names(sample_osl_aliquots))}"))

metrics_mixed_all <- data.frame()
for (name in names(sample_osl_aliquots)) {

  stacked_aliquots <- sample_osl_aliquots[[name]]
  print(glue("Aliquot {name}"))
  metrics_output <- Make_custom_model_and_retrieve_OSL_TL_metrics(
    sequence,
    dose_rate,
    own_parameters = prep_parameters_from_aliquot(aliquot = stacked_aliquots),
    own_state_parameters = RLumModel::.set_pars("Bailey2001")$n)


  nb_pop_1 <- nrow(stacked_aliquots[stacked_aliquots$population == 1, c('population', 'osl_N1', 'osl_N2', 'osl_N3', 'osl_N4')])
  nb_pop_2 <- nrow(stacked_aliquots[stacked_aliquots$population == 2, c('population', 'osl_N1', 'osl_N2', 'osl_N3', 'osl_N4')])
  frac_pop_1 <- nb_pop_1/(nb_pop_1 + nb_pop_2)
  frac_pop_2 <- nb_pop_2/(nb_pop_1 + nb_pop_2)

  metrics_mixed <- Single_record_TL_OSL_metrics_df(TL_hash = metrics_output$TL_metrics,
                                                   OSL_hash = metrics_output$OSL_metrics,
                                                   record_num = keys(metrics_output$TL_metrics)[[1]])
  metrics_mixed <- cbind("sources" = rep("Mixed", nrow(metrics_mixed)), metrics_mixed)
  metrics_mixed_all <- rbind(metrics_mixed_all, metrics_mixed)
}


