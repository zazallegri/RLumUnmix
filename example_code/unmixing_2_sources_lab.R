library(RLumUnmix)
library(hash)
library(Luminescence)
library(robustbase)
library(dplyr)
library(fingerPro)

data_path <- paste0(system.file("extdata", package="RLumUnmix"),"/unmixing_samples/")

# ---------- Get sequence + samples information ----------

#We use two very different pure samples, Fontainebleau (source 1) and Morat (source 2). We then created three mixed samples of different proportions (based on weight), that were each divided into 10 aliquots. We hence have 50 measurements in total (10 for each source and mixtures).


sequence <- Get_sequence(file_path = paste0(data_path, "SOURCE/R3_unmixing_2_sources_21.11.24_SOURCE.SEQ"), lab_dose_rate = 0.2)[1:4]

mixed_samples_information <- hash()

# Origin of the sources
mixed_samples_information[["Sources"]][["Source_1"]] <- "FBQ"
mixed_samples_information[["Sources"]][["Source_2"]] <- "Morat"

# Definition of our samples (mixtures): position on carousel + expected proportions
mixed_samples_information[["Sample_1"]][["Positions"]] <- seq(1, 10)
mixed_samples_information[["Sample_1"]][["Proportions"]] <- c(0.5, 0.5)

mixed_samples_information[["Sample_2"]][["Positions"]] <- seq(11, 20)
mixed_samples_information[["Sample_2"]][["Proportions"]] <- c(0.75, 0.25)

mixed_samples_information[["Sample_3"]][["Positions"]] <- seq(21, 30)
mixed_samples_information[["Sample_3"]][["Proportions"]] <- c(0.34, 0.66)


# ---------- Preparation of source data ----------

# Read binx file of the sources
bin_file_SOURCE <- read_BIN2R(file = paste0(data_path,
                                            "SOURCE/R3_unmixing_2_sources_21.11.24_SOURCE.binx"),
                              fastForward = FALSE)

print(bin_file_SOURCE)




# ---------- Extract metrics of the source data ----------

OSL_metrics <- hash()
TL_metrics <- hash()
metrics <- data.frame(matrix(NA, nrow = 0, ncol = 0))

for (sample_position_on_disk in unique(as.data.frame(bin_file_SOURCE)$POSITION)) {

  if ((sample_position_on_disk >= 1)&(sample_position_on_disk <= 10)) {
    source_num <- "S1"
  } else if ((sample_position_on_disk >= 11)&(sample_position_on_disk <= 20)) {
    source_num <- "S2"
  } else {
    print("ERROR - sample_position_on_disk is out of range")
  }


  if ('OSL' %in% unique(data.frame(bin_file_SOURCE)[, "LTYPE"])) {
    OSL_data <- Risoe.BINfileData2RLum.Analysis(bin_file_SOURCE, pos = sample_position_on_disk, ltype = "OSL")
    OSL_metrics[sample_position_on_disk] <- Compute_OSL_metrics(sequence = sequence, data = OSL_data)
  }

  if ('TL' %in% unique(data.frame(bin_file_SOURCE)[, "LTYPE"])) {
    TL_data <- Risoe.BINfileData2RLum.Analysis(bin_file_SOURCE, pos = sample_position_on_disk, ltype = "TL")
    TL_metrics[sample_position_on_disk] <- Compute_TL_metrics(sequence = sequence,
                                                              data = TL_data,
                                                              automatic_peak_finder = TRUE)
  }

  metrics_temp <- Extract_OSL_TL_metrics_for_all_records(TL_hash = TL_metrics[[as.character(sample_position_on_disk)]],
                                                         OSL_hash = OSL_metrics[[as.character(sample_position_on_disk)]],
                                                         source_number = source_num,
                                                         records_to_extract_data_from = NULL)

  metrics_temp <- cbind(disk_position = sample_position_on_disk, metrics_temp)
  metrics <- bind_rows(metrics, metrics_temp)

}

# Replace NA values with 0
metrics[is.na(metrics)] <- 0

print(metrics)
# Save the Data Frame
#write.csv(metrics, file = paste0(data_folder, "SOURCE/metrics_SOURCE.csv"))


# ---------- Preparation of mix data ----------

# Read binx file of the mixes
bin_file_MIX <- read_BIN2R(file = paste0(data_path,
                                         "MIX/R3_unmixing_2_sources_22.11.24_MIX.binx"),
                           fastForward = FALSE)

print(bin_file_MIX)


# ---------- Extract metrics of mixed data ----------

OSL_metrics <- hash()
TL_metrics <- hash()
metrics <- data.frame(matrix(NA, nrow = 0, ncol = 0))

for (sample_position_on_disk in unique(as.data.frame(bin_file_MIX)$POSITION)) {


  if ('OSL' %in% unique(data.frame(bin_file_MIX)[, "LTYPE"])) {
    OSL_data <- Risoe.BINfileData2RLum.Analysis(bin_file_MIX, pos = sample_position_on_disk, ltype = "OSL")
    OSL_metrics[sample_position_on_disk] <- Compute_OSL_metrics(sequence = sequence, data = OSL_data)
  }

  if ('TL' %in% unique(data.frame(bin_file_MIX)[, "LTYPE"])) {
    TL_data <- Risoe.BINfileData2RLum.Analysis(bin_file_MIX, pos = sample_position_on_disk, ltype = "TL")
    TL_metrics[sample_position_on_disk] <- Compute_TL_metrics(sequence = sequence,
                                                              data = TL_data,
                                                              automatic_peak_finder = TRUE)
  }

  metrics_temp <- Extract_OSL_TL_metrics_for_all_records(TL_hash = TL_metrics[[as.character(sample_position_on_disk)]],
                                                         OSL_hash = OSL_metrics[[as.character(sample_position_on_disk)]],
                                                         source_number = "Mixed",
                                                         records_to_extract_data_from = NULL)

  metrics_temp <- cbind(disk_position = sample_position_on_disk, metrics_temp)
  metrics <- bind_rows(metrics, metrics_temp)

}

# Replace NA values with 0
metrics[is.na(metrics)] <- 0

# Save Data Frame
#write.csv(metrics, file = paste0(data_folder, "MIX/metrics_MIX.csv"))



# ---------- Prepare Data Frame that will be used for the unmixing ----------

# From previous steps:
metrics_SOURCE <- read.csv(file = paste0(data_path, "SOURCE/metrics_SOURCE.csv"))
metrics_MIX <- read.csv(file = paste0(data_path, "MIX/metrics_MIX.csv"))

## Sample 1
sample <- "Sample_1"
stacked_sample_1 <- bind_rows(metrics_SOURCE, metrics_MIX[metrics_MIX$disk_position %in% mixed_samples_information[[sample]][["Positions"]], ])

# Replace NA values with 0
stacked_sample_1[is.na(stacked_sample_1)] <- 0

# Add expected proportions - not mandatory
stacked_sample_1$Pop_fraction <- c(rep(mixed_samples_information[[sample]][["Proportions"]][1],
                                       nrow(stacked_sample_1[stacked_sample_1$sources == "S1", ])),
                                   rep(mixed_samples_information[[sample]][["Proportions"]][2],
                                       nrow(stacked_sample_1[stacked_sample_1$sources == "S2", ])),
                                   rep(NaN, nrow(stacked_sample_1[stacked_sample_1$sources == "Mixed", ])))

# Remove column named 'disk_position'
stacked_sample_1 <- stacked_sample_1 %>% select(-disk_position)

# Save
#filename <- paste0(as.character(mixed_samples_information[[sample]][["Proportions"]][1]),"-", as.character(mixed_samples_information[[sample]][["Proportions"]][2]))
#write.csv(stacked_sample_1, file = paste0(data_path, sample, "/", filename, ".csv"))


########################################################################################

## Sample 2
sample <- "Sample_2"
stacked_sample_2 <- bind_rows(metrics_SOURCE, metrics_MIX[metrics_MIX$disk_position %in% mixed_samples_information[[sample]][["Positions"]], ])

# Replace NA values with 0
stacked_sample_2[is.na(stacked_sample_2)] <- 0

# Add expected proportions
stacked_sample_2$Pop_fraction <- c(rep(mixed_samples_information[[sample]][["Proportions"]][1],
                                       nrow(stacked_sample_2[stacked_sample_2$sources == "S1", ])),
                                   rep(mixed_samples_information[[sample]][["Proportions"]][2],
                                       nrow(stacked_sample_2[stacked_sample_2$sources == "S2", ])),
                                   rep(NaN, nrow(stacked_sample_2[stacked_sample_2$sources == "Mixed", ])))

# Remove column named 'disk_position'
stacked_sample_2 <- stacked_sample_2 %>% select(-disk_position)

# Save
#filename <- paste0(as.character(mixed_samples_information[[sample]][["Proportions"]][1]),"-", as.character(mixed_samples_information[[sample]][["Proportions"]][2]))
#write.csv(stacked_sample_2, file = paste0(data_path, sample, "/", filename, ".csv"))

########################################################################################

## Sample 3
sample <- "Sample_3"
stacked_sample_3 <- bind_rows(metrics_SOURCE, metrics_MIX[metrics_MIX$disk_position %in% mixed_samples_information[[sample]][["Positions"]], ])

# Replace NA values with 0
stacked_sample_3[is.na(stacked_sample_3)] <- 0

# Add expected proportions
stacked_sample_3$Pop_fraction <- c(rep(mixed_samples_information[[sample]][["Proportions"]][1],
                                       nrow(stacked_sample_3[stacked_sample_3$sources == "S1", ])),
                                   rep(mixed_samples_information[[sample]][["Proportions"]][2],
                                       nrow(stacked_sample_3[stacked_sample_3$sources == "S2", ])),
                                   rep(NaN, nrow(stacked_sample_3[stacked_sample_3$sources == "Mixed", ])))

# Remove column named 'disk_position'
stacked_sample_3 <- stacked_sample_3 %>% select(-disk_position)

# Save
#filename <- paste0(as.character(mixed_samples_information[[sample]][["Proportions"]][1]),"-", as.character(mixed_samples_information[[sample]][["Proportions"]][2]))
#write.csv(stacked_sample_3, file = paste0(data_path, sample, "/", filename, ".csv"))




# ---------- Perform unmixing ----------

# Choose sample to work with
sample <- "Sample_2"

pop_ratios_iteration <- mixed_samples_information[[sample]][["Proportions"]]
filename <- paste0(as.character(mixed_samples_information[[sample]][["Proportions"]][1]), "-", as.character(mixed_samples_information[[sample]][["Proportions"]][2]))
data_path <- paste0(data_path, sample, "/", filename, ".csv")

# Same procedure as for synthetic signals (vignette "Unmixing of 2 synthetic sources")
output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list
stacked_df_OG <- output$stacked_df_OG

#It seemed that the unmixing performed better when using a single mix sample. We hence use the median value of all mix samples for every tracer:

mgeo <- as.data.frame(cbind(id = "M1", t(robustbase::colMedians(as.matrix(mgeo[, -1]), na.rm = TRUE))))
mgeo <- type.convert(mgeo, as.is = TRUE)# Modify column classes

# Some values where 0, fingerPro does not like that...
sgeo[sgeo == 0] <- 1e-15
mgeo[mgeo == 0] <- 1e-15

## Consensus ranking
crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 1000, seed = 1234567)
head(crgeo)




# ---------- Complete unmixing routine ----------

proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()

for (nb_tracer in 2:15) {

  output <- Complete_unmixing_routine(mgeo,
                                      sgeo,
                                      crgeo,
                                      source_list,
                                      Population_fraction,
                                      filename,
                                      score_threshold = 80,
                                      nb_tracer = nb_tracer)


  proportions_all <- rbind(proportions_all, output$proportions)
  metrics_all <- rbind(metrics_all, output$metrics)
  ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

}

## Look at results

head(proportions_all)
head(ratio_differences_all)
head(metrics_all)


# ---------- Look at best result ----------

best_results <- head(metrics_all[order(metrics_all$Manhattan_dist_median),],1)

output <- Complete_unmixing_routine(mgeo,
                                    sgeo,
                                    crgeo,
                                    source_list,
                                    Population_fraction,
                                    filename,
                                    score_threshold = 80,
                                    nb_tracer = best_results$nb_tracer)


head(best_results)
