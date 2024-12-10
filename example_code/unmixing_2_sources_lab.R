library(RLumUnmix)
library(hash)
library(robustbase)

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
