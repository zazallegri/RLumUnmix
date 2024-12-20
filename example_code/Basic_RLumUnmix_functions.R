library(RLumUnmix)
library(Luminescence)
library(glue)
library(hash)

# Set path
data_path <- system.file("extdata", package="RLumUnmix")


### ----------------Read/Create sequence ----------------

## Read .SEQ sequence
example_sequence <- Get_sequence(file_path = paste0(data_path, "/SEQ_file.SEQ"), lab_dose_rate = 0.2)
print(example_sequence)

## Create sequence manually
manual_sequence <- Create_sequence_manually()
print(manual_sequence)


## Save as .RData
saveRDS(manual_sequence, file = paste0(data_path, "manual_sequence.RData"), compress = FALSE)

## Now it can be read with Get_sequence()
example_manual_sequence <- Get_sequence(file_path = paste0(data_path, "manual_sequence.RData"),
                                        lab_dose_rate = 0.2)
print(example_manual_sequence)


### ----------------Compute OSL and TL metrics from synthetic/lab signals ----------------

## Generate synthetic OSL and TL signals
data <- Generate_synthetic_OSL_TL_signals(sequence = example_sequence, model = "Bailey2001")
print(data)

## Extract metrics from OSL and TL signals

OSL_metrics <- Compute_OSL_metrics(sequence = example_sequence,
                                   data = Luminescence::get_RLum(data, recordType ='OSL$' , drop = FALSE))

TL_metrics <- Compute_TL_metrics(sequence = example_sequence,
                                 data = Luminescence::get_RLum(data, recordType ='TL$' , drop = FALSE),
                                 automatic_peak_finder =  TRUE)

## Show results for first TL curve metrics
print(TL_metrics[['Record_1']])



### ----------------Extract metrics from signals stemming from .binx file ----------------
## Read binx file and extract metrics (only for sample located at position 1 in a standard risø TL/OSL reader)

# Note that the automatic peak detection is not as good as for the synthetic signals.
# Argument "automatic_peak_finder" in function [Compute_TL_metrics()] can be set to FALSE to select peaks manually.

bin_file <- read_BIN2R(file = paste0(data_path, "/binx_file.binx"), fastForward = FALSE)

print(glue("Position numbers: {unique(as.data.frame(bin_file)$POSITION)}"))

OSL_metrics <- hash()
TL_metrics <- hash()

sample_position_on_disk <- 1

if ('OSL' %in% unique(data.frame(bin_file)[, "LTYPE"])) {
  OSL_data <- Risoe.BINfileData2RLum.Analysis(bin_file, pos = sample_position_on_disk, ltype = "OSL")
  OSL_metrics[sample_position_on_disk] <- Compute_OSL_metrics(sequence = example_sequence, data = OSL_data)
}

if ('TL' %in% unique(data.frame(bin_file)[, "LTYPE"])) {
  TL_data <- Risoe.BINfileData2RLum.Analysis(bin_file, pos = sample_position_on_disk, ltype = "TL")
  TL_metrics[sample_position_on_disk] <- Compute_TL_metrics(sequence = example_sequence,
                                                            data = TL_data,
                                                            automatic_peak_finder = TRUE)
}



