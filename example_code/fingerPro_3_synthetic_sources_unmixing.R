library(RLumUnmix)
library(fingerPro)

data_path <- system.file("extdata", package="RLumUnmix")

# ------------ Read and prepare data for unmixing ------------


## Declare Data Frame for results
proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()

## Define expected ratios (if known) - also used as filename
pop_ratios_iteration <- c(0.25, 0.5, 0.25)

## File corresponds to "stacked_df" from vignette "sandbox_3_synthetic_sources_preparations_for_unmixing".
filename <- paste0(as.character(pop_ratios_iteration[1]), "-", as.character(pop_ratios_iteration[2]), "-", as.character(pop_ratios_iteration[3]))
data_path <- paste0(data_path, "/", filename, ".csv")

## Read and prepare data for unmixing
output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list
stacked_df_OG <- output$stacked_df_OG

print(sgeo)
print(mgeo)


# ------------ Perform consensus ranking and "pairs" method describes in fingerPro pacakge ------------

crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 1000, seed = 1234567)
head(crgeo)
pgeo <- pairs(sgeo, mgeo, iter = 1000, seed = 1234567)
head(pgeo)



# ------------  Take a look at pgeo and select the pairs to use for unmixing ------------

print(pgeo[(pgeo$cons == 1) & (order(pgeo$Dmax)), ])

pairs <- pgeo[(pgeo$cons == 1) & (pgeo$Dmax < 0.002), "id"]



# ------------ Perform unmixing using 2 - 10 tracers with pairs selected above ------------

for (pair in pairs) {
  for (nb_tracer in 2:10) {

    output <- Complete_unmixing_routine(mgeo,
                                        sgeo,
                                        crgeo,
                                        source_list,
                                        Population_fraction,
                                        filename,
                                        error_threshold = NULL,
                                        score_threshold = 80,
                                        nb_tracer = nb_tracer,
                                        tracer_pair = pair)

    proportions_all <- rbind(proportions_all, output$proportions)
    metrics_all <- rbind(metrics_all, output$metrics)
    ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

  }
}


# ------------ Get best results (number of tracers used for unmixing that minimizes the Manhattan distance between the expected proportion and the median/mean estimated proportion) ------------

best_metrics <- metrics_all[(metrics_all$Manhattan_dist_mean <= 0.225) & (metrics_all$Manhattan_dist_median <= 0.235), ]
print(best_metrics)

proportions_best <- data.frame()
ratio_differences_best <- data.frame()
metrics_best <- data.frame()

for (i in seq_len(nrow(best_metrics))) {

  output <- Complete_unmixing_routine(mgeo,
                                      sgeo,
                                      crgeo,
                                      source_list,
                                      Population_fraction,
                                      filename,
                                      error_threshold = NULL,
                                      score_threshold = best_metrics[i, ]$score_threshold,
                                      nb_tracer = best_metrics[i, ]$nb_tracer,
                                      tracer_pair = best_metrics[i, ]$tracer_pair)


  proportions_best <- rbind(proportions_best, output$proportions)
  metrics_best <- rbind(metrics_best, output$metrics)
  ratio_differences_best <- rbind(ratio_differences_best, output$ratio_differences)

}

# Results are clearly not as good as with two sources. More different proportions led to better results.


