library(RLumUnmix)
library(fingerPro)
library(dplyr)
library(purrr)
library(plyr)
library(robustbase)
library(qpcR)
library(glue)



shift_rows <- function(df, n) {

  if (n == 0){
    return (df)
  } else {

    return (df %>%
              slice(c((nrow(df) - n + 1):nrow(df), 1:(nrow(df) - n))))
  }
}


local_path <- system.file("extdata", package="RLumUnmix")

# Original pop_ratios_iteration

# pop_ratios_iteration <- c(0.25, 0.75)
# filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
#                     as.character(pop_ratios_iteration[2]))
# data_path <- paste0(local_path,"/", filename, ".csv")

# pop_ratios_iteration <- c(0.5, 0.5)
# filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
#                     as.character(pop_ratios_iteration[2]))
# data_path <- paste0(local_path, "/unmixing_samples/Sample_1/", filename, ".csv")

pop_ratios_iteration <- c(0.75, 0.25)
filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
                   as.character(pop_ratios_iteration[2]))
data_path <- paste0(local_path, "/unmixing_samples/Sample_2/", filename, ".csv")

# pop_ratios_iteration <- c(0.34, 0.66)
# filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
#                     as.character(pop_ratios_iteration[2]))
# data_path <- paste0(local_path, "/unmixing_samples/Sample_3/", filename, ".csv")


save_path <- paste0(local_path, "/switch_sources/2_sources/", filename, "/")


###################### S1: S1 - S2: S2 ######################

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list
# ##################### Shift ratio positions #################################################################
nb_shift <- 0

# Put row "Mixed" aside for now
df <- Population_fraction[2:nrow(Population_fraction), ]

# Shift rows by 0
Population_fraction <- rbind(Population_fraction[1, ], cbind(df["sources"], shift_rows(df["Pop_fraction"], nb_shift)))
pop_ratios_iteration <- shift_rows(as.data.frame(pop_ratios_iteration), n = nb_shift)$pop_ratios_iteration

# Shift sgeo sources
names(sgeo)[names(sgeo) == "n"] <- "n_temp"
sgeo <- shift_rows(sgeo, nb_shift)
names(sgeo)[names(sgeo) == "n_temp"] <- "n"
sgeo$id <- source_list
###################################################################################

# Get a sinlge mix line
if (nrow(mgeo) > 1) {
  print("mgeo contains multiple aliquots - transformed to a single one")
  # Put all "Mixed" data in a single line using median value
  mgeo <- as.data.frame(cbind(id = "M1", t(colMedians(as.matrix(mgeo[, -1]), na.rm = TRUE))))
  mgeo <- type.convert(mgeo, as.is = TRUE)# Modify column classes
}

# Some values where 0, fingerPro does not like that...
sgeo[sgeo == 0] <- 1e-15
mgeo[mgeo == 0] <- 1e-15

crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 1000, seed = 1234567)
head(crgeo)

# Select threshold based on "head(crgeo)"
score_threshold <- 80


proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()
all_values <- list()

for (nb_tracer in (length(source_list)+1):(nrow(crgeo[crgeo$score >= score_threshold, ])-1)) {

  output <- Complete_unmixing_routine(mgeo,
                                      sgeo,
                                      crgeo,
                                      source_list,
                                      Population_fraction,
                                      filename,
                                      score_threshold = score_threshold,
                                      nb_tracer = nb_tracer)


  proportions_all <- rbind(proportions_all, output$proportions)
  metrics_all <- rbind(metrics_all, output$metrics)
  ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

  temp <- output$unmixing_results_long[, c("variable", "value")]
  all_values <- append(all_values, temp[temp$variable == "S1", "value"])
}

#write.csv(all_values, paste0(save_path, "all_values_S1.csv"))
#write.csv(metrics_all, paste0(save_path, "metrics_all_S1.csv"))


###################### S1: S2 - S2: S1 ######################

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list

# ##################### Shift ratio positions #################################################################
nb_shift <- 1

# Put row "Mixed" aside for now
df <- Population_fraction[2:nrow(Population_fraction), ]

# Shift rows by 0
Population_fraction <- rbind(Population_fraction[1, ], cbind(df["sources"], shift_rows(df["Pop_fraction"], nb_shift)))
pop_ratios_iteration <- shift_rows(as.data.frame(pop_ratios_iteration), n = nb_shift)$pop_ratios_iteration

# Shift sgeo sources
names(sgeo)[names(sgeo) == "n"] <- "n_temp"
sgeo <- shift_rows(sgeo, nb_shift)
names(sgeo)[names(sgeo) == "n_temp"] <- "n"
sgeo$id <- source_list
###################################################################################

# Get a sinlge mix line
if (nrow(mgeo) > 1) {
  print("mgeo contains multiple aliquots - transformed to a single one")
  # Put all "Mixed" data in a single line using median value
  mgeo <- as.data.frame(cbind(id = "M1", t(colMedians(as.matrix(mgeo[, -1]), na.rm = TRUE))))
  mgeo <- type.convert(mgeo, as.is = TRUE)# Modify column classes
}

# Some values where 0, fingerPro does not like that...
sgeo[sgeo == 0] <- 1e-15
mgeo[mgeo == 0] <- 1e-15

crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 1000, seed = 1234567)
head(crgeo)


# Define threshold based on "head(crgeo")
score_threshold <- 80

proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()
all_values <- list()

for (nb_tracer in (length(source_list)+1):(nrow(crgeo[crgeo$score >= score_threshold, ]) - 1)) {

  output <- Complete_unmixing_routine(mgeo,
                                      sgeo,
                                      crgeo,
                                      source_list,
                                      Population_fraction,
                                      filename,
                                      score_threshold = score_threshold,
                                      nb_tracer = nb_tracer)


  proportions_all <- rbind(proportions_all, output$proportions)
  metrics_all <- rbind(metrics_all, output$metrics)
  ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

  temp <- output$unmixing_results_long[, c("variable", "value")]
  all_values <- append(all_values, temp[temp$variable == "S1", "value"])


}

#write.csv(all_values, paste0(save_path, "all_values_S2.csv"))
#write.csv(metrics_all, paste0(save_path, "metrics_all_S2.csv"))


###################### Analysis ######################

metrics_all_S1 <- read.csv(paste0(save_path, "metrics_all_S1.csv"))
metrics_all_S2 <- read.csv(paste0(save_path, "metrics_all_S2.csv"))

all_values_S1 <- read.csv(paste0(save_path, "all_values_S1.csv"))
all_values_S2 <- read.csv(paste0(save_path, "all_values_S2.csv"))

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list



boxplot_data <- as.data.frame(qpcR:::cbind.na(unlist(unname(as.list(all_values_S1))[-1]), unlist(unname(as.list(all_values_S2))[-1])))
colnames(boxplot_data) <- c('S1', 'S2')

#png(filename = paste0(save_path, filename, "_plot.png"), width = 800, height = 600)
graphics::par(bg = "white")
boxplot(boxplot_data, ylim = c(-0.1, 1.1))

graphics::grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
title(glue("{round(Population_fraction[Population_fraction$sources == 'S1', 'Pop_fraction'],3)} - {round(Population_fraction[Population_fraction$sources == 'S2', 'Pop_fraction'], 3)}"))

if (!is.null(Population_fraction)) {
  # Add red points
  points_plot <- Population_fraction[Population_fraction$sources != 'Mixed', ]
  points_plot$sources_int <- as.integer(gsub("S", "", points_plot$sources))
  graphics::points(points_plot$sources_int, points_plot$Pop_fraction, col = "red", pch = 19, cex = 1.5)
}

#dev.off()



