library(RLumUnmix)
library(fingerPro)
library(dplyr)
library(purrr)
library(plyr)
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
# pop_ratios_iteration <- c(0.25, 0.5, 0.25)
# filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
#                     as.character(pop_ratios_iteration[2]), "-",
#                     as.character(pop_ratios_iteration[3]), "_all_sequence")
# data_path <- paste0(local_path, "/", filename, ".csv")

pop_ratios_iteration <- c(0.25, 0.5, 0.25)
filename <- paste0(as.character(pop_ratios_iteration[1]), "-",
                   as.character(pop_ratios_iteration[2]), "-",
                   as.character(pop_ratios_iteration[3]))
data_path <- paste0(local_path, "/", filename, ".csv")


save_path <- paste0(local_path, "/switch_sources/3_sources/", filename, "/")



###################### S1: S1 - S2: S2 - S3: S3 ######################

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list

# ##################### Shift ratio positions
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
#############################################


crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 500, seed = 1234567)
pgeo <- pairs(sgeo, mgeo, iter = 500, seed = 1234567)

## Take a look at crgeo and pgeo
head(crgeo)
pgeo[(pgeo$cons == 1) & (order(pgeo$Dmax)), ]

# Select pairs and score threshold based on crgeo and pgeo
pairs <- pgeo[(pgeo$cons == 1) & (pgeo$Dmax < 0.002), "id"]
score_threshold <- 90


proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()
all_values <- list()

for (pair in pairs) {
  for (nb_tracer in (length(source_list)+1):(nrow(crgeo[crgeo$score >= score_threshold, ]) - 1)) {

    output <- Complete_unmixing_routine(mgeo,
                                        sgeo,
                                        crgeo,
                                        source_list,
                                        Population_fraction,
                                        filename,
                                        error_threshold = NULL,
                                        score_threshold = score_threshold,
                                        nb_tracer = nb_tracer,
                                        tracer_pair = pair)

    proportions_all <- rbind(proportions_all, output$proportions)
    metrics_all <- rbind(metrics_all, output$metrics)
    ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

    temp <- output$unmixing_results_long[, c("variable", "value")]
    all_values <- append(all_values, temp[temp$variable == "S1", "value"])


  }
}

#write.csv(all_values, paste0(save_path, "all_values_S1.csv"))
#write.csv(metrics_all, paste0(save_path, "metrics_all_S1.csv"))





###################### S1: S3 - S2: S1 - S3: S2 ######################

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list

# ##################### Shift ratio positions
nb_shift <- 1

# Put row "Mixed" aside for now
df <- Population_fraction[2:nrow(Population_fraction), ]

# Shift rows by 1
Population_fraction <- rbind(Population_fraction[1, ], cbind(df["sources"], shift_rows(df["Pop_fraction"], nb_shift)))
pop_ratios_iteration <- shift_rows(as.data.frame(pop_ratios_iteration), n = nb_shift)$pop_ratios_iteration

# Shift sgeo sources
names(sgeo)[names(sgeo) == "n"] <- "n_temp"
sgeo <- shift_rows(sgeo, nb_shift)
names(sgeo)[names(sgeo) == "n_temp"] <- "n"
sgeo$id <- source_list
#############################################

crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 500, seed = 1234567)
pgeo <- pairs(sgeo, mgeo, iter = 500, seed = 1234567)

## Take a look at pgeo
head(crgeo)
pgeo[(pgeo$cons == 1) & (order(pgeo$Dmax)), ]


# Select pairs and score threshold based on crgeo and pgeo
pairs <- pgeo[(pgeo$cons == 1) & (pgeo$Dmax < 0.002), "id"]
score_threshold <- 90



proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()
all_values <- list()

for (pair in pairs) {
  for (nb_tracer in (length(source_list)+1):(nrow(crgeo[crgeo$score >= score_threshold, ]) - 1)) {

    output <- Complete_unmixing_routine(mgeo,
                                        sgeo,
                                        crgeo,
                                        source_list,
                                        Population_fraction,
                                        filename,
                                        error_threshold = NULL,
                                        score_threshold = score_threshold,
                                        nb_tracer = nb_tracer,
                                        tracer_pair = pair)

    proportions_all <- rbind(proportions_all, output$proportions)
    metrics_all <- rbind(metrics_all, output$metrics)
    ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

    temp <- output$unmixing_results_long[, c("variable", "value")]
    all_values <- append(all_values, temp[temp$variable == "S1", "value"])

  }
}

#write.csv(all_values, paste0(save_path, "all_values_S3.csv"))
#write.csv(metrics_all, paste0(save_path, "metrics_all_S3.csv"))



###################### S1: S2 - S2: S3 - S3: S1 ######################

output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list


# ##################### Shift ratio positions
nb_shift <- 2

# Put row "Mixed" aside for now
df <- Population_fraction[2:nrow(Population_fraction), ]

# Shift rows by 1
Population_fraction <- rbind(Population_fraction[1, ], cbind(df["sources"], shift_rows(df["Pop_fraction"], nb_shift)))
pop_ratios_iteration <- shift_rows(as.data.frame(pop_ratios_iteration), n = nb_shift)$pop_ratios_iteration

# Shift sgeo sources
names(sgeo)[names(sgeo) == "n"] <- "n_temp"
sgeo <- shift_rows(sgeo, nb_shift)
names(sgeo)[names(sgeo) == "n_temp"] <- "n"
sgeo$id <- source_list
#############################################


crgeo <- cr_ns(source=sgeo, mixture = mgeo, maxiter = 500, seed = 1234567)
pgeo <- pairs(sgeo, mgeo, iter = 500, seed = 1234567)

## Take a look at pgeo
head(crgeo)
pgeo[(pgeo$cons == 1) & (order(pgeo$Dmax)), ]


# Select pairs and score threshold based on crgeo and pgeo
pairs <- pgeo[(pgeo$cons == 1) & (pgeo$Dmax < 0.002), "id"]
score_threshold <- 90

proportions_all <- data.frame()
ratio_differences_all <- data.frame()
metrics_all <- data.frame()
all_values <- list()

for (pair in pairs) {
  for (nb_tracer in (length(source_list)+1):(nrow(crgeo[crgeo$score >= score_threshold, ]) - 1)) {

    output <- Complete_unmixing_routine(mgeo,
                                        sgeo,
                                        crgeo,
                                        source_list,
                                        Population_fraction,
                                        filename,
                                        error_threshold = NULL,
                                        score_threshold = score_threshold,
                                        nb_tracer = nb_tracer,
                                        tracer_pair = pair)

    proportions_all <- rbind(proportions_all, output$proportions)
    metrics_all <- rbind(metrics_all, output$metrics)
    ratio_differences_all <- rbind(ratio_differences_all, output$ratio_differences)

    temp <- output$unmixing_results_long[, c("variable", "value")]
    all_values <- append(all_values, temp[temp$variable == "S1", "value"])

  }
}

#write.csv(all_values, paste0(save_path, "all_values_S2.csv"))
#write.csv(metrics_all, paste0(save_path, "metrics_all_S2.csv"))


###################### Analysis ######################

metrics_all_S1  <- read.csv(paste0(save_path, "metrics_all_S1.csv"))
metrics_all_S2  <- read.csv(paste0(save_path, "metrics_all_S2.csv"))
metrics_all_S3  <- read.csv(paste0(save_path, "metrics_all_S3.csv"))

all_values_S1  <- read.csv(paste0(save_path, "all_values_S1.csv"))
all_values_S2  <- read.csv(paste0(save_path, "all_values_S2.csv"))
all_values_S3  <- read.csv(paste0(save_path, "all_values_S3.csv"))


output <- Prepare_data_for_tracer_selection(data_path)
sgeo <- output$sgeo
mgeo <- output$mgeo
Population_fraction <- output$Population_fraction
source_list <- output$source_list

########################################################

boxplot_data <- as.data.frame(qpcR:::cbind.na(unlist(unname(as.list(all_values_S1))[-1]), unlist(unname(as.list(all_values_S2))[-1]), unlist(unname(as.list(all_values_S3))[-1])))
colnames(boxplot_data) <- c("S1", "S2", "S3")


#png(filename = paste0(save_path, filename, "_plot.png"), width = 800, height = 600)
graphics::par(bg = "white")
boxplot(boxplot_data, ylim = c(-0.1, 1.1))

graphics::grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

title(glue("{round(Population_fraction[Population_fraction$sources == 'S1', 'Pop_fraction'],3)} - {round(Population_fraction[Population_fraction$sources == 'S2', 'Pop_fraction'], 3)} - {round(Population_fraction[Population_fraction$sources == 'S3', 'Pop_fraction'], 3)}"))


if (!is.null(Population_fraction)) {
  # Add red points
  points_plot <- Population_fraction[Population_fraction$sources != 'Mixed', ]
  points_plot$sources_int <- as.integer(gsub("S", "", points_plot$sources))
  graphics::points(points_plot$sources_int, points_plot$Pop_fraction, col = "red", pch = 19, cex = 1.5)
}
#dev.off()












