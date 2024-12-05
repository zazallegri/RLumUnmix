#' Rename sequence
#'
#' @param list_of_lists Sequence
#'
#' @return Renamed sequence with unique names
#' @export
#'
#' @examples se vignettes
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
