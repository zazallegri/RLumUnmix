#' Comparison of parameters
#'
#' @param param_set1 First set of parameters (same format as RLumModel::.set_pars())
#' @param param_set2 Second set of parameters (same format as RLumModel::.set_pars())
#'
#' @return Common and non-common parameters printed in console.
#' @export
#'
#' @examples compare_parameters(RLumModel::.set_pars("Bailey2001"), RLumModel::.set_pars("Bailey2001"))
compare_parameters <- function(param_set1, param_set2) {

  # Get common elements
  common_elements <- intersect(names(param_set1), names(param_set2))

  # Get non-common elements
  non_common_elements <- setdiff(union(names(param_set1), names(param_set2)), common_elements)


  print("Common elements:")
  for (common_element in common_elements) {
    print(common_element)
    print(param_set1[[common_element]])
    print(param_set2[[common_element]])
  }

  print("Non-common elements:")
  for (non_common_element in non_common_elements) {
    print(non_common_element)

    if (non_common_element %in% names(param_set1)) {
      print(param_set1[[non_common_element]])
    } else {
      print(param_set2[[non_common_element]])
    }
  }

}
