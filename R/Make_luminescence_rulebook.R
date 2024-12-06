#' Create Luminescence RuleBook
#'
#' @description
#' Sets sandbox RuleBook with luminescence properties as rules + strict minimum properties mandatory in the rule book (grainsize distribution, age).
#' Rules should be changed/added according to specific needs.
#'
#'
#'
#' @param populations_parameters Bailey2001 parameters for every population in the RuleBook
#' @param pop_ratios Ratios of populations in the mix
#'
#' @return Rulebook with luminescence model properties as rules
#' @export
#'
#' @examples see vignettes
Make_luminescence_rulebook <- function(populations_parameters, pop_ratios) {

  # Sets sandbox rulebook with luminescence properties as rules + strict minimum properties mandatory in the rule book (grainsize distribution, age).
  # Rules should be changed/added according to specific needs.

  if (length(populations_parameters) != length(pop_ratios)) {
    print("ERROR in [Make_luminescence_rulebook()]: Number of populations extracted from aruments 'population_parameters' and 'pop_ratios' do not match")
    stop()
  }

  nb_populations <- length(pop_ratios)

  ## get empty rulebook
  # The following adds rules for age, population, grainsize, packing and density
  # In addition, also adds the 7 parameters of Bailey2001 to the rule book
  # -> (N, E, s, A B, Th, E_th) preceded by "osl_". (1-9 for N, E, s, A B and 1-5 for Th, E_th)
  # Also adds osl_doserate and osl_R
  book_osl<-sandbox::get_RuleBook(book="empty",
                         osl="Bailey2001")

  ## add another populations
  book_osl <- sandbox::add_Population(book = book_osl,
                             populations = nb_populations-1)#first pop is added by default in get_RuleBook()

  ## Change age depth data
  depth_true<-list(seq(from=0.5, to=10.5, by=1))

  ## get number of depth intervals
  n_depth<-length(depth_true[[1]])

  # set true age
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


  ## add population contribution with depth
  book_osl <- sandbox::set_Rule(book=book_osl,
                     parameter="population",
                     value=population_proportion_fill,
                     depth=depth_true)

  # update rulebook with true age and depth definition
  book_osl <- sandbox::set_Rule(book=book_osl,
                     parameter="age",
                     value=age_true,
                     depth=depth_true)



  ## update rulebook with default luminescence model parameters
  # set_Rule() for all 7 model parameters + osl_R (mean = value from .set_pars(), sd = 0)
  # osl_doserate to be set seperately
  book_osl <- sandbox::set_Rule(book=book_osl,
                     parameter="Bailey2001",
                     depth=depth_true)

  # set_Rule() for all 7 model parameters. Mean is based on original populations_parameters
  # and sd = 0. The same mean is given to all depth. Function [Get_param_normal_dist_list_of_values()] shoulde be
  # modified/replaced to give different values for each depth.
  book_osl <- set_Rule_for_all_model_parameters(book = book_osl,
                                                depth = depth_true,
                                                populations_parameters = populations_parameters)




  book_osl <- sandbox::set_Rule(book=book_osl,
                     parameter="grainsize",
                     value=gsd_fill,
                     depth=depth_true)


  return(book_osl)

}
