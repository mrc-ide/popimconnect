assert_popim_pop_3d <- function(x, name = deparse(substitute(x))) {
    if(!identical(class(x), c("popim_population_3d", "list")))
        stop(sprintf("'%s' must be of class 'popim_population_3d'", name),
             call. = FALSE)
}

