assert_vip_pop_3d <- function(x, name = deparse(substitute(x))) {
    if(!identical(class(x), "vip_population_3d"))
        stop(sprintf("'%s' must be of class 'vip_population_3d'", name),
             call. = FALSE)
}

