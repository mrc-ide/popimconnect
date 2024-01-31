assert_vip_pop_wide <- function(x, name = deparse(substitute(x))) {
    if(!class(x) == "vip_population_wide")
        stop(sprintf("'%s' must be of class 'vip_pop_wide'", name),
             call. = FALSE)
}

