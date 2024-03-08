validate_popim_pop_3d <- function(x, name = deparse(substitute(x))) {

    ## class:
    if(!inherits(x, "list")) stop(sprintf("'%s' is not a list and therefore cannot be coerced to class `popim_population_3d`.", name))

    ## names of list elements:
    if(!identical(names(x),
              c("region_labels", "years_labels", "age_labels",
                "vacc_data", "pop_data")))
        stop(sprintf("'%s' must have list elements named 'region_labels', 'years_labels', 'age_labels', 'vacc_data', 'pop_data'", name), call. = FALSE)

    ## checking dimensions:
    n_regions <- length(x$region_labels)
    n_years <- length(x$years_labels)
    n_age <- length(x$age_labels)

    if(!identical(dim(x$vacc_data), c(n_regions, n_years, n_age)))
        stop("The dimensions of `vacc_data` don't match the labels `region_labels`, `years_labels` or `age_labels`.")

    if(!identical(dim(x$pop_data), c(n_regions, n_years, n_age)))
        stop("The dimensions of `vacc_data` don't match the labels `region_labels`, `years_labels` or `age_labels`.")

    ## checking dimnames match the labels:
    if(!all(
            identical(x$region_labels, dimnames(x$vacc_data)$region),
            identical(x$years_labels |> as.character(),
                      dimnames(x$vacc_data)$year),
            identical(x$age_labels |> as.character(),
                      dimnames(x$vacc_data)$age)))
        stop("explicit labels and dimnames of vacc_data do not match")

    if(!all(
            identical(x$region_labels, dimnames(x$pop_data)$region),
            identical(x$years_labels |> as.character(),
                      dimnames(x$pop_data)$year),
            identical(x$age_labels |> as.character(),
                      dimnames(x$pop_data)$age)))
        stop("explicit labels and dimnames of pop_data do not match")

    ## checking range of data in the arrays
    if(min(x$vacc_data, na.rm = TRUE) < 0 | max(x$vacc_data, na.rm = TRUE) > 1)
        stop("`vacc_data` must be between 0 and 1 for all non-missing entries")
    if(min(x$pop_data, na.rm = TRUE) < 0)
        stop("`pop_data` must be positive.")

    x    
}
