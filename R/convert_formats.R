## convert the population structure between Keith's wide and my long
## format

## constructor for an empty population in the wide format: this is
## exactly Keith's format, including the idiosynchratic names.
vip_pop_3d <- function(region = character(),
                       year_min = integer(), year_max = integer(),
                       age_min = 0, age_max = 100) {

    vip:::assert_character(region)
    
    vip:::assert_scalar_wholenumber(year_min)
    vip:::assert_scalar_wholenumber(year_max)
    vip:::assert_non_negative(year_max - year_min)

    vip:::assert_scalar_wholenumber(age_min)
    vip:::assert_scalar_wholenumber(age_max)
    vip:::assert_non_negative(age_min)
    vip:::assert_non_negative(age_max - age_min)

    age <- age_min:age_max
    year <- year_min:year_max

    my_dims <- c(length(region), length(year), length(age))
    my_dimnames <- list(region = region, year = year, age = age)

    ## the arrays containing the immunity and population data are
    ## initialised to NA.
    vacc_data <- array(data = rep(0, length(region)*length(year)*length(age)),
                       dim = my_dims, dimnames = my_dimnames)
    pop_data <- array(data = rep(NA_real_,
                                 length(region)*length(year)*length(age)),
                      dim = my_dims, dimnames = my_dimnames)
                       
    pop_wide <- list(region_labels = region,
                     years_labels = year,
                     age_labels = age,
                     vacc_data = vacc_data,
                     pop_data = pop_data)

    class(pop_wide) <- c("vip_population_3d", "list")

    validate_vip_pop_3d(pop_wide)
}

##' Converts from the standard (long) `vip_population` format to the 3d format
##'
##' Takes a vip_population object and converts it to the 3d format
##' that is based on 3d arrays for region / year / age.
##'
##' The S3 class `vip_pop_3d` is a list of 5 elements, the first 3
##' label the dimesions of the data, the last 3 are the 3d arrays
##' holding the data that is stored in the `immunity` and `pop_size`
##' columns of the `vip_population` object converted:
##'
##' * region_labels: character vector labelling the first dimension of
##'   the 3d arrays, it holds the labels from the `region` column of
##'   the `vip_population` object,
##' 
##' * years_labels: integer vector labelling the 2nd dimension, it
##'   holds the calendar years from column `year`
##'
##' * age_labels: integer vector labelling the 3rd dimension, holding
##'   the age of each cohort from column `age`
##'
##' * vacc_data: 3d numeric array, holding the immunity data from column
##'   `immunity`. All entries must be between 0 and 1.
##'
##' * pop_data: 3d numeric array, holding the population size data for
##'   the relevant birth cohort from column `pop_size`. All entries must
##'   be non-negative.
##'
##' @param pop_long object of class "vip_population" to be converted
##'     to the 3d format
##' @return object of S3 class `vip_pop_3d`: list of * the labels
##'     for each dimension (region, year, age) * two 3d arrays
##'     vacc_data and pop_data holding the data stored in the columns
##'     `immunity` and `pop_size` in the input `vip_population` object
##' @seealso [convert_pop_to_3d()], [vip::vip_population()]
##' @author Tini Garske
##' @importFrom rlang .data
convert_pop_to_3d <- function(pop_long) {

    vip:::assert_population(pop_long)

    ## setting up the structure:
    pop_wide <- vip_pop_3d(region = attributes(pop_long)$region,
                             year_min = attributes(pop_long)$year_min,
                             year_max = attributes(pop_long)$year_max,
                             age_min = attributes(pop_long)$age_min,
                             age_max = attributes(pop_long)$age_max)

    ## filling the vacc_data array:
    for(reg in attributes(pop_long)$region) {

        pop_wide$vacc_data[reg,,] <-
        pop_long |> dplyr::filter(.data$region == reg) |>
            dplyr::select(tidyselect::any_of(c("year", "age", "immunity"))) |>
            tidyr::pivot_wider(names_from = tidyselect::all_of("age"),
                               values_from = tidyselect::all_of("immunity")) |>
            dplyr::arrange(.data$year) |>
            dplyr::select(!tidyselect::any_of("year")) |> as.matrix()

    }
    ## filling in the pop_data array if pop_long has a pop_size column:
    if("pop_size" %in% names(pop_long)) {
        for(reg in attributes(pop_long)$region) {
            pop_wide$pop_data[reg,,] <-
                pop_long |> dplyr::filter(.data$region == reg) |>
                dplyr::select(
                           tidyselect::all_of(c("year", "age", "pop_size"))) |>
                tidyr::pivot_wider(names_from = tidyselect::all_of("age"),
                                   values_from = tidyselect::all_of("pop_size")) |>
                dplyr::arrange(.data$year) |>
                dplyr::select(!tidyselect::any_of("year")) |> as.matrix()
        }
    }
    pop_wide
}

##' Convert from the `vip_population_3d` class `vip_population` class
##' (long format)
##' 
##' Takes and object of class `vip_population_3d` and converts it to
##' the standard (long format) of S3 class `vip_population` (as
##' generated by [vip::vip_population()].
##' 
##' @param pop_3d An object of class `vip_population_3d`
##' @return An object of class `vip_population`
##' @seealso [vip::vip_population()], [convert_pop_to_3d()]
##' @author Tini Garske
##' @export
convert_pop_to_long <- function(pop_3d) {

    assert_vip_pop_3d(pop_3d) ## just checking the class is correct.

    ## setting up the structure:
    pop_long <- vip::vip_population(region = pop_3d$region_labels,
                                    year_min = pop_3d$years_labels |> min(),
                                    year_max = pop_3d$years_labels |> max(),
                                    age_min = pop_3d$age_labels |> min(),
                                    age_max = pop_3d$age_labels |> max())
    ## take off the columns to be filled in with data from pop_3d:
    pop_long <- pop_long |>
        dplyr::select(!tidyselect::any_of(c("immunity", "pop_size")))

    ## filling in the data:
    ## local conversion function as I don't want to check for valid inputs...
    array_to_df <- function(pv_data, col_name = "Freq") {
        ## data format conversion:
        pl <- as.data.frame.table(pv_data, stringsAsFactors = FALSE)
        pl$year = as.integer(pl$year)
        pl$age = as.integer(pl$age)

        names(pl)[names(pl) == "Freq"] = col_name

        pl
    }

    pl <- array_to_df(pop_3d$vacc_data, col_name = "immunity")
    pop_long <- dplyr::left_join(pop_long, pl, by = c("region", "year", "age"))

    pl <- array_to_df(pop_3d$pop_data, col_name = "pop_size")
    pop_long <- dplyr::left_join(pop_long, pl, by = c("region", "year", "age"))

    ## set the attributes:
    region <- pop_3d$region_labels
    year_min <- pop_3d$years_labels |> min()
    year_max <- pop_3d$years_labels |> max()
    age_min <- pop_3d$age_labels |> min()
    age_max <- pop_3d$age_labels |> max()

    pop_long <- structure(
        pop_long,
        region = region,
        year_min = year_min,
        year_max = year_max,
        age_min = age_min,
        age_max = age_max,
        class = c("vip_population", "data.frame")
    )

    pop_long
}
