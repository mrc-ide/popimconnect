test_that("convert_pop_to_long fails for invalid inputs", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- popim::popim_population(region = my_regions,
                              year_min = year_min, year_max = year_max,
                              age_min = age_min, age_max = age_max)
    expect_error(convert_pop_to_long(pl))


    pw <- popim_pop_3d(region = my_regions,
                     year_min = year_min, year_max = year_max,
                     age_min = age_min, age_max = age_max)

    class(pw) <- "data.frame" ## stripping the popim_population class attribute

    expect_error(convert_pop_to_long(pw))
})

test_that("converts a `popim_pop_3d`-class object initialised to 0 correctly", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- popim::popim_population(region = my_regions,
                         year_min = year_min, year_max = year_max,
                         age_min = age_min, age_max = age_max)

    pw <- popim_pop_3d(region = my_regions,
                     year_min = year_min, year_max = year_max,
                     age_min = age_min, age_max = age_max)

    expect_equal(pl, convert_pop_to_long(pw)) 
})

test_that("successive conversion from long to 3d to long gives the original", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- popim::popim_population(region = my_regions,
                              year_min = year_min, year_max = year_max,
                              age_min = age_min, age_max = age_max)

    pl$immunity <- (1:length(pl$region))/100
    pl$pop_size <- 1:length(pl$region)

    pw <- convert_pop_to_3d(pl)
    pl_out <- convert_pop_to_long(pw)

    expect_equal(pl, pl_out)
})
