test_that("validator validates a good object correctly", {

    pop <- vip_pop_3d(region = "UK", year_min = 2000, year_max = 2003,
                      age_min = 0, age_max = 5)
    expect_identical(validate_vip_pop_3d(pop), pop)
    expect_warning(validate_vip_pop_3d(pop)) ## because the population
                                             ## data is all missing.

    pop <- vip_pop_3d(region = c("UK", "FRA"), year_min = 2000, year_max = 2003,
                      age_min = 0, age_max = 5)
    pop$pop_data[,,] = 0
    expect_identical(validate_vip_pop_3d(pop), pop)
    
})

test_that("validator throws an error for a bad object", {
    ## wrong class:
    pop <- vip_pop_3d(region = "UK", year_min = 2000, year_max = 2003,
                      age_min = 0, age_max = 5)
    class(pop) <- "not a list"
    expect_error(validate_vip_pop_3d(pop))

    ## invalid data in vacc_data:
    pop <- vip_pop_3d(region = "UK", year_min = 2000, year_max = 2003,
                      age_min = 0, age_max = 5)
    pop$vacc_data[1,1,1] <- 2
    expect_error(validate_vip_pop_3d(pop))

})
