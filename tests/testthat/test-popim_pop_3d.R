test_that("popim_pop_3d returns an object of the correct structure", {
    pop <- popim_pop_3d(region = "UK", year_min = 2000, year_max = 2005)

    expect_equal(names(pop), c("region_labels", "years_labels", "age_labels",
                               "vacc_data", "pop_data"))

    expect_equal(dim(pop$vacc_data), c(1, 6, 101))
    expect_equal(dim(pop$pop_data), dim(pop$vacc_data))

    expect_equal(pop$region_labels, dimnames(pop$vacc_data)$region)
    expect_equal(pop$years_labels |> as.character(),
                 dimnames(pop$vacc_data)$year)
    expect_equal(pop$age_labels |> as.character(), dimnames(pop$vacc_data)$age)
})

test_that("popim_pop_3d fails for invalid inputs", {
    expect_error(popim_pop_3d(2000, 2005))
    expect_error(popim_pop_3d("UK", 18, 10, 0, 10))
    expect_error(popim_pop_3d("UK", 2000:2005, 0, 100))
    expect_error(popim_pop_3d("UK", 2000, 2005, -1, 5))
})
