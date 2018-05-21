context("Test functions to make Gadget files")

test_that("make_gadget_timefile produces correct output under different scenarios", {
    quarterly_time <-
        list(firstyear = 1985, firststep = 1, lastyear = 2015, laststep = 4,
             notimesteps = c(4,3,3,3,3))
    class(quarterly_time) <- c("gadget.time", "list")
    biannual_time <-
        modifyList(quarterly_time,
                   list(laststep = 2, notimesteps = c(2, 6, 6)))
    annual_time <-
        modifyList(quarterly_time,
                   list(laststep = 1, notimesteps = c(1,12)))
    monthly_time <-
        modifyList(quarterly_time,
                   list(laststep = 12, notimesteps = c(12, rep(1,12))))
    custom_time <-
        modifyList(quarterly_time,
                   list(notimesteps = c(4, 2, 4, 4, 2)))
    expect_equal(make_gadget_timefile(1985, 2015, "quarterly"), quarterly_time)
    expect_equal(make_gadget_timefile(1985, 2015, "annually"), annual_time)
    expect_equal(make_gadget_timefile(1985, 2015, "biannual"), biannual_time)
    expect_equal(make_gadget_timefile(1985, 2015, "monthly"), monthly_time)
    expect_equal(make_gadget_timefile(1985, 2015, timesteps = c(4, 2, 4, 4, 2)), custom_time)
})

test_that("make_gadget_areafile returns the correct output", {
    temp_data <-
        expand.grid(year = 1:5, step = 1:4,
                    area = 1, mean = 3)
    area_list <-
        list(areas = 1, size = 100, temperature = temp_data)
    class(area_list) <- c("gadget.area", "list")
    temp2_data <-
        expand.grid(year = 1:2, step = 1:4,
                    area = 1:2, mean = 3)
    two_areas <-
        list(areas = 1:2, size = c(100, 150), temperature = temp2_data)
    class(two_areas) <- c("gadget.area", "list")
    expect_equal(make_gadget_areafile(1, 100, temp_data), area_list)
    expect_equal(make_gadget_areafile(1:2, c(100, 150), temp2_data), two_areas)

})
