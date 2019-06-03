test_that("The mongo database loads", {
    db <- getDb()
    expect_true('roGFP1' %in% db$find()$'sensor_name')
})
