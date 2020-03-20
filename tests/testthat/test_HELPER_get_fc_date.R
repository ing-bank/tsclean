
context("get_fc_date")

test_that("check get_fc_date for correctly loading README", {
  for (i in 1:12) {
    function_output <- get_fc_date(
      misob_year = 2018,
      scenario = month.name[i]
    )
    expect_true(is.numeric(function_output))
    expect_true(nchar(function_output) == 6)
    expect_true(function_output > 201800)
    expect_true(function_output < 201813)
  }
  function_output <- get_fc_date(
    misob_year = 2018,
    scenario = "Forecast potato"
  )
  expect_equal(function_output, NA_real_)
})

test_that("check get_fc_date with invalid inputs", {
  expect_error(
    get_fc_date(
      misob_year = "potato"
    )
  )
  expect_error(
    get_fc_date(
      misob_year = 42
    )
  )
  expect_error(
    get_fc_date(
      misob_year = 12345
    )
  )
  expect_error(
    get_fc_date(
      misob_year = 2018,
      scenario = ""
    )
  )
  expect_error(
    get_fc_date(
      misob_year = 2018,
      scenario = 42
    )
  )
  expect_error(
    get_fc_date(
      misob_year = 2018,
      scenario = list()
    )
  )
})
