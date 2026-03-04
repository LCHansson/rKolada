test_that("values_minimize() removes monotonous columns", {
  values_df <- tibble::tibble(
    kpi = c("N00001", "N00001"),
    municipality = c("Stockholm", "Gothenburg"),
    value = c(100, 200),
    year = c(2020, 2020),
    gender = c("T", "T")
  )
  result <- values_minimize(values_df)
  expect_true("kpi" %in% names(result))
  expect_true("municipality" %in% names(result))
  expect_true("value" %in% names(result))
  # "year" and "gender" are monotonous but "year" is not in the keep list
  # so it depends on n_distinct
  expect_false("gender" %in% names(result))
})

test_that("values_minimize() keeps columns with varying data", {
  values_df <- tibble::tibble(
    kpi = c("N00001", "N00001"),
    municipality = c("Stockholm", "Gothenburg"),
    value = c(100, 200),
    year = c(2019, 2020)
  )
  result <- values_minimize(values_df)
  expect_true("year" %in% names(result))
})

test_that("values_minimize() handles NULL input", {
  expect_warning(result <- values_minimize(NULL), "empty object")
  expect_null(result)
})

test_that("values_legend() creates legend string", {
  values_df <- tibble::tibble(
    kpi = c("N00001", "N00001", "N00002"),
    value = c(1, 2, 3)
  )
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002", "N00003"),
    title = c("KPI One", "KPI Two", "KPI Three")
  )
  result <- values_legend(values_df, kpi_df)
  expect_type(result, "character")
  expect_match(result, "N00001: KPI One")
  expect_match(result, "N00002: KPI Two")
})

test_that("values_legend() handles NULL input", {
  expect_warning(result <- values_legend(NULL, NULL), "empty object")
  expect_null(result)
})
