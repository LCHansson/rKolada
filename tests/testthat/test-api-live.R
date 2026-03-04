# Live API integration tests
# These tests require internet access and are skipped offline

test_that("kolada_available() returns TRUE when API is up", {
  skip_if_not(kolada_available(), "Kolada API not available")
  expect_true(kolada_available())
})

test_that("get_kpi() returns valid tibble from live API", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_kpi(max_results = 5)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("id" %in% names(result))
  expect_true("title" %in% names(result))
  # v3 schema: auspice (not auspices)
  expect_true("auspice" %in% names(result))
})

test_that("get_municipality() returns valid tibble from live API", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_municipality(max_results = 5)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("type" %in% names(result))
})

test_that("get_municipality() with region_type filters correctly", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_municipality(region_type = "L", max_results = 50)
  expect_s3_class(result, "tbl_df")
  # All returned rows should be of type "L"
  if (nrow(result) > 0 && "type" %in% names(result)) {
    expect_true(all(result$type == "L"))
  }
})

test_that("get_values() returns data for known KPI", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_values(
    kpi = "N00003",
    municipality = "0180",
    period = 2021
  )
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("value" %in% names(result))
  expect_true(is.numeric(result$value))
})

test_that("get_values() with from_date parameter works", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_values(
    kpi = "N00003",
    municipality = "0180",
    period = 2021,
    from_date = "2020-01-01"
  )
  expect_s3_class(result, "tbl_df")
})

test_that("get_kpi_groups() returns valid tibble", {
  skip_if_not(kolada_available(), "Kolada API not available")
  result <- get_kpi_groups(max_results = 5)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("members" %in% names(result))
})

test_that("compose_metadata_query() produces working v3 URLs", {
  skip_if_not(kolada_available(), "Kolada API not available")
  url <- compose_metadata_query("kpi", per_page = 1)
  expect_match(url, "v3")
  # Actually fetch to verify URL works
  resp <- httr2::request(url) |> httr2::req_perform()
  expect_equal(httr2::resp_status(resp), 200L)
})
