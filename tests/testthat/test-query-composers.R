test_that("compose_metadata_query() produces v3 HTTPS URL", {
  url <- compose_metadata_query("kpi")
  expect_match(url, "^https://api\\.kolada\\.se/v3/kpi")
})

test_that("compose_metadata_query() includes title as query param", {
  url <- compose_metadata_query("kpi", title = "test")
  expect_match(url, "\\?title=test")
})

test_that("compose_metadata_query() includes id in path", {
  url <- compose_metadata_query("kpi", id = "N00001")
  expect_match(url, "/kpi/N00001")
})

test_that("compose_metadata_query() includes multiple ids", {
  url <- compose_metadata_query("kpi", id = c("N00001", "N00002"))
  expect_match(url, "/kpi/N00001,N00002")
})

test_that("compose_metadata_query() includes page and per_page", {
  url <- compose_metadata_query("kpi", page = 2, per_page = 100)
  expect_match(url, "page=2")
  expect_match(url, "per_page=100")
})

test_that("compose_metadata_query() includes municipality for ou entity", {
  url <- compose_metadata_query("ou", municipality = "0180")
  expect_match(url, "municipality=0180")
})

test_that("compose_metadata_query() includes region_type for municipality entity", {
  url <- compose_metadata_query("municipality", region_type = "K")
  expect_match(url, "region_type=K")
})

test_that("compose_metadata_query() ignores region_type for non-municipality entity", {
  url <- compose_metadata_query("kpi", region_type = "K")
  expect_no_match(url, "region_type")
})

test_that("compose_metadata_query() errors on invalid entity", {
  expect_error(compose_metadata_query("invalid_entity"))
})

test_that("compose_metadata_query() errors on NULL entity", {
  expect_error(compose_metadata_query(entity = NULL))
})

test_that("compose_data_query() produces v3 HTTPS URL", {
  url <- compose_data_query(kpi = "N00001", municipality = "0180")
  expect_match(url, "^https://api\\.kolada\\.se/v3/data")
})

test_that("compose_data_query() builds correct path segments", {
  url <- compose_data_query(kpi = "N00001", municipality = "0180", period = 2020)
  expect_match(url, "/kpi/N00001")
  expect_match(url, "/municipality/0180")
  expect_match(url, "/year/2020")
})

test_that("compose_data_query() uses oudata for ou unit_type", {
  url <- compose_data_query(kpi = "N00001", ou = "V15E144001101", unit_type = "ou")
  expect_match(url, "/v3/oudata")
  expect_match(url, "/ou/V15E144001101")
})

test_that("compose_data_query() includes from_date", {
  url <- compose_data_query(kpi = "N00001", municipality = "0180", from_date = "2020-01-01")
  expect_match(url, "from_date=2020-01-01")
})

test_that("compose_data_query() errors with too few params", {
  expect_error(compose_data_query(kpi = "N00001"))
})

test_that("compose_data_query() errors on invalid unit_type", {
  expect_error(compose_data_query(kpi = "N00001", municipality = "0180", unit_type = "bad"))
})
