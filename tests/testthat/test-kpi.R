test_that("kpi_minimize() removes monotonous columns but keeps core columns", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002"),
    title = c("KPI 1", "KPI 2"),
    description = c("desc1", "desc2"),
    auspice = c("E", "E")
  )
  result <- kpi_minimize(kpi_df, remove_monotonous_data = TRUE)
  expect_false("auspice" %in% names(result))
  expect_true(all(c("id", "title", "description") %in% names(result)))
})

test_that("kpi_minimize() keeps non-monotonous columns", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002"),
    title = c("KPI 1", "KPI 2"),
    description = c("desc1", "desc2"),
    has_ou_data = c(0, 0),
    municipality_type = c("K", "L")
  )
  result <- kpi_minimize(kpi_df, remove_monotonous_data = TRUE)
  expect_false("has_ou_data" %in% names(result))
  expect_true("municipality_type" %in% names(result))
})

test_that("kpi_minimize() preserves id, title, description even if monotonous", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00001"),
    title = c("Same", "Same"),
    description = c("Same", "Same"),
    extra = c("a", "a")
  )
  result <- kpi_minimize(kpi_df, remove_monotonous_data = TRUE)
  expect_true(all(c("id", "title", "description") %in% names(result)))
})

test_that("kpi_minimize() handles NULL input", {
  expect_warning(result <- kpi_minimize(NULL), "empty object")
  expect_null(result)
})

test_that("kpi_extract_ids() returns id vector", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002", "N00003"),
    title = c("A", "B", "C")
  )
  ids <- kpi_extract_ids(kpi_df)
  expect_equal(ids, c("N00001", "N00002", "N00003"))
})

test_that("kpi_extract_ids() handles NULL input", {
  expect_warning(result <- kpi_extract_ids(NULL), "empty object")
  expect_null(result)
})

test_that("kpi_bind_keywords() adds keyword columns in wide form", {
  kpi_df <- tibble::tibble(
    id = c("N00001"),
    title = c("Kostnad skola elev"),
    description = c("desc")
  )
  result <- kpi_bind_keywords(kpi_df, n = 2, form = "wide")
  expect_true("keyword_1" %in% names(result))
  expect_true("keyword_2" %in% names(result))
})

test_that("kpi_bind_keywords() handles NULL input", {
  expect_warning(result <- kpi_bind_keywords(NULL), "empty object")
  expect_null(result)
})

test_that("kpi_describe() handles NULL input", {
  expect_warning(result <- kpi_describe(NULL), "empty object")
  expect_null(result)
})
