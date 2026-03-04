test_that("kpi_search() filters by title", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002", "N00003"),
    title = c("Befolkning totalt", "Kostnad skola", "Inkomst totalt"),
    description = c("desc1", "desc2", "desc3")
  )
  result <- kpi_search(kpi_df, "totalt")
  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c("N00001", "N00003")))
})

test_that("kpi_search() filters by specific column", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002"),
    title = c("Befolkning", "Kostnad"),
    description = c("total kostnad", "skola")
  )
  result <- kpi_search(kpi_df, "kostnad", column = "title")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "N00002")
})

test_that("kpi_search() is case insensitive", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002"),
    title = c("Befolkning", "KOSTNAD")
  )
  result <- kpi_search(kpi_df, "kostnad")
  expect_equal(nrow(result), 1)
})

test_that("kpi_search() handles NULL input", {
  expect_warning(result <- kpi_search(NULL, "test"), "empty object")
  expect_null(result)
})

test_that("kpi_search() handles multiple query terms", {
  kpi_df <- tibble::tibble(
    id = c("N00001", "N00002", "N00003"),
    title = c("Befolkning", "Kostnad", "Inkomst")
  )
  result <- kpi_search(kpi_df, c("befolkning", "inkomst"))
  expect_equal(nrow(result), 2)
})

test_that("municipality_search() filters by title", {
  munic_df <- tibble::tibble(
    id = c("0180", "1480", "1280"),
    title = c("Stockholm", "Gothenburg", "Malmo"),
    type = c("K", "K", "K")
  )
  result <- municipality_search(munic_df, "Stockholm")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "0180")
})

test_that("municipality_search() filters by type column", {
  munic_df <- tibble::tibble(
    id = c("0180", "01", "1480"),
    title = c("Stockholm", "Region Stockholm", "Gothenburg"),
    type = c("K", "L", "K")
  )
  result <- municipality_search(munic_df, "K", column = "type")
  expect_equal(nrow(result), 2)
})

test_that("municipality_search() handles NULL input", {
  expect_warning(result <- municipality_search(NULL, "test"), "empty object")
  expect_null(result)
})

test_that("ou_search() filters by title", {
  ou_df <- tibble::tibble(
    id = c("V1", "V2"),
    title = c("Skola A", "Forskola B"),
    municipality = c("Stockholm", "Malmo"),
    municipality_id = c("0180", "1280")
  )
  result <- ou_search(ou_df, "Skola")
  expect_equal(nrow(result), 2)
})

test_that("ou_search() handles NULL input", {
  expect_warning(result <- ou_search(NULL, "test"), "empty object")
  expect_null(result)
})
