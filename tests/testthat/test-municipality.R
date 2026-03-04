test_that("municipality_name_to_id() converts names to ids", {
  munic_df <- tibble::tibble(
    id = c("0180", "1480", "1280"),
    title = c("Stockholm", "Gothenburg", "Malmo"),
    type = c("K", "K", "K")
  )
  result <- municipality_name_to_id(munic_df, "Stockholm")
  expect_equal(unname(result), "0180")
})

test_that("municipality_name_to_id() handles case insensitivity", {
  munic_df <- tibble::tibble(
    id = c("0180"),
    title = c("Stockholm"),
    type = c("K")
  )
  result <- municipality_name_to_id(munic_df, "stockholm")
  expect_equal(unname(result), "0180")
})

test_that("municipality_name_to_id() warns on unmatched names", {
  munic_df <- tibble::tibble(
    id = c("0180"),
    title = c("Stockholm"),
    type = c("K")
  )
  expect_warning(
    result <- municipality_name_to_id(munic_df, "NonExistent"),
    "not found"
  )
})

test_that("municipality_name_to_id() handles NULL input", {
  expect_warning(
    result <- municipality_name_to_id(NULL, "Stockholm"),
    "empty object"
  )
  expect_null(result)
})

test_that("municipality_id_to_name() converts ids to names", {
  munic_df <- tibble::tibble(
    id = c("0180", "1480", "1280"),
    title = c("Stockholm", "Gothenburg", "Malmo"),
    type = c("K", "K", "K")
  )
  result <- municipality_id_to_name(munic_df, "0180")
  expect_equal(unname(result), "Stockholm")
})

test_that("municipality_id_to_name() warns on unmatched ids", {
  munic_df <- tibble::tibble(
    id = c("0180"),
    title = c("Stockholm"),
    type = c("K")
  )
  expect_warning(
    result <- municipality_id_to_name(munic_df, "9999"),
    "not found"
  )
})

test_that("municipality_id_to_name() handles NULL input", {
  expect_warning(
    result <- municipality_id_to_name(NULL, "0180"),
    "empty object"
  )
  expect_null(result)
})

test_that("municipality_extract_ids() returns id vector", {
  munic_df <- tibble::tibble(
    id = c("0180", "1480"),
    title = c("Stockholm", "Gothenburg"),
    type = c("K", "K")
  )
  ids <- municipality_extract_ids(munic_df)
  expect_equal(ids, c("0180", "1480"))
})

test_that("municipality_extract_ids() handles NULL input", {
  expect_warning(
    result <- municipality_extract_ids(NULL),
    "empty object"
  )
  expect_null(result)
})
