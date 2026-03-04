test_that("kpi_grp_unnest() expands group members", {
  kpi_grp_df <- tibble::tibble(
    id = c("G1"),
    title = c("Test group"),
    members = list(tibble::tibble(
      member_id = c("N00001", "N00002"),
      member_title = c("KPI 1", "KPI 2")
    ))
  )
  result <- kpi_grp_unnest(kpi_grp_df)
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "title", "group_id", "group_title") %in% names(result)))
  expect_equal(result$id, c("N00001", "N00002"))
})

test_that("kpi_grp_extract_ids() extracts member ids", {
  kpi_grp_df <- tibble::tibble(
    id = c("G1"),
    title = c("Test group"),
    members = list(tibble::tibble(
      member_id = c("N00001", "N00002"),
      member_title = c("KPI 1", "KPI 2")
    ))
  )
  ids <- kpi_grp_extract_ids(kpi_grp_df)
  expect_equal(ids, c("N00001", "N00002"))
})

test_that("kpi_grp_search() filters by title", {
  kpi_grp_df <- tibble::tibble(
    id = c("G1", "G2"),
    title = c("Ekonomi grupp", "Utbildning grupp"),
    members = list(
      tibble::tibble(member_id = "N1", member_title = "A"),
      tibble::tibble(member_id = "N2", member_title = "B")
    )
  )
  result <- kpi_grp_search(kpi_grp_df, "ekonomi")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "G1")
})

test_that("municipality_grp_unnest() expands group members", {
  munic_grp_df <- tibble::tibble(
    id = c("G1"),
    title = c("Test group"),
    members = list(tibble::tibble(
      member_id = c("0180", "1480"),
      member_title = c("Stockholm", "Gothenburg")
    ))
  )
  result <- municipality_grp_unnest(munic_grp_df)
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "title", "group_id", "group_title") %in% names(result)))
})

test_that("municipality_grp_unnest() handles NULL input", {
  expect_warning(
    result <- municipality_grp_unnest(NULL),
    "empty object"
  )
  expect_null(result)
})

test_that("municipality_grp_extract_ids() extracts member ids", {
  munic_grp_df <- tibble::tibble(
    id = c("G1"),
    title = c("Test group"),
    members = list(tibble::tibble(
      member_id = c("0180", "1480"),
      member_title = c("Stockholm", "Gothenburg")
    ))
  )
  ids <- municipality_grp_extract_ids(munic_grp_df)
  expect_equal(ids, c("0180", "1480"))
})

test_that("municipality_grp_extract_ids() handles NULL input", {
  expect_warning(
    result <- municipality_grp_extract_ids(NULL),
    "empty object"
  )
  expect_null(result)
})

test_that("municipality_grp_search() filters by title", {
  munic_grp_df <- tibble::tibble(
    id = c("G1", "G2"),
    title = c("Storstader", "Glesbygd"),
    members = list(
      tibble::tibble(member_id = "0180", member_title = "Stockholm"),
      tibble::tibble(member_id = "2505", member_title = "Arvidsjaur")
    )
  )
  result <- municipality_grp_search(munic_grp_df, "storstader")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "G1")
})
