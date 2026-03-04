test_that("allowed_entities() returns expected entity names", {
  ents <- allowed_entities()
  expect_type(ents, "character")
  expect_true("kpi" %in% ents)
  expect_true("municipality" %in% ents)
  expect_true("ou" %in% ents)
  expect_true("kpi_groups" %in% ents)
  expect_true("municipality_groups" %in% ents)
})

test_that("append_query_params() adds params to URL without existing query", {
  url <- rKolada:::append_query_params("https://example.com/path", page = 1, per_page = 100)
  expect_match(url, "\\?page=1")
  expect_match(url, "per_page=100")
})

test_that("append_query_params() adds params to URL with existing query", {
  url <- rKolada:::append_query_params("https://example.com/path?title=test", page = 1)
  expect_match(url, "\\?title=test&page=1")
})

test_that("append_query_params() drops NA params", {
  url <- rKolada:::append_query_params("https://example.com/path", page = NA, per_page = 100)
  expect_no_match(url, "(\\?|&)page=")
  expect_match(url, "per_page=100")
})

test_that("append_query_params() drops NULL params", {
  url <- rKolada:::append_query_params("https://example.com/path", page = NULL, per_page = 100)
  expect_no_match(url, "(\\?|&)page=")
  expect_match(url, "per_page=100")
})

test_that("append_query_params() returns original URL when all params are NA", {
  url <- rKolada:::append_query_params("https://example.com/path", page = NA)
  expect_equal(url, "https://example.com/path")
})

test_that("cache_handler() returns identity when cache is FALSE", {
  ch <- rKolada:::cache_handler("test", FALSE, tempdir)
  expect_false(ch("discover"))
  df <- tibble::tibble(x = 1:3)
  expect_identical(ch("store", df), df)
})

test_that("stopwords() returns character vector", {
  sw <- rKolada:::stopwords()
  expect_type(sw, "character")
  expect_true(length(sw) > 0)
})

# --- chunk_vector() ---

test_that("chunk_vector() returns list(NULL) for NULL input", {
  expect_equal(rKolada:::chunk_vector(NULL), list(NULL))
})

test_that("chunk_vector() passes through short vectors unchanged", {
  expect_equal(rKolada:::chunk_vector(1:5), list(1:5))
  expect_equal(rKolada:::chunk_vector(1:25), list(1:25))
})

test_that("chunk_vector() splits vectors exceeding max_size", {
  chunks <- rKolada:::chunk_vector(1:60)
  expect_length(chunks, 3)
  expect_equal(unname(lengths(chunks)), c(25, 25, 10))
  expect_equal(unlist(chunks, use.names = FALSE), 1:60)
})

test_that("chunk_vector() respects custom max_size", {
  chunks <- rKolada:::chunk_vector(1:10, max_size = 3)
  expect_length(chunks, 4)
  expect_equal(unname(lengths(chunks)), c(3, 3, 3, 1))
})

test_that("chunk_vector() handles exact multiple of max_size", {
  chunks <- rKolada:::chunk_vector(1:50)
  expect_length(chunks, 2)
  expect_equal(unname(lengths(chunks)), c(25, 25))
})
