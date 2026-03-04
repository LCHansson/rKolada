# Mocked API tests using httptest2
# Fixtures are pre-recorded v3 API responses

# Helper to create mock responses inline
mock_json_response <- function(values, next_url = NULL) {
  body <- list(values = values)
  if (!is.null(next_url)) body$next_url <- next_url
  jsonlite::toJSON(body, auto_unbox = TRUE)
}

test_that("get_kpi() returns tibble from mocked response", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(
        status_code = 200L,
        body = charToRaw(mock_json_response(
          data.frame(
            id = c("N00001", "N00002"),
            title = c("KPI One", "KPI Two"),
            description = c("Desc 1", "Desc 2"),
            auspice = c("X", "E"),
            has_ou_data = c(0, 1),
            is_divided_by_gender = c(FALSE, TRUE),
            municipality_type = c("K", "K"),
            operating_area = c("Area 1", "Area 2"),
            publication_date = c("2020-01-01", "2020-01-01"),
            publ_period = c("2020", "2020"),
            stringsAsFactors = FALSE
          )
        ))
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- get_kpi(max_results = 2)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("id" %in% names(result))
  expect_true("title" %in% names(result))
  expect_true("auspice" %in% names(result))
})

test_that("get_municipality() returns tibble from mocked response", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(
        status_code = 200L,
        body = charToRaw(mock_json_response(
          data.frame(
            id = c("0180", "1480"),
            title = c("Stockholm", "Gothenburg"),
            type = c("K", "K"),
            stringsAsFactors = FALSE
          )
        ))
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- get_municipality(max_results = 2)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("type" %in% names(result))
})

test_that("get_metadata() handles connection errors gracefully", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      stop("Connection refused")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- get_metadata("kpi", max_results = 1),
    "Could not connect"
  )
  expect_null(result)
})

test_that("get_metadata() handles malformed JSON gracefully", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(
        status_code = 200L,
        body = charToRaw("not valid json{{{")
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_warning(
    result <- get_metadata("kpi", max_results = 1),
    "malformatted"
  )
  expect_null(result)
})

test_that("get_values() chunks >25 KPIs into separate requests", {
  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      # Return a minimal valid response for each chunk
      kpi_id <- if (call_count == 1L) "N00001" else "N00026"
      body <- jsonlite::toJSON(list(
        values = data.frame(
          kpi = kpi_id,
          municipality = "0180",
          period = 2020L,
          values = I(list(data.frame(
            gender = "T", status = "", value = call_count
          ))),
          stringsAsFactors = FALSE
        )
      ), auto_unbox = TRUE)
      structure(list(
        status_code = 200L,
        body = charToRaw(body)
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )

  kpis <- paste0("N", sprintf("%05d", 1:30))
  result <- get_values(kpi = kpis, municipality = "0180", simplify = FALSE)

  expect_s3_class(result, "tbl_df")
  # Two chunks: 25 + 5 = 2 API calls
  expect_equal(call_count, 2L)
  expect_equal(nrow(result), 2)
})

test_that("get_metadata() chunks >25 IDs into separate requests", {
  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      body <- jsonlite::toJSON(list(
        values = data.frame(
          id = paste0("N", sprintf("%05d", call_count)),
          title = paste("KPI", call_count),
          stringsAsFactors = FALSE
        )
      ), auto_unbox = TRUE)
      structure(list(
        status_code = 200L,
        body = charToRaw(body)
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )

  ids <- paste0("N", sprintf("%05d", 1:30))
  result <- get_metadata("kpi", id = ids)

  expect_s3_class(result, "tbl_df")
  expect_equal(call_count, 2L)
  expect_equal(nrow(result), 2)
})

test_that("get_values() does not chunk when <=25 params", {
  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      body <- jsonlite::toJSON(list(
        values = data.frame(
          kpi = "N00001",
          municipality = "0180",
          period = 2020L,
          values = I(list(data.frame(
            gender = "T", status = "", value = 1
          ))),
          stringsAsFactors = FALSE
        )
      ), auto_unbox = TRUE)
      structure(list(
        status_code = 200L,
        body = charToRaw(body)
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )

  kpis <- paste0("N", sprintf("%05d", 1:25))
  result <- get_values(kpi = kpis, municipality = "0180", simplify = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(call_count, 1L)
})

test_that("get_values() handles empty results gracefully", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(
        status_code = 200L,
        body = charToRaw(jsonlite::toJSON(list(values = list()), auto_unbox = TRUE))
      ), class = "httr2_response")
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_warning(
    result <- get_values(kpi = "N00001", municipality = "0180"),
    "zero hits"
  )
  expect_null(result)
})
