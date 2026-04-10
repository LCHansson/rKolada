# Global variables
utils::globalVariables(c("."))

# Resolve lang parameter, falling back to option. Returns "SV" or "EN".
resolve_lang <- function(lang = NULL) {
  lang <- lang %||% getOption("rKolada.lang", "SV")
  lang <- toupper(lang)
  if (!lang %in% c("SV", "EN")) {
    cli::cli_abort('{.arg lang} must be {.val SV} or {.val EN}.')
  }
  lang
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Append query parameters to a URL string, replacing urltools::param_set().
# Drops any params whose value is NA or NULL.
append_query_params <- function(url, ...) {
  params <- list(...)
  params <- params[!vapply(params, function(x) is.null(x) || all(is.na(x)), logical(1))]
  if (length(params) == 0) return(url)

  has_query <- grepl("\\?", url)
  param_strings <- paste0(
    names(params), "=",
    vapply(params, as.character, character(1))
  )
  query_string <- paste(param_strings, collapse = "&")

  if (has_query)
    paste0(url, "&", query_string)
  else
    paste0(url, "?", query_string)
}

# Internal helper for *_search() functions. Replaces deprecated filter_at().
entity_search <- function(df, query, column, caller_name) {
  if (is.null(df)) {
    cli::cli_warn("An empty object was used as input to {.fn {caller_name}}.")
    return(NULL)
  }

  if (is.null(column))
    column <- names(df)

  pattern <- tolower(as.character(paste(query, collapse = "|")))

  df |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(column),
        ~ stringr::str_detect(tolower(.x), pattern)
      )
    )
}

#' Allowed entities: Kolada metadata classes
#'
#' @return A vector of names of allowed metadata entities, i.e. the correct
#' spelling of all allowed values of the `entity` parameter in [get_metadata()].
#'
#' @export
allowed_entities <- function() {
  c(
    "kpi",
    "kpi_groups",
    "municipality",
    "municipality_groups",
    "ou"
  )
}


# Split a vector into chunks of at most max_size elements.
# Returns a list of vectors. NULL and short inputs pass through as a single-element list.
chunk_vector <- function(x, max_size = 25) {
  if (is.null(x) || length(x) <= max_size) return(list(x))
  split(x, ceiling(seq_along(x) / max_size))
}

# Custom stop words for keyword detection
stopwords <- function() {
  c("alla", "allt", "\u00e4n", "\u00e4r", "\u00e5t", "att", "av", "blev", "bli",
    "blir", "blivit", "d\u00e5", "d\u00e4r", "de", "dem", "den", "denna",
    "deras", "dess", "dessa", "det", "detta", "dig", "din", "dina", "ditt",
    "du", "efter", "ej", "eller", "en", "enl", "er", "era", "ert", "ett",
    "f\u00f6r", "fr\u00e5n", "ha", "hade", "han", "hans", "har", "h\u00e4r",
    "henne", "hennes", "hon", "honom", "hur", "i", "icke", "ingen", "inom",
    "inte", "jag", "ju", "kan", "kunde", "man", "med", "mellan", "men", "mig",
    "min", "mina", "mitt", "mot", "mycket", "n\u00e5gon", "n\u00e5got",
    "n\u00e5gra", "n\u00e4r", "ni", "nu", "och", "om", "oss", "\u00f6ver",
    "p\u00e5", "s\u00e5", "s\u00e5dan", "s\u00e5dana", "s\u00e5dant", "samma",
    "sedan", "sig", "sin", "sina", "sitta", "sj\u00e4lv", "skulle", "som",
    "till", "under", "upp", "ut", "utan", "vad", "var", "v\u00e5r", "vara",
    "v\u00e5ra", "varf\u00f6r", "varit", "varje", "vars", "vart", "v\u00e5rt",
    "vem", "vi", "vid", "vilka", "vilkas", "vilken", "vilket")
}

# Internal HTTP helper with retry and error handling.
# Returns the parsed JSON response or NULL on failure.
kolada_get <- function(url, max_retries = 3) {
  for (i in seq_len(max_retries + 1)) {
    res <- tryCatch(
      httr2::request(url) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_timeout(30) |>
        httr2::req_perform(),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      cli::cli_warn(c(
        "Could not connect to the Kolada database.",
        "i" = "Please check your internet connection.",
        "i" = "URL: {.url {url}}"
      ))
      return(NULL)
    }

    status <- httr2::resp_status(res)

    if (status == 429 && i <= max_retries) {
      wait <- 2^i
      cli::cli_inform("Rate limited (HTTP 429). Retrying in {wait}s...")
      Sys.sleep(wait)
      next
    }

    body_str <- httr2::resp_body_string(res)
    parsed <- tryCatch(jsonlite::fromJSON(body_str), error = function(e) e)

    if (inherits(parsed, "error")) {
      cli::cli_warn(c(
        "Kolada returned malformatted HTML/JSON.",
        "i" = "URL: {.url {url}}"
      ))
      return(NULL)
    }

    return(parsed)
  }

  cli::cli_warn("Max retries exceeded for {.url {url}}.")
  NULL
}

#' Get the rKolada cache directory
#'
#' Returns the path to the persistent cache directory used by rKolada.
#' The directory is created if it does not exist.
#'
#' @return A string containing the path to the cache directory.
#'
#' @export
kolada_cache_dir <- function() {
  dir <- tools::R_user_dir("rKolada", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#' Clear the rKolada cache
#'
#' Removes all cached files from the rKolada cache directory.
#'
#' @param cache_dir The cache directory to clear. Defaults to
#'   [kolada_cache_dir()].
#'
#' @return Invisibly returns the cache directory path.
#'
#' @export
kolada_clear_cache <- function(cache_dir = kolada_cache_dir()) {
  files <- list.files(cache_dir, full.names = TRUE)
  if (length(files) > 0) {
    file.remove(files)
    cli::cli_inform("Removed {length(files)} cached file{?s} from {.path {cache_dir}}.")
  } else {
    cli::cli_inform("Cache directory is already empty.")
  }
  invisible(cache_dir)
}

# Cache handler factory
#
# @param entity String; a keyword describing the data entity handled by the
# cache handler.
# @param cache Logical; whether data should be stored.
# @param cache_location String; where data should be stored (a path to a
# directory or a function that returns a path to a directory).
cache_handler <- function(entity, cache, cache_location) {

  if(is.function(cache_location))
    cache_location <- cache_location()

  storage <- switch(
    as.character(isTRUE(cache)),
    "FALSE" = "",
    "TRUE" = paste0(cache_location, "/rkolada_", entity,
                    "_cache_", Sys.Date(), ".RData")
  )

  if (storage == "")
    return(function(method, df) {
      if (method == "store")
        return(df)
      else
        return(FALSE)
    })

  function(method, df) {
    switch(method,
           discover = { file.exists(storage) },
           load = { load(file = storage, envir = environment()); return(df) },
           store = { save(df, file = storage); return(df) },
           NULL
    )
  }
}
