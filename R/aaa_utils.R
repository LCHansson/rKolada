# Global variables
utils::globalVariables(c("."))

#' Allowed entities: Kolada metadata classes
#'
#' @return A vector of names of allowed metadata entities, i.e. the correct
#' spelling of all allowed values of the \code{entity} parameter in \code{\link{get_metadata}}.
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
