allowed_entities <- function() {
  c(
    "kpi",
    "kpi_groups",
    "municipality",
    "municipality_groups",
    "ou"
  )
}


`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}


stopwords <- function() {
  c("alla", "allt", "än", "är", "åt", "att", "av", "blev", "bli", "blir", "blivit", "då", "där", "de", "dem", "den", "denna", "deras", "dess", "dessa", "det", "detta", "dig", "din", "dina", "ditt", "du", "efter", "ej", "eller", "en", "er", "era", "ert", "ett", "för", "från", "ha", "hade", "han", "hans", "har", "här", "henne", "hennes", "hon", "honom", "hur", "i", "icke", "ingen", "inom", "inte", "jag", "ju", "kan", "kunde", "man", "med", "mellan", "men", "mig", "min", "mina", "mitt", "mot", "mycket", "någon", "något", "några", "när", "ni", "nu", "och", "om", "oss", "över", "på", "så", "sådan", "sådana", "sådant", "samma", "sedan", "sig", "sin", "sina", "sitta", "själv", "skulle", "som", "till", "under", "upp", "ut", "utan", "vad", "var", "vår", "vara", "våra", "varför", "varit", "varje", "vars", "vart", "vårt", "vem", "vi", "vid", "vilka", "vilkas", "vilken", "vilket")
}


cache_handler <- function(entity, cache) {

  storage <- switch(
    as.character(cache),
    no =, "FALSE" = "",
    tempfile =, "TRUE" =, yes = paste0(tempdir(), "/rkolada_", entity, "_cache_", Sys.Date(), ".RData"),
    wd = paste0(getwd(), "/rkolada_", entity, "_cache_", Sys.Date(), ".RData"),
    ""
  )

  # Special case: path to a folder
  if (is.character(cache) && dir.exists(cache))
    storage <- paste0(cache, "/rkolada_", entity, "_cache_", Sys.Date(), ".RData")

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
