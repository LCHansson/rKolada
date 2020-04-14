#' Description specifications in {glue} format
#' @param entity The metadata entity to be described.
desc_glue_spec <- function(entity) {
  # {paste0(paste(rep("#", as.integer(options("width"))), collapse = ""), "\n")}

  switch(entity,
         kpi = '
#### {id}: {title}

##### Description
{description}

##### Metadata
- Has OU data: {as.logical(has_ou_data)}
- Divided by gender: {as.logical(is_divided_by_gender)}
- Municipality type: {municipality_type}
- Operating area: {operating_area}

- Publication date: {publication_date}
- Publication period: {publ_period}
- OU publication date: {ou_publication_date}

##### Keywords
{keywords}

',
         kpi_group = '
#### {id}: {title}

#####  Group contains {num_members} KPIs:
{member_data}

'
  )
}


safely_transformer <- function(otherwise = NA) {
  function(text, envir) {
    tryCatch(
      eval(parse(text = text, keep.source = FALSE), envir),
      error = function(e) if (is.language(otherwise)) eval(otherwise) else otherwise)
  }
}

glue_data_safely <- function(..., .otherwise = NA, .envir = parent.frame()) {
  glue::glue_data(..., .transformer = safely_transformer(.otherwise), .envir = .envir)
}