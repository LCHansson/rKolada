# Description specifications in {glue} format
# @param type The metadata entity to be described.
desc_glue_spec <- function(type) {

  switch(type,
         nogroup = '
{heading_prefix}{id}: {title}

{sub_heading_prefix} Description
{description}

{sub_heading_prefix} Metadata
- Has OU data: {as.logical(has_ou_data)}
- Divided by gender: {as.logical(is_divided_by_gender)}
- Municipality type: {municipality_type}
- Operating area: {operating_area}

- Publication date: {publication_date}
- Publication period: {publ_period}
- OU publication date: {ou_publication_date}

{sub_heading_prefix} Keywords
{keywords}

',
         group = '
{heading_prefix}{id}: {title}

{sub_heading_prefix} Group contains {num_members} {entity} entities:
{member_data}

'
  )
}


safely_transformer <- function(otherwise = NA) {
  function(text, envir) {
    tryCatch(
      eval(parse(text = text, keep.source = FALSE), envir),
      error = function(e)
        if (is.language(otherwise)) eval(otherwise) else otherwise
    )
  }
}

glue_data_safely <- function(
  .x, spec, .entity, .format, .heading_length = 2, .sub_heading_length = 3,
  .otherwise = NA, .envir = parent.frame()
) {
  .x$heading_prefix <- switch(
    .format,
    inline = paste(c(rep("#", options("width")), "\n"), collapse = ""),
    md = paste(c(rep("#", .heading_length), " "), collapse = "")
  )
  .x$sub_heading_prefix <- paste(rep("#", .sub_heading_length), collapse = "")
  .x$entity <- .entity

  print(glue::glue_data(
    .x, spec,
    .transformer = safely_transformer(.otherwise), .envir = .envir
  ))
  return(invisible(.x))
}