# Description format function (replaces glue_data templates)
# @param type The metadata entity to be described ("nogroup" or "group").
# @param row A single-row list with fields to interpolate.
# @param heading_prefix Prefix for headings.
# @param sub_heading_prefix Prefix for sub-headings.
# @param entity Entity type label (e.g. "KPI", "Municipality").
desc_format <- function(type, row, heading_prefix, sub_heading_prefix, entity) {

  # Safely extract a field, returning "Unknown" if missing or error
  safe_get <- function(name, otherwise = "Unknown") {
    val <- tryCatch(row[[name]], error = function(e) otherwise)
    if (is.null(val) || length(val) == 0 || all(is.na(val))) otherwise else val
  }

  if (type == "nogroup") {
    paste0(
      "\n", heading_prefix, safe_get("id"), ": ", safe_get("title"), "\n",
      "\n", sub_heading_prefix, " Description\n",
      safe_get("description"), "\n",
      "\n", sub_heading_prefix, " Metadata\n",
      "- Has OU data: ", as.logical(safe_get("has_ou_data")), "\n",
      "- Divided by gender: ", as.logical(safe_get("is_divided_by_gender")), "\n",
      "- Municipality type: ", safe_get("municipality_type"), "\n",
      "- Operating area: ", safe_get("operating_area"), "\n",
      "- Auspice: ", safe_get("auspice"), "\n",
      "\n",
      "- Publication date: ", safe_get("publication_date"), "\n",
      "- Publication period: ", safe_get("publ_period"), "\n",
      "\n", sub_heading_prefix, " Keywords\n",
      safe_get("keywords"), "\n"
    )
  } else if (type == "group") {
    paste0(
      "\n", heading_prefix, safe_get("id"), ": ", safe_get("title"), "\n",
      "\n", sub_heading_prefix, " Group contains ", safe_get("num_members"),
      " ", entity, " entities:\n",
      safe_get("member_data"), "\n"
    )
  }
}


# Iterate over rows of a data frame, printing a formatted description for each.
# Replaces the old glue_data_safely() function.
# @param .x A data frame to iterate over.
# @param type One of "nogroup" or "group".
# @param .entity Entity type label (e.g. "KPI", "Municipality").
# @param .format One of "inline" or "md".
# @param .heading_length Heading level for markdown output.
# @param .sub_heading_length Sub-heading level for markdown output.
format_data_safely <- function(
  .x, type, .entity, .format, .heading_length = 2, .sub_heading_length = 3
) {
  heading_prefix <- switch(
    .format,
    inline = paste0(paste(rep("#", getOption("width", 80)), collapse = ""), "\n"),
    md = paste0(paste(rep("#", .heading_length), collapse = ""), " ")
  )
  sub_heading_prefix <- paste(rep("#", .sub_heading_length), collapse = "")

  for (i in seq_len(nrow(.x))) {
    row <- as.list(.x[i, ])
    cat(desc_format(type, row, heading_prefix, sub_heading_prefix, .entity))
  }

  return(invisible(.x))
}
