# Package index

## Data access

Download data and metadata from the Kolada API.

- [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
  : Get data from Kolada
- [`get_metadata()`](https://lchansson.github.io/rKolada/reference/get_metadata.md)
  : Download metadata from the Kolada API
- [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  [`get_kpi_groups()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  [`get_ou()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  [`get_municipality()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  [`get_municipality_groups()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  : Download metadata for a specific entity from the Kolada API

## Search and filter

Search and filter metadata tables.

- [`kpi_search()`](https://lchansson.github.io/rKolada/reference/kpi_search.md)
  : Search for Kolada KPIs using a Kolada KPI table
- [`municipality_search()`](https://lchansson.github.io/rKolada/reference/municipality_search.md)
  : Search a Kolada municipality metadata table
- [`ou_search()`](https://lchansson.github.io/rKolada/reference/ou_search.md)
  : Search a Kolada Organizational Unit metadata table
- [`kpi_grp_search()`](https://lchansson.github.io/rKolada/reference/kpi_grp_search.md)
  : Search a Kolada KPI Group metadata table for group names
- [`municipality_grp_search()`](https://lchansson.github.io/rKolada/reference/municipality_grp_search.md)
  : Search a Kolada Municipality Group metadata table for group names

## Describe

Human-readable descriptions of metadata.

- [`kpi_describe()`](https://lchansson.github.io/rKolada/reference/kpi_describe.md)
  : Describe the KPIs in a Kolada KPI metadata table
- [`kpi_grp_describe()`](https://lchansson.github.io/rKolada/reference/kpi_grp_describe.md)
  : Describe the KPIs in a Kolada KPI Group metadata table
- [`municipality_grp_describe()`](https://lchansson.github.io/rKolada/reference/municipality_grp_describe.md)
  : Describe the municipalitie in a Kolada Municipality Group metadata
  table

## Transform

Simplify and reshape metadata and data.

- [`kpi_minimize()`](https://lchansson.github.io/rKolada/reference/kpi_minimize.md)
  : Simplify a KPI table
- [`kpi_bind_keywords()`](https://lchansson.github.io/rKolada/reference/kpi_bind_keywords.md)
  : Add keyword columns to a Kolada KPI table
- [`values_minimize()`](https://lchansson.github.io/rKolada/reference/values_minimize.md)
  : Simplify a Kolada values table
- [`values_legend()`](https://lchansson.github.io/rKolada/reference/values_legend.md)
  : Create KPI long-form descriptions to add to a plot
- [`kpi_grp_unnest()`](https://lchansson.github.io/rKolada/reference/kpi_grp_unnest.md)
  : Create a KPI table from a Kolada KPI Group metadata table
- [`municipality_grp_unnest()`](https://lchansson.github.io/rKolada/reference/municipality_grp_unnest.md)
  : Create a municipality table from a Kolada Municipality Group
  metadata table

## Extract IDs

Extract ID vectors for use with get_values().

- [`kpi_extract_ids()`](https://lchansson.github.io/rKolada/reference/kpi_extract_ids.md)
  : Extract a vector of KPI ID strings from a Kolada KPI metadata table
- [`municipality_extract_ids()`](https://lchansson.github.io/rKolada/reference/municipality_extract_ids.md)
  : Extract a vector of municipality ID strings from a Kolada
  municipality table
- [`kpi_grp_extract_ids()`](https://lchansson.github.io/rKolada/reference/kpi_grp_extract_ids.md)
  : Extract KPI ID strings from a Kolada KPI Group metadata table
- [`municipality_grp_extract_ids()`](https://lchansson.github.io/rKolada/reference/municipality_grp_extract_ids.md)
  : Extract municipality ID strings from a Kolada municipality group
  table
- [`municipality_name_to_id()`](https://lchansson.github.io/rKolada/reference/municipality_name_to_id.md)
  : Convert a vector of municipality names to municipality ids
- [`municipality_id_to_name()`](https://lchansson.github.io/rKolada/reference/municipality_id_to_name.md)
  : Convert a vector of municipality ids to municipality names

## Query building

Compose API query URLs.

- [`compose_metadata_query()`](https://lchansson.github.io/rKolada/reference/compose_metadata_query.md)
  : Compose a query to fetch metadata from the Kolada API.
- [`compose_data_query()`](https://lchansson.github.io/rKolada/reference/compose_data_query.md)
  : Compose a query to fetch metadata from the Kolada API. Its use is
  mainly

## Utilities

API availability, caching, and other helpers.

- [`kolada_available()`](https://lchansson.github.io/rKolada/reference/kolada_available.md)
  : Check if the Kolada API is available
- [`kolada_cache_dir()`](https://lchansson.github.io/rKolada/reference/kolada_cache_dir.md)
  : Get the rKolada cache directory
- [`kolada_clear_cache()`](https://lchansson.github.io/rKolada/reference/kolada_clear_cache.md)
  : Clear the rKolada cache
- [`allowed_entities()`](https://lchansson.github.io/rKolada/reference/allowed_entities.md)
  : Allowed entities: Kolada metadata classes
