# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

rKolada is an R package that wraps the Kolada REST API (https://api.kolada.se/v3/) — a database of 4,000+ Key Performance Indicators for Swedish municipalities and regions. It provides tidy, pipeable interfaces for downloading and processing Kolada data.

## Build & Development Commands

```bash
# Check package (equivalent to CRAN check)
R CMD check .

# Build package
R CMD build .

# Install locally
R CMD INSTALL .

# Run tests
Rscript -e "testthat::test_local()"

# Regenerate documentation (roxygen2)
Rscript -e "devtools::document()"

# Build pkgdown site
Rscript -e "pkgdown::build_site()"

# Regenerate internal vignette data (sysdata.rda)
Rscript data-raw/vignette-data.R
```

From an R session with devtools:
```r
devtools::check()       # Full package check
devtools::test()        # Run tests only
devtools::document()    # Rebuild docs from roxygen2 comments
devtools::load_all()    # Load package for interactive development
devtools::install()     # Install from source
```

## Architecture

### API Layer (`R/metadata.R`, `R/data.R`)

Two core functions handle all Kolada API communication:

- `get_metadata()` — fetches entity metadata (KPIs, municipalities, groups, OUs) with caching and pagination
- `get_values()` — fetches actual KPI data values with automatic pagination and optional simplification (joins human-readable names)

Query URLs are built by `compose_metadata_query()` and `compose_data_query()`. All HTTP is via `httr2`. Pagination is handled transparently using the v3 `next_url` field — callers never see it.

### Entity Wrappers

Thin wrappers around `get_metadata()` for each entity type: `get_kpi()`, `get_kpi_groups()`, `get_municipality()`, `get_municipality_groups()`, `get_ou()`.

### Entity Operations (`R/kpi.R`, `R/municipality.R`, `R/kpi_groups.R`, `R/municipality_grp.R`)

Each entity type follows a consistent pattern of operations:
- `*_search()` — filter by text/regex
- `*_describe()` — print human-readable descriptions
- `*_extract_ids()` — extract ID vector for passing to `get_values()`
- `*_minimize()` — remove low-information columns
- `*_grp_unnest()` — expand grouped entities

### Design Conventions

- All public functions return tibbles and are designed for `%>%` chaining
- Naming: `entity_operation()` (e.g., `kpi_search`, `municipality_extract_ids`)
- API failures return NULL with warnings (no exceptions)
- `kolada_available()` guards all examples/vignettes for offline safety (required by CRAN policy)
- Uses `.data` pronoun from rlang for tidy evaluation in filter/mutate contexts; string column names in select/rename/unnest contexts
- Roxygen2 v7.3.1 generates all man pages and NAMESPACE — never edit those by hand

### Internal Data

`R/sysdata.rda` contains pre-cached API responses used by vignettes to avoid network calls. Regenerate via `data-raw/vignette-data.R`.

## Testing

Tests use testthat edition 3 and live in `tests/testthat/`. Test files:
- `test-query-composers.R` — URL construction (pure, no network)
- `test-utils.R` — internal utilities (pure)
- `test-search.R` — `*_search()` functions with synthetic data (pure)
- `test-kpi.R` — KPI operations (pure)
- `test-municipality.R` — municipality operations (pure)
- `test-groups.R` — group unnest/search/extract (pure)
- `test-data-helpers.R` — `values_minimize()`, `values_legend()` (pure)
- `test-api-mocked.R` — API functions with mocked httr2 responses (offline-safe)
- `test-api-live.R` — live integration tests (skipped when API unavailable)

The `kolada_available()` guard function should wrap any test or example that calls the API.

## CRAN Notes

The package was previously archived for CRAN policy violations (network access in tests/examples). All API-dependent code must be wrapped with `kolada_available()` checks and must fail gracefully when offline.
