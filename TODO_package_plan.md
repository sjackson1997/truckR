# truckR Package Reorganization Plan

## Overview

This document captures findings from a review of the `truckR` package (currently at
`C:\Users\KQV2764\OneDrive - MDLZ\AdvancedDataAnalyst\truckR`) and the related
`Resource_Files` folder that holds reusable R functions and data.

The goal is to consolidate reusable transportation-analysis functions into a proper
R package suitable for internal use (and potentially CRAN someday).

---

## Package Naming

"truckR" is too narrow for the actual scope. The package covers:

- Routing APIs (PC Miler, HERE)
- Market rate data (DAT iQ / Chainalytics)
- OTM shipment data filtering and summarization
- Lane key construction and postal code normalization
- Bid report ingestion (CSO exports)
- Excel output formatting (openxlsx styles)
- gt table construction for market position reporting

Consider a name that reflects "Mondelez transportation sourcing/analytics toolbox" —
something like `mdlzTransport`, `freightR`, `sourcingTools`, or similar.

---

## Current State (as of 2025-01-xx, commit 975c6fa)

### What exists in `truckR/R/`:
| File | Status |
|------|--------|
| `pcm_prep_dataframe.R` | Good — documented, exported |
| `pcm_process_request.R` | Needs docs, uses global vars |
| `pcm_Route_json.R` | Partial docs |
| `pcm_POST.R` | Partial docs, references global `route_url_post` and `PC_MILER_API_KEY` |
| `pcm_MileageReport_to_df.R` | Partial docs |
| `convert_row_to_list.R` | Internal helper, shouldn't be exported |
| `split_df_to_size.R` | Good — documented |
| `get_location_columns.R` | Internal helper, no docs |
| `match_columns.R` | Internal helper, no docs |
| `utils.R` | `fix.ZIP`, `fix.CTRY`, `fix.MODE` — no docs, should be exported |
| `utils-pipe.R` | Magrittr pipe re-export (fine) |
| `PC_Miler_API.R` | **PROBLEM** — loose script with executable code, must be removed from `R/` |
| `sysdata.rda` | Internal data (`pcm_address_regexes`) — correct |

### Datasets in `data/`:
- `df_route_samples.rda` — clean test routes
- `df_route_unclean.rda` — routes with intentional errors for testing

### Git:
- 30+ commits on `main`, pushed to `origin/main`
- Clean history, no branches

---

## Issues to Fix (Priority Order)

### P0 — Blockers (package won't pass `R CMD check`)

1. **Remove `PC_Miler_API.R` from `R/`** — it contains top-level executable code
   (readRDS, loops, API calls). Move to `inst/examples/pcm_example.R` or delete.
2. **Remove `scratch.R`** from package root (or add to `.Rbuildignore`).
3. **Eliminate global variable references** in `pcm_POST.R`:
   - `route_url_post` → make a parameter with a default
   - `PC_MILER_API_KEY` → parameter defaulting to `Sys.getenv("PC_MILER_API_KEY")`

### P1 — Structural improvements

4. **Switch NAMESPACE to roxygen2-managed exports.** Remove `exportPattern(...)` line.
   Add `@export` only to user-facing functions. Run `devtools::document()`.
   - Export: `pcm_prep_dataframe`, `pcm_process_request`, `fix.ZIP`, `fix.CTRY`, `fix.MODE`, `split_df_to_size`
   - Keep internal: `convert_row_to_list`, `match_columns`, `get_location_columns1`, `get_location_columns2`, `pcm_Route_json`, `pcm_POST`, `pcm_MileageReport_to_df`
5. **Fix DESCRIPTION:**
   - Fill in Title, Author, Maintainer, Description
   - Add to `Imports`: `dplyr`, `rlang`, `stringr`
   - Add to `Suggests`: `testthat (>= 3.0.0)`
   - Consider bumping R dependency to `>= 4.1.0` (for native pipe, if desired)
6. **Complete roxygen documentation** for all exported functions — especially
   `@return`, `@param` descriptions, and `@examples`.

### P2 — Testing

7. **Set up testthat infrastructure:** `usethis::use_testthat()`
8. **Write tests for pure functions first:**
   - `fix.ZIP()` — use `df_route_unclean` to verify corrections
   - `fix.CTRY()` — test "U.S.", "USA", "MEX", "CAN", "CA", "MX" variants
   - `fix.MODE()` — test "OTR", "TL", "ROAD", "INTRMDL", "IMDL" variants
   - `split_df_to_size()` — verify chunk sizes
   - `pcm_prep_dataframe()` — verify RouteId assignment and deduplication
9. **Mock API tests** (later) for `pcm_POST` / `pcm_process_request` using `httptest` or `webmockr`.

### P3 — Migrate functions from Resource_Files

Source location: `Resource_Files/Scripts/Reused_Common_Scripts/` and `Resource_Files/Scripts/`

| Function group | Source file(s) | Target file in package |
|---|---|---|
| Lane keys | `Functions_LaneKeys.R` | `R/lane_keys.R` |
| Fix functions (expanded) | `Functions_LaneKeys.R` | `R/utils.R` (merge with existing) |
| OTM utilities | `Functions_Other.R` | `R/otm.R` |
| Bid report I/O | `Functions_Other.R`, `Common_Setup_and_Functions.R` | `R/bid_reports.R` |
| CSO name replacements | `Functions_Other.R` | `R/cso_helpers.R` (or internal data) |
| Volume forecasting | `Common_Setup_and_Functions.R` | `R/forecasting.R` |
| Ratio breakdown | `Common_Setup_and_Functions.R` | `R/allocation.R` |
| HERE API | `HERE_API_Route_Function.R`, `HERE_API_Single_Geocode_Function.R` | `R/here_api.R` |
| DAT iQ API | `DAT_IQ_API_function.R` | `R/datiq_api.R` |
| Plot theme & helpers | `Common_Plot_Code.R` | `R/plotting.R` |
| gt market tables | `Grammar_of_Tables_functions.R` | `R/gt_tables.R` |
| OpenXLSX styles | `OpenXLSX_Calls.R` | `R/excel_styles.R` |

**Migration approach for each group:**
1. Create a feature branch
2. Copy functions in, remove `library()` calls, add `pkg::` qualifiers
3. Add roxygen2 documentation
4. Add tests
5. Update DESCRIPTION Imports
6. Run `devtools::check()`
7. Merge to main

### P4 — Cleanup of Resource_Files

10. After functions are migrated, update other projects that `source()` from
    Resource_Files to use `library(packageName)` instead.
11. Archive or remove migrated function files from Resource_Files.
12. Organize remaining Resource_Files content:
    - `Data/` — keep as reference data store
    - `Scripts/Adhoc/` — move one-off scripts (SO questions, XML compare, Kenneth's adhoc)
    - `Scripts/ETL/` — keep data-reading/cleaning scripts that aren't generic functions
    - Remove empty folders (`DATiQ/`, `Plots/`)
    - Move secrets to `.Renviron` (never commit `secrets.R`)

### P5 — Nice-to-haves for CRAN readiness

13. Add a `README.Rmd` with basic usage examples
14. Add a `NEWS.md` changelog
15. Add a package-level documentation file (`R/packageName-package.R`)
16. Set up GitHub Actions for `R CMD check` (if hosted on GitHub)
17. Add vignettes for major workflows (PC Miler routing, DAT iQ market data)
18. Consider `pkgdown` site for documentation

---

## Secrets / API Keys

Current approach is mixed:
- PC Miler: `Sys.getenv("PC_MILER_API_KEY")` (correct pattern)
- HERE: `readRDS("path/to/HERE_API_KEY.rds")` (should switch to env var)
- DAT iQ: hardcoded in function body (must be removed, use env var)

All API functions should follow this pattern:
```r
my_api_function <- function(..., api_key = Sys.getenv("MY_API_KEY")) {
  if (nchar(api_key) == 0) stop("API key not found. Set MY_API_KEY env variable.")
  ...
}
```

---

## Notes

- The `renv` lockfile is present but may be stale. Run `renv::status()` to check.
- The magrittr pipe re-export is fine, but if you bump to R >= 4.1 you could
  refactor to native `|>` and drop the dependency.
- The `exportPattern("^[[:alpha:]]+")` in NAMESPACE means *everything* in `R/` is
  currently exported, including internal helpers. This is why switching to
  roxygen2-driven exports is important.
