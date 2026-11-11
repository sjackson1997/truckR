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

## Status (October 2026)

Done:
- P0 blockers: `PC_Miler_API.R` moved to `inst/examples` (API key removed), `scratch.R` ignored, `pcm_POST` takes the key and URL as arguments.
- P1: roxygen-managed exports; DESCRIPTION filled in; every exported function and both data sets documented. `devtools::check()` is clean (0 errors, 0 warnings, 0 notes).
- P2: testthat set up (about 430 test blocks, all offline). The older `pcm_*` functions still have no tests.
- Bid reports: `read_BidReport2()` and `ingest_bidreports()` added.
- New (spec `.kiro/specs/multi-api-routing`, Phase 1): `route()` with an optional waypoint, the OpenRouteService provider, provider registry, answer cache with quota handling, README and NEWS.

Next (the later phases of the routing spec):
1. Phase 2: second provider (OSRM, Google or HERE) for production-scale runs, since the free ORS limits (2,000 routes per day) are small for big bid files.
2. Phase 3: credential helpers, provider options, retry and batch settings.
3. Phase 4: refactor the PC*MILER functions onto the provider interface, tests for them, CRAN-style extras (vignette, pkgdown, GitHub Actions).
4. Then P3 below (migrating functions from Resource_Files).

---
Open question (before sharing any routing results outside the company):
- OpenRouteService results are based on OpenStreetMap and are licensed CC-BY-SA 4.0, which asks for attribution ("openrouteservice.org | OpenStreetMap contributors") and may apply share-alike terms to what is shared. For now the package is used only inside the company, so nothing is needed. Before sharing distances with a carrier, customer or other outside party: (1) read the formal ORS Terms of Service (only a forum answer from ORS staff has been read, which said storing results is fine), (2) ask Legal or Procurement whether the attribution is enough, (3) decide whether to add the attribution line to exported files. The same check applies to any second provider added later. The README has a short note on this.
## Remaining plan
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
