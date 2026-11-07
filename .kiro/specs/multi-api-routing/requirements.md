# Requirements Document

## Introduction

The truckR package currently supports only the PC Miler API for transportation route analysis (mileage, travel time, tolls, etc.). This feature extends the package to support multiple routing API providers (Google Maps, HERE, OSRM, OpenRouteService) through a common abstraction layer, implements secure API key management suitable for a shared R package, and maintains CRAN-readiness for future submission.

## Glossary

- **Router**: The truckR abstraction layer that dispatches route requests to a configured API provider
- **Provider**: A specific routing API backend (e.g., PC Miler, Google Maps, HERE, OSRM, OpenRouteService)
- **Route_Request**: A structured object containing origin/destination pairs and routing options to be sent to a Provider
- **Route_Result**: A standardized data frame returned by any Provider containing distance, duration, and optional metadata
- **Credentials_Store**: The mechanism used to securely store and retrieve API keys (environment variables via .Renviron or system keyring)
- **Provider_Config**: A configuration object specifying which Provider to use and any provider-specific options
- **Column_Mapper**: The component that identifies origin/destination columns in a user-supplied data frame using regex patterns

## Requirements

### Requirement 1: Provider Abstraction Layer

**User Story:** As a developer, I want a common interface for all routing providers, so that I can switch between APIs without changing my analysis code.

#### Acceptance Criteria

1. THE Router SHALL expose a single `route()` function that accepts a Route_Request and returns a Route_Result regardless of which Provider is active
2. WHEN a Provider is registered, THE Router SHALL validate that the Provider implements all required interface functions (route single, route batch, and validate credentials)
3. THE Router SHALL return Route_Result data frames with a consistent schema containing at minimum: RouteId, origin address fields, destination address fields, the always-present distance, duration and waypoint-flag columns defined in Requirement 13 criterion 6 (`distance_direct`, `distance_routed`, `duration_direct`, `duration_routed`, `has_waypoint`), error fields, and `provider_name`. Distances SHALL be expressed in miles and durations in hours, regardless of the units a Provider returns.
4. WHEN a user calls `route()` without configuring a Provider, THE Router SHALL stop with a clear error message indicating no Provider is configured
5. THE Router SHALL support registering custom Provider implementations at runtime via a `register_provider()` function

### Requirement 2: PC Miler Provider

**User Story:** As a current truckR user, I want the existing PC Miler functionality preserved as a provider module, so that my existing workflows continue to function.

#### Acceptance Criteria

1. THE PC_Miler_Provider SHALL implement the Provider interface using the existing PC Miler REST API for route mileage reports
2. THE PC_Miler_Provider SHALL support batch requests by chunking routes into groups of configurable size (default 20)
3. WHEN the PC Miler API returns an error for a stop, THE PC_Miler_Provider SHALL include the error information in the Route_Result rather than failing the entire batch
4. THE PC_Miler_Provider SHALL map its API-specific response fields (LMiles, TMiles, LHours, THours, LTolls, TTolls, LEstghg, TEstghg) into the standardized Route_Result schema

### Requirement 3: Google Maps Provider

**User Story:** As a developer, I want to use the Google Maps Directions API as a routing provider, so that I have an alternative commercial option with broad global coverage.

#### Acceptance Criteria

1. THE Google_Maps_Provider SHALL implement the Provider interface using the Google Maps Directions API
2. THE Google_Maps_Provider SHALL extract distance and duration from the API response legs and map them into the Route_Result schema
3. WHEN the Google Maps API returns a status other than "OK" for a route, THE Google_Maps_Provider SHALL include the error status in the Route_Result error fields
4. THE Google_Maps_Provider SHALL support specifying travel mode (driving is default for trucking) via Provider_Config options

### Requirement 4: HERE Provider

**User Story:** As a developer, I want to use the HERE Routing API as a provider, so that I have another commercial alternative with truck-specific routing capabilities.

#### Acceptance Criteria

1. THE HERE_Provider SHALL implement the Provider interface using the HERE Routing v8 API
2. THE HERE_Provider SHALL support truck-specific routing parameters (vehicle weight, height, length) via Provider_Config options
3. WHEN the HERE API returns an error or no route found, THE HERE_Provider SHALL include the error detail in the Route_Result error fields
4. THE HERE_Provider SHALL extract distance and duration from the route sections and map them into the Route_Result schema

### Requirement 5: OSRM Provider (Free/Open Source)

**User Story:** As a developer, I want to use the OSRM routing engine as a free provider, so that I can perform route analysis without paid API subscriptions.

#### Acceptance Criteria

1. THE OSRM_Provider SHALL implement the Provider interface using the OSRM HTTP API (public demo server or user-specified server URL)
2. THE OSRM_Provider SHALL geocode city/state/zip/country addresses to coordinates before requesting routes, since OSRM requires coordinate inputs
3. WHEN the OSRM API returns no route or an error, THE OSRM_Provider SHALL include the error status in the Route_Result error fields
4. THE OSRM_Provider SHALL allow users to specify a custom OSRM server URL via Provider_Config for self-hosted instances
5. IF the public OSRM demo server rate-limits requests, THEN THE OSRM_Provider SHALL retry with exponential backoff up to a configurable maximum number of attempts

### Requirement 6: OpenRouteService Provider (Free Tier)

**User Story:** As a developer, I want to use OpenRouteService as a provider, so that I have a free option with a generous API tier and truck routing profiles.

#### Acceptance Criteria

1. THE ORS_Provider SHALL implement the Provider interface using the OpenRouteService Directions API v2
2. THE ORS_Provider SHALL support the "driving-hgv" (heavy goods vehicle) profile for truck-specific routing
3. WHEN the OpenRouteService API returns an error, THE ORS_Provider SHALL include the error detail in the Route_Result error fields
4. THE ORS_Provider SHALL geocode city/state/zip/country addresses to coordinates before requesting routes, since the Directions API requires coordinate inputs
5. THE ORS_Provider SHALL respect the free-tier rate limit by throttling requests to a configurable maximum requests-per-minute

### Requirement 7: API Key and Secrets Management

**User Story:** As a package user, I want a secure and ergonomic way to provide my API keys, so that my credentials are never exposed in source code and other users of the package can supply their own keys.

#### Acceptance Criteria

1. THE Credentials_Store SHALL read API keys from environment variables following the naming convention `TRUCKR_{PROVIDER}_API_KEY` (e.g., `TRUCKR_PCMILER_API_KEY`, `TRUCKR_GOOGLE_API_KEY`)
2. THE Credentials_Store SHALL provide a helper function `truckr_set_key(provider, key)` that writes the key to the user-level `.Renviron` file with the correct variable name
3. WHEN a Provider function is called and the required API key environment variable is not set, THE Credentials_Store SHALL stop with a clear error message naming the missing variable and suggesting how to set it
4. THE Credentials_Store SHALL provide a `truckr_has_key(provider)` function that returns TRUE or FALSE indicating whether a key is configured for the given provider
5. THE Credentials_Store SHALL ensure that API keys are never written to package source files, version control, or logged in console output
6. WHERE the keyring package is available, THE Credentials_Store SHALL optionally support reading keys from the system keyring as a more secure alternative to .Renviron

### Requirement 8: Provider Configuration and Selection

**User Story:** As a developer, I want to easily switch between routing providers and pass provider-specific options, so that I can compare results or use the best provider for each use case.

#### Acceptance Criteria

1. THE Router SHALL allow setting the active Provider via a `truckr_set_provider(provider_name)` function that persists for the session
2. THE Router SHALL allow passing provider-specific options via a `truckr_provider_options(...)` function
3. WHEN `truckr_set_provider()` is called with an unregistered provider name, THE Router SHALL stop with an error listing available providers
4. THE Router SHALL provide a `truckr_get_provider()` function that returns the name of the currently active Provider
5. THE Router SHALL provide a `truckr_list_providers()` function that returns the names of all registered Providers

### Requirement 9: Data Preparation and Column Mapping

**User Story:** As a developer, I want the existing column-matching intelligence preserved and generalized, so that any provider can accept data frames with flexible column naming.

#### Acceptance Criteria

1. THE Column_Mapper SHALL identify origin and destination address columns in user-supplied data frames using configurable regex patterns
2. THE Column_Mapper SHALL support the address components: city, state/province, postal code, and country
3. WHEN required address columns cannot be matched, THE Column_Mapper SHALL stop with a clear error listing which components could not be found
4. THE Column_Mapper SHALL assign a unique RouteId to each distinct origin-destination pair and de-duplicate before sending to the Provider
5. THE Column_Mapper SHALL support an optional street address component for providers that accept full addresses

### Requirement 10: CRAN Readiness

**User Story:** As a package maintainer, I want the package to comply with CRAN policies from the start, so that future submission requires minimal rework.

#### Acceptance Criteria

1. THE truckR_Package SHALL pass `R CMD check` with no errors and no warnings
2. THE truckR_Package SHALL declare all dependencies in the DESCRIPTION file Imports or Suggests fields with minimum version constraints where needed
3. THE truckR_Package SHALL use conditional checks (`requireNamespace()`) for optional dependencies (e.g., keyring) listed in Suggests
4. THE truckR_Package SHALL include roxygen2 documentation with `@export`, `@param`, `@return`, and `@examples` tags for all exported functions
5. THE truckR_Package SHALL wrap all API calls in examples and tests with `\dontrun{}` or skip conditions so that CRAN checks do not require live API access
6. THE truckR_Package SHALL not store any credentials or secrets in package source, data, or vignettes

### Requirement 11: Error Handling and Retry Logic

**User Story:** As a developer, I want robust error handling for API calls, so that transient network failures do not silently corrupt my results.

#### Acceptance Criteria

1. IF an API request fails due to a network error (timeout, connection refused), THEN THE Router SHALL retry the request up to a configurable number of times (default 3) with exponential backoff
2. IF all retry attempts fail, THEN THE Router SHALL return an error entry in the Route_Result with the failure reason rather than stopping execution
3. WHEN a Provider returns an HTTP error status (4xx or 5xx), THE Router SHALL classify it as retryable (5xx, 429) or non-retryable (4xx except 429) and only retry retryable errors
4. THE Router SHALL emit informative messages via `message()` during retries so users are aware of transient failures

### Requirement 12: Batch Processing

**User Story:** As a developer processing large shipment files, I want efficient batch routing that respects API rate limits, so that I can process hundreds of routes reliably.

#### Acceptance Criteria

1. THE Router SHALL split large route sets into provider-appropriate chunk sizes before dispatching to the Provider
2. THE Router SHALL combine all chunk results into a single Route_Result data frame with consistent column ordering
3. WHILE processing batches, THE Router SHALL emit progress messages indicating how many chunks remain
4. WHEN a single chunk fails after retries, THE Router SHALL continue processing remaining chunks and include error entries for the failed routes in the final Route_Result

### Requirement 13: Intermediate Waypoints (Pass-Through Points)

**User Story:** As a developer, I want to optionally force a route to pass through a single intermediate point (for example, shipments from Mexico to California that must route through Laredo, TX), so that computed distances reflect the real path and I can audit the detour.

#### Acceptance Criteria

1. THE Router SHALL support an optional per-route intermediate waypoint, enabled by an OPTIONAL logical column in the input data frame named `Use_Waypoint`. The column MAY be omitted entirely. Rows where the flag is FALSE or NA SHALL be routed directly origin-to-destination. The rules for combining the column with the `waypoint` parameter are:
   - column absent and `waypoint` NULL: no waypoint processing; all lanes are routed directly;
   - column present and `waypoint` supplied: lanes with TRUE use the waypoint;
   - column present with any TRUE but `waypoint` NULL: stop with a clear error;
   - column present with no TRUE and `waypoint` NULL: all lanes are routed directly (no error);
   - column absent but `waypoint` supplied: stop with a clear error asking the caller to add `Use_Waypoint`, so a waypoint is never silently applied to every lane;
   - column present but not logical: stop with a clear error.
2. THE Router SHALL accept the waypoint location as a function parameter supplied by the caller (not required to be a column), specified flexibly as any of: a postal/zip code, a city/state, a full street address, or latitude/longitude coordinates.
3. WHERE a provider requires coordinate inputs (e.g. OSRM, OpenRouteService), THE Router SHALL geocode a non-coordinate waypoint to coordinates before requesting the route, using the same geocoding path as origin/destination addresses.
4. THE Router SHALL support at most ONE intermediate waypoint per route. Multiple intermediate stops are explicitly out of scope.
5. THE Column_Mapper SHALL incorporate the waypoint (and whether it is used) into the lane-level RouteId, so that two rows sharing an origin and destination but differing in waypoint usage are treated as distinct lanes.
6. THE Route_Result SHALL ALWAYS include the following columns, populated for EVERY lane regardless of whether any lane uses a waypoint, so downstream analysis and filtering can reference them consistently:
   - `distance_direct`: the shortest origin-to-destination distance with no waypoint,
   - `distance_routed`: the distance as specified by the user. For a lane that uses a waypoint, this is the full origin-to-waypoint-to-destination distance. For a lane that does not use a waypoint, it equals `distance_direct`. (Working name; see Note C.)
   - `duration_direct`: the driving time in hours for the shortest origin-to-destination route with no waypoint,
   - `duration_routed`: the driving time in hours for the route as specified by the user (the sum of the two segment durations when a waypoint is used, otherwise equal to `duration_direct`),
   - `has_waypoint` (logical): TRUE if the lane was routed through a waypoint, FALSE otherwise, for quick filtering.
7. WHEN no route in the input uses a waypoint, THE Route_Result SHALL contain NO waypoint-related columns beyond the five in criterion 6.
8. WHEN at least one route in the input uses a waypoint, THE Route_Result SHALL retain all standard columns, including the five in criterion 6, and additionally include the columns below. For lanes that did NOT use a waypoint, every one of these additional columns SHALL be NA:
   - the waypoint value that was used and its type (zip, city/state, address, or coordinates),
   - `detour_added` (numeric): the additional distance in miles the waypoint adds versus the direct route (`distance_routed - distance_direct`),
   - `detour_duration_added` (numeric): the additional driving time in hours (`duration_routed - duration_direct`).
9. THE Router SHALL treat a waypoint lane as two ordinary point-to-point segments, origin-to-waypoint and waypoint-to-destination, and SHALL compute `distance_routed` and `duration_routed` as the sums of the two segment distances and durations. This keeps every Provider limited to simple point-to-point routing, so waypoint support is never required of a Provider. Per-segment distances are internal and SHALL NOT appear in the Route_Result.
   - IF either segment fails or returns an error, THEN `distance_routed` SHALL be NA for that lane and the error detail SHALL be included in the lane's error fields.
10. FOR each lane that uses a waypoint, THE Router SHALL verify that neither `distance_routed` nor `duration_routed` is shorter than its direct counterpart (within a 0.5% tolerance), and SHALL surface a warning or error field when it is, since this indicates a likely mis-geocoded or invalid waypoint.
11. BEFORE submission to a Provider, THE Column_Mapper SHALL expand all lanes (direct lanes, the direct segment of each waypoint lane, and both waypoint segments) into a single pool of point-to-point segments and de-duplicate that pool by origin and destination location, so each distinct segment (e.g. "Mexico City to Laredo, TX") is submitted exactly once regardless of how many lanes use it. Results SHALL then be mapped back to every originating lane to populate `distance_direct` and `distance_routed`.

### Requirement 14: Geocoding and Location Caching

**User Story:** As a developer using coordinate-based providers on rate-limited free tiers, I want locations geocoded once and reused, so that I conserve API quota and get consistent coordinates across lanes, segments and waypoints.

#### Acceptance Criteria

1. THE Geocoder SHALL convert an origin, destination or waypoint specified as a postal code, city/state, full street address, or coordinates into latitude/longitude, passing coordinates through unchanged without an API call.
2. THE Geocoder SHALL restrict each lookup to the location's country when a country is known, to avoid matching a same-named place in another country.
3. THE Geocoder SHALL de-duplicate locations before any API call, so each distinct location is geocoded at most once per run.
4. THE Geocoder SHALL cache results for the session and SHALL support an optional persistent on-disk cache in a user-level cache directory, never in the package source or the working project.
5. WHEN a location cannot be geocoded, THE Geocoder SHALL record the failure for that location only, and every lane or segment depending on that location SHALL be returned with NA distances and a descriptive error rather than stopping the whole run.
6. THE Geocoder SHALL record the match confidence or match type returned by the geocoding service and SHALL flag low-confidence matches in the Route_Result so they can be reviewed.
7. THE Geocoder SHALL be implemented per Provider (each Provider supplies its own geocoding function) so that a Provider without geocoding can delegate to another.
8. THE Router SHALL pass each Provider every available component of a location (street address, postal code, city, state, country), allowing any component to be missing, including the street address, which is expected to be present only occasionally, and the Provider SHALL choose its own geocoding strategy from them. The package SHALL NOT assume one strategy (such as city/state or postal code) works for all Providers.
9. WHERE a street address is present for a location, THE Provider SHALL attempt to geocode using it first. IF the street address cannot be matched, THEN the Provider MAY fall back to a coarser lookup from the remaining components, but SHALL flag the location in `geocode_flag` as having used the coarser fallback, so lanes with reduced precision are identifiable. The street address SHALL be part of the location's de-duplication key, so two different addresses in the same city are not collapsed into one location.
10. WHEN a Provider cannot reliably geocode a location from the components supplied (for example a postal-code-only location in a country the Provider handles poorly), THE Provider SHALL return a failed location with an explanatory reason rather than falling back to a loose search that could return a wrong place.

## Implementation Notes

These notes capture cross-cutting decisions that affect how the requirements above are prioritized and implemented. They are context for the design phase, not additional acceptance criteria.

### Note A: Provider prioritization — PC Miler is now legacy/untestable

The maintainer no longer holds a PC Miler license and cannot test or develop against the PC Miler API. Consequently:

- The existing PC Miler code (Requirement 2) SHOULD be preserved as a legacy provider but is NOT an active development target. It should be clearly marked as untested/legacy.
- The free or free-tier providers that can be developed and tested without a paid subscription are the PRIMARY implementation targets: OSRM (Requirement 5), OpenRouteService (Requirement 6), and the free tiers of Google Maps (Requirement 3) and HERE (Requirement 4).
- The intermediate waypoint feature (Requirement 13) SHOULD be designed and first implemented against a free, testable provider (OpenRouteService is a strong candidate: it supports intermediate waypoints and a `driving-hgv` truck profile), NOT against PC Miler.

### Note B: Generalize the data-preparation step beyond PC Miler

The current `pcm_prep_dataframe()` function conflates two provider-agnostic concerns: (1) column mapping (identifying origin/destination address columns) and (2) RouteId assignment plus de-duplication of identical routes. Despite the `pcm_` prefix, neither concern is specific to PC Miler.

- During implementation, this preparation step SHOULD be generalized into a provider-neutral component (aligning with the Column_Mapper of Requirement 9 and the batch de-duplication of Requirement 9.4), so that every provider consumes a common prepared structure.
- The provider-neutral prep step MUST be extended to (a) carry the waypoint into the lane-level RouteId per Requirement 13.5, and (b) expand lanes into a de-duplicated pool of point-to-point segments per Requirement 13.11. Today's `pcm_prep_dataframe()` de-duplicates whole lanes only, so segment-level de-duplication is new behavior.
- This refactor is deferred until active provider implementation begins; the existing function remains in place and functional in the meantime.

### Note C: Naming of the "as-specified" distance column

The column holding the distance as the user specified the lane (via the waypoint when one is used, direct otherwise) is provisionally named `distance_routed`, paired with `distance_direct`. Other candidates considered: `distance_spec`, `distance_used`, `distance_final`. The name is a working choice and can be changed before implementation; the requirement is that the column exists, is always populated, and is the single column downstream analysis references.

### Note D: Delivery phases

The requirements above are too large to build at once. They are delivered in phases, each independently useful:

- **Phase 1 (first design and build):** Requirement 1 (core Router), Requirement 6 (OpenRouteService provider), the key-reading parts of Requirement 7 (7.1, 7.3, 7.4), Requirement 9 (column mapping and de-duplication), Requirement 13 (waypoints and segment expansion), Requirement 14 (geocoding and caching), and the minimum rate-limit handling in 6.5.
- **Phase 2:** the rest of Requirements 7, 8, 11 and 12 (key helpers and keyring, provider selection, retry and backoff, batch progress).
- **Phase 3:** additional providers: Requirements 5 (OSRM), 3 (Google Maps), 4 (HERE).
- **Phase 4:** Requirement 10 (CRAN readiness), including documenting the older functions and datasets.

Requirement 2 (PC Miler) is not scheduled; see Note A.
