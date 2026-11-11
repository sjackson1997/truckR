# truckR 0.2.0

* New `route()`: driving distance and duration for lanes, with an optional
  single waypoint (`Use_Waypoint` column and `waypoint` argument). Lanes are
  split into point-to-point segments and each distinct place and route is
  looked up once.
* New OpenRouteService provider (free tier), registered on load. Provider
  functions: `register_provider()`, `truckr_set_provider()`,
  `truckr_get_provider()`, `truckr_list_providers()`.
* Answers are saved in memory and on disk (120 days); see
  `truckr_cache_info()` and `truckr_cache_clear()`. Runs stop cleanly when the
  daily quota is used up and can be repeated to finish.
* Places that may be wrong are returned with a distance and marked
  (`geocode_check`, `geocode_flag`).
* API keys are read from `TRUCKR_<PROVIDER>_API_KEY`; see `truckr_has_key()`.
* New: `read_BidReport2()`, `ingest_bidreports()`.
* The PC*MILER functions no longer contain an API key and take the key and
  URL as arguments.
* Documented the PC*MILER functions and the example data sets.

# truckR 0.1.0

* First version.
