# truckR

Driving distance and time for freight lanes, with an optional waypoint, plus
helpers for reading transportation bid reports and for the PC*MILER service.

```r
library(truckR)
```

## Setting up the API key

Routes are looked up with [OpenRouteService](https://openrouteservice.org/)
(ORS), which has a free tier. Get a key from your ORS account, then add this
line to your user `.Renviron` file (open it with `usethis::edit_r_environ()`)
and restart R:

```
TRUCKR_ORS_API_KEY=your_key_here
```

The key is never stored in the package, never shown in messages and never
written to the saved answers. Check that R can see it (this does not show it):

```r
truckr_has_key("ors")
```

## Routing lanes

`route()` takes a table of lanes with these columns: `orig_city`,
`orig_state`, `orig_zip`, `orig_ctry`, `dest_city`, `dest_state`, `dest_zip`
and `dest_ctry`. It returns the same table, in the same order, with these
columns added (distances in miles, durations in hours):

```r
out <- route(df_route_samples[1:5, ])
out[, c("orig_city", "dest_city", "distance_routed", "duration_routed", "route_error")]
```

| Column | Meaning |
|---|---|
| `distance_direct`, `duration_direct` | Shortest route from origin to destination |
| `distance_routed`, `duration_routed` | The route as asked for (through the waypoint, if there is one) |
| `has_waypoint` | Whether the lane used a waypoint |
| `route_error` | Empty, or why a lane has no numbers (or a warning about them) |
| `geocode_flag` | Empty, or a note about how a place was found |
| `geocode_check` | `TRUE` when a place may be the wrong one: look at `geocode_flag` and check the distance |
| `provider_name`, `RouteId` | The provider used, and an id shared by identical lanes |

A lane that cannot be routed has empty numbers and a reason in `route_error`.
It does not stop the other lanes.

## Going through a waypoint

Add a logical `Use_Waypoint` column to mark the lanes that go through the
waypoint, and give the waypoint once. For example, lanes from Mexico to the
United States that cross at Laredo:

```r
lanes$Use_Waypoint <- TRUE
out <- route(lanes, waypoint = "78040")        # a zip code
# or: waypoint = "Laredo, TX"                  # a city and state
# or: waypoint = c(lat = 27.5, lon = -99.5)    # coordinates
```

Only one waypoint is supported. When any lane uses it, four more columns are
added: `waypoint_value`, `waypoint_type`, `detour_added` (miles) and
`detour_duration_added` (hours), empty for lanes that do not use it. A
waypoint lane's `distance_routed` is the sum of its two legs (origin to
waypoint, waypoint to destination), so it can differ a little from a true
through-route.

## Limits, and saved answers

The free ORS service allows about 2,000 routes and 3,000 place lookups per
day, and about 40 requests per minute, so a long run is slow. Each distinct
place and each distinct route is requested only once, and every answer is
saved on disk, so running the same lanes again costs nothing. Answers older
than 120 days are looked up again.

`route()` says up front how many requests it needs and how many remain. If the
daily limit is reached part way, it stops cleanly, keeps what it has and marks
the lanes that are missing ("quota exhausted; re-run after reset"). Run it
again after the quota resets to finish them.

```r
truckr_cache_info()    # what is saved
truckr_cache_clear()   # throw it away
```

Behavior can be changed with options, for example
`options(truckr.cache = "session")` to keep nothing on disk, or
`options(truckr.geocode_doubtful = "reject")` to get no distance, rather than
a flagged one, when a place cannot be found with confidence. See `?route` and
`?truckr_cache_info`.

## Providers

ORS is built in and is the active provider. Other providers can be added with
`register_provider()` and chosen with `truckr_set_provider()`.

## Other functions

`read_BidReport2()` and `ingest_bidreports()` read bid report files. The
`pcm_*` functions send lanes to PC*MILER; they were written first and are not
covered by the tests.

## Attribution

Routes and places from ORS are based on OpenStreetMap data and are licensed
under CC-BY-SA 4.0. If results are shared outside the company, credit them as
"openrouteservice.org | OpenStreetMap contributors"; the share-alike terms may
also apply. (ORS staff have confirmed on their forum that results may be
stored, but this is not legal advice and the formal terms of service should be
checked before wider sharing.)
