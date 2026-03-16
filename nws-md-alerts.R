library(httr2)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)

# ── 1. Fetch active alerts for Maryland ──────────────────────────────────────
alerts_raw <- request("https://api.weather.gov/alerts/active") |>
  req_url_query(area = "MD") |>
  req_headers(Accept = "application/geo+json") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

alerts_df <- as.data.frame(alerts_raw$features$properties)

if (nrow(alerts_df) == 0) stop("No active alerts for Maryland.")

# ── 2. Explode affectedZones into one row per zone URL ────────────────────────
alerts_long <- alerts_df |>
  select(id, event, headline, description, severity,
         urgency, certainty, effective, expires, affectedZones) |>
  unnest(affectedZones) |>
  rename(zone_url = affectedZones)

# ── 3. Load local zone cache and join geometries ──────────────────────────────
zones_sf <- st_read("nws_zones_cache/md_zones.gpkg", layer = "zones", quiet = TRUE)

alerts_sf <- alerts_long |>
  left_join(
    zones_sf |> select(zone_url, zone_id, zone_name, zone_type),
    by = "zone_url"
  ) |>
  filter(!is.na(zone_id)) |>        # drop unmatched zones (VA, etc.)
  st_as_sf()

# ── 4. Dissolve multi-zone alerts into one polygon per alert ──────────────────
alerts_dissolved <- alerts_sf |>
  group_by(id, event, headline, severity, expires) |>
  summarise(.groups = "drop") |>
  st_as_sf()


# ── 5. Export as GeoJSON ──────────────────────────────────────────────────────
st_write(alerts_dissolved, "md_alerts.geojson", driver = "GeoJSON", delete_dsn = TRUE)