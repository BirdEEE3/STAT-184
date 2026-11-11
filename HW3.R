library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

dir.create("output", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)


p_a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ drv) +
  scale_x_continuous(limits = c(NA, 5)) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)") +
  theme_bw()
print(p_a)

ggsave("output/p1a_displ_hwy_by_drv.png", p_a, width = 9, height = 4, dpi = 150)


p_b_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count") +
  theme_bw()
print(p_b_count)
ggsave("output/p1b_count.png", p_b_count, width = 5, height = 4, dpi = 150)

p_b_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type") +
  theme_bw()
print(p_b_prop)
ggsave("output/p1b_proportion.png", p_b_prop, width = 5, height = 4, dpi = 150)

drv_sizes <- vehicles %>% count(drv, name = "n_cars")
print(drv_sizes)


carat_max <- 3
gems_small <- gems %>% filter(carat <= carat_max)

carat_records <- tibble(
  table = c("gems (original)", "gems_small (carat <= 3)", "excluded (carat > 3)"),
  n_records = c(nrow(gems), nrow(gems_small), nrow(gems) - nrow(gems_small))
)
print(carat_records)

x_scale <- scale_x_continuous(name = "Carat", limits = c(0, carat_max),
                              breaks = seq(0, carat_max, by = 0.5))

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right",
                 fill = "grey30", color = NA) +
  x_scale +
  labs(title = "Diamond carat, bin width = 0.01",
       subtitle = paste0("gems_small: carat <= 3, n = ", format(nrow(gems_small), big.mark = ",")),
       y = "Count of diamonds") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right",
                 fill = "grey30", color = "white") +
  x_scale +
  labs(title = "Diamond carat, bin width = 0.5",
       subtitle = paste0("gems_small: carat <= 3, n = ", format(nrow(gems_small), big.mark = ",")),
       y = "Count of diamonds") +
  theme_bw()

print(p_fine)
print(p_coarse)
ggsave("output/p1c_fine.png", p_fine, width = 8, height = 4, dpi = 150)
ggsave("output/p1c_coarse.png", p_coarse, width = 8, height = 4, dpi = 150)

carat_099_100 <- gems %>%
  mutate(carat_2dp = round(carat, 2)) %>%
  filter(carat_2dp %in% c(0.99, 1.00)) %>%
  count(carat_2dp, name = "n_diamonds")
print(carat_099_100)

# Answer: 23 diamonds are exactly 0.99 carat and 1,558 are exactly 1.00 carat.
# Narrow bins show sharp spikes at or just above round weights (0.3, 0.5, 0.7, 1.0, 1.5, 2.0) with very few stones just below them.

projected_crs <- 26916

lib_url <- "https://data.cityofchicago.org/resource/x8fc-8rcq.json?$limit=50000"
ca_url <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson?$limit=50000"
lib_raw_file <- "data/raw/chicago_libraries_x8fc-8rcq.json"
ca_raw_file <- "data/raw/chicago_community_areas_igwz-8jzy.geojson"

if (!file.exists(lib_raw_file)) download.file(lib_url, lib_raw_file, mode = "wb")
if (!file.exists(ca_raw_file)) download.file(ca_url, ca_raw_file, mode = "wb")

data_sources <- tibble(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  role = c("facility points (Problems 2 and 3)", "area boundaries (Problem 3)"),
  publisher = c("City of Chicago / Chicago Public Library", "City of Chicago"),
  portal_page = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                  "https://data.cityofchicago.org/d/igwz-8jzy"),
  download_url = c(lib_url, ca_url),
  dataset_id = c("x8fc-8rcq", "igwz-8jzy"),
  local_raw_file = c(lib_raw_file, ca_raw_file),
  format = c("JSON (Socrata SODA API), location field with latitude/longitude in WGS84",
             "GeoJSON, MultiPolygon, WGS84 (EPSG:4326)"),
  retrieved = c("2026-10-10", "2026-10-07"),
  license = c("City of Chicago Data Portal Terms of Use", "City of Chicago Data Portal Terms of Use"),
  notes = c("Single API request with $limit far above the record count, so no pagination cut-off",
            "77 official community areas; boundaries are static and not dated to the library snapshot")
)
write_csv(data_sources, "data_sources.csv")
print(data_sources %>% select(dataset, dataset_id, retrieved))


lib_raw <- jsonlite::fromJSON(lib_raw_file, flatten = TRUE) %>% as_tibble()
n_raw <- nrow(lib_raw)

lib_checked <- lib_raw %>%
  transmute(
    raw_row = row_number(),
    site_name = trimws(branch_),
    address = trimws(address),
    zip = zip,
    lat = suppressWarnings(as.numeric(location.latitude)),
    lon = suppressWarnings(as.numeric(location.longitude))
  ) %>%
  mutate(
    coord_missing = is.na(lat) | is.na(lon),
    coord_zero = !coord_missing & (lat == 0 | lon == 0),
    coord_swapped = !coord_missing & lat < -80 & lon > 30,
    coord_outside_chicago = !coord_missing & !coord_zero &
      !(lat >= 41.6 & lat <= 42.1 & lon >= -88.0 & lon <= -87.5),
    name_missing = is.na(site_name) | site_name == ""
  )

exact_dupes <- lib_checked %>%
  group_by(site_name, address, lat, lon) %>%
  filter(n() > 1) %>%
  ungroup()

shared_coords <- lib_checked %>%
  filter(!coord_missing) %>%
  group_by(lat, lon) %>%
  filter(n() > 1) %>%
  ungroup()

duplicate_names <- lib_checked %>%
  group_by(site_name) %>%
  filter(n() > 1) %>%
  ungroup()

cleaning_audit <- tibble(
  check = c("raw records",
            "missing or non-numeric coordinates",
            "zero coordinates",
            "latitude/longitude swapped",
            "outside Chicago bounding box",
            "missing facility name",
            "exact duplicate records (name + address + coordinates)",
            "records sharing coordinates with another record (kept)",
            "records sharing a name with another record"),
  n_records = c(n_raw,
                sum(lib_checked$coord_missing),
                sum(lib_checked$coord_zero),
                sum(lib_checked$coord_swapped),
                sum(lib_checked$coord_outside_chicago),
                sum(lib_checked$name_missing),
                nrow(exact_dupes),
                nrow(shared_coords),
                nrow(duplicate_names))
)
print(cleaning_audit)

removals <- lib_checked %>%
  mutate(removal_reason = case_when(
    coord_missing ~ "missing or non-numeric coordinates",
    coord_zero ~ "zero coordinates",
    coord_swapped ~ "latitude/longitude swapped",
    coord_outside_chicago ~ "coordinates outside Chicago",
    name_missing ~ "missing facility name",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(removal_reason)) %>%
  select(raw_row, site_name, address, lat, lon, removal_reason)
print(removals)

lib_clean <- lib_checked %>%
  filter(!raw_row %in% removals$raw_row) %>%
  arrange(site_name, raw_row) %>%
  mutate(site_id = sprintf("LIB%03d", row_number())) %>%
  select(site_id, site_name, address, zip, lat, lon, raw_row)

record_counts <- tibble(
  stage = c("raw", "removed", "cleaned"),
  n_records = c(n_raw, nrow(removals), nrow(lib_clean))
)
print(record_counts)

# Answer: 82 raw records
stopifnot(nrow(lib_clean) == n_raw - nrow(removals),
          !anyDuplicated(lib_clean$site_id),
          nrow(lib_clean) >= 20)

write_csv(lib_clean, "data/clean/chicago_libraries_clean.csv")

sites_m <- lib_clean %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(projected_crs)

crs_info <- tibble(
  object = "sites_m",
  epsg = st_crs(sites_m)$epsg,
  crs_name = st_crs(sites_m)$Name,
  units = st_crs(sites_m)$units_gdal
)
print(crs_info)

community_areas <- st_read(ca_raw_file, quiet = TRUE) %>% st_transform(projected_crs)

lib_caption <- paste0(
  "Source: City of Chicago Data Portal, Libraries - Locations (x8fc-8rcq), retrieved 2026-10-10;\n",
  "Boundaries - Community Areas (igwz-8jzy), retrieved 2026-10-07.\n",
  "Projection: NAD83 / UTM zone 16N (EPSG:26916), metres."
)

p_sites <- ggplot() +
  geom_sf(data = community_areas, aes(color = "Community area boundary"),
          fill = "grey97", linewidth = 0.2) +
  geom_sf(data = sites_m, aes(shape = "Library location"),
          color = "#1f4e9c", size = 1.8) +
  scale_color_manual(name = NULL, values = c("Community area boundary" = "grey55")) +
  scale_shape_manual(name = NULL, values = c("Library location" = 16)) +
  labs(title = "Chicago Public Library locations",
       subtitle = paste0(nrow(sites_m), " retained library records"),
       caption = lib_caption) +
  theme_bw() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
print(p_sites)
ggsave("output/p_sites.png", p_sites, width = 7, height = 8.5, dpi = 150)

buffer_500 <- sites_m %>% st_buffer(dist = 500) %>% st_union() %>% st_make_valid()
buffer_1000 <- sites_m %>% st_buffer(dist = 1000) %>% st_union() %>% st_make_valid()

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buffer_500)) / 1e6,
                     as.numeric(st_area(buffer_1000)) / 1e6)
)
print(buffer_summary)
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])

buffer_zones <- st_sf(radius_m = c(500, 1000),
                      geometry = c(buffer_500, buffer_1000))
st_write(sites_m, "output/problem2_libraries.gpkg", layer = "sites_m",
         delete_dsn = TRUE, quiet = TRUE)
st_write(buffer_zones, "output/problem2_libraries.gpkg", layer = "buffer_union",
         append = TRUE, quiet = TRUE)
write_csv(buffer_summary, "output/buffer_summary.csv")

# Answer: Merged buffers : 64.31 km2 at 500 m, 250.11 km2 at 1,000 m.

p_buffers <- ggplot() +
  geom_sf(data = community_areas, fill = "grey97", color = "grey60", linewidth = 0.2) +
  geom_sf(data = st_sf(geometry = buffer_500), aes(fill = "Within 500 m of a library (combined)"),
          color = "#d95f02", alpha = 0.45, linewidth = 0.2) +
  geom_sf(data = sites_m, aes(shape = "Library location"), color = "#1f4e9c", size = 1.2) +
  scale_fill_manual(name = NULL, values = c("Within 500 m of a library (combined)" = "#fdae6b")) +
  scale_shape_manual(name = NULL, values = c("Library location" = 16)) +
  labs(title = "Area within 500 metres of a Chicago Public Library",
       subtitle = paste0(nrow(sites_m), " libraries; combined 500 m buffer = ",
                         round(buffer_summary$union_area_km2[1], 1), " km2 (overlaps counted once)"),
       caption = lib_caption) +
  theme_bw() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
print(p_buffers)
ggsave("output/p_buffers.png", p_buffers, width = 7, height = 8.5, dpi = 150)


districts_raw <- st_read(ca_raw_file, quiet = TRUE)

boundary_audit <- tibble(
  check = c("features", "missing IDs", "duplicate IDs", "missing names",
            "duplicate names", "empty geometries", "invalid geometries"),
  n = c(nrow(districts_raw),
        sum(is.na(districts_raw$area_numbe) | districts_raw$area_numbe == ""),
        sum(duplicated(districts_raw$area_numbe)),
        sum(is.na(districts_raw$community) | districts_raw$community == ""),
        sum(duplicated(districts_raw$community)),
        sum(st_is_empty(districts_raw)),
        sum(!st_is_valid(districts_raw)))
)
print(boundary_audit)

# Answer: 77 community areas. The boundaries are a fixed official geography
districts <- districts_raw %>%
  st_transform(projected_crs) %>%
  st_make_valid() %>%
  transmute(district_id = as.integer(area_numbe),
            district_name = tools::toTitleCase(tolower(community))) %>%
  arrange(district_id)

stopifnot(st_crs(districts) == st_crs(sites_m),
          all(st_is_valid(districts)),
          !anyDuplicated(districts$district_id),
          !any(is.na(districts$district_id)),
          all(st_geometry_type(districts) %in% c("POLYGON", "MULTIPOLYGON")))

p_overlay <- ggplot() +
  geom_sf(data = districts, fill = "grey97", color = "grey50", linewidth = 0.25) +
  geom_sf(data = sites_m, color = "#1f4e9c", size = 1.2) +
  labs(title = "Libraries over Chicago community areas",
       subtitle = paste0(nrow(sites_m), " libraries, ", nrow(districts), " community areas"),
       caption = lib_caption) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, size = 7))
print(p_overlay)
ggsave("output/p_overlay.png", p_overlay, width = 7, height = 8.5, dpi = 150)

match_list <- st_intersects(sites_m, districts)

site_match_audit <- sites_m %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  transmute(site_id, site_name,
            n_matches = lengths(match_list),
            matched_district_ids = vapply(match_list,
                                          function(i) paste(districts$district_id[i], collapse = ";"),
                                          character(1)))
write_csv(site_match_audit, "site_match_audit.csv")

match_summary <- site_match_audit %>%
  mutate(match_type = case_when(n_matches == 0 ~ "zero areas",
                                n_matches == 1 ~ "exactly one area",
                                TRUE ~ "multiple areas")) %>%
  count(match_type, name = "n_sites")
print(match_summary)

not_exactly_one <- site_match_audit %>% filter(n_matches != 1)
print(not_exactly_one)

# Answer: All 82 libraries match exactly one area none unassined.

assigned_sites <- site_match_audit %>%
  filter(n_matches == 1) %>%
  mutate(district_id = as.integer(matched_district_ids))
n_assigned <- nrow(assigned_sites)
n_unassigned <- nrow(site_match_audit) - n_assigned

district_counts <- districts %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  left_join(assigned_sites %>% count(district_id, name = "n_sites"), by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

stopifnot(nrow(district_counts) == nrow(districts),
          all(district_counts$area_km2 > 0),
          sum(district_counts$n_sites) == n_assigned,
          !any(is.na(district_counts$n_sites)))

district_counts_table <- district_counts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  arrange(desc(n_sites), district_id)
print(district_counts_table, n = Inf)
write_csv(district_counts_table, "output/district_counts.csv")

top_by_density <- district_counts_table %>% arrange(desc(sites_per_km2)) %>% head(10)
print(top_by_density)

# Answer: Five areas tie with 3 libraries (Humboldt, Portage, New City, West Side, Austin) and 16 areas
# have none. By density, Douglas leads and small one-branch areas rise, while Austin falls to 29th of 77 because of
# its large size, so accounting for area size does change the comparison.

map_bbox <- st_bbox(districts)
map_coords <- coord_sf(xlim = map_bbox[c("xmin", "xmax")],
                       ylim = map_bbox[c("ymin", "ymax")],
                       crs = st_crs(projected_crs), expand = TRUE)

count_caption <- paste0(
  lib_caption, "\n",
  n_assigned, " of ", nrow(sites_m), " libraries assigned to exactly one community area; ",
  n_unassigned, " left unassigned.\nZero-count areas are outlined in red; grey fill would mark missing values."
)

zero_areas <- district_counts %>% filter(n_sites == 0)

p_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites), color = "grey40", linewidth = 0.2) +
  geom_sf(data = zero_areas, aes(color = "0 libraries"), fill = NA, linewidth = 0.6) +
  scale_fill_distiller(name = "Libraries\n(count)", palette = "Blues", direction = 1,
                       limits = c(0, max(district_counts$n_sites)),
                       breaks = 0:max(district_counts$n_sites), na.value = "grey60") +
  scale_color_manual(name = NULL, values = c("0 libraries" = "#c0392b")) +
  map_coords +
  labs(title = "Number of libraries per Chicago community area",
       caption = count_caption) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, size = 6.5))
print(p_count)
ggsave("output/p_count.png", p_count, width = 7, height = 8.5, dpi = 150)

p_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2), color = "grey40", linewidth = 0.2) +
  geom_sf(data = zero_areas, aes(color = "0 libraries per km2"), fill = NA, linewidth = 0.6) +
  scale_fill_distiller(name = "Libraries\nper km2", palette = "Purples", direction = 1,
                       limits = c(0, max(district_counts$sites_per_km2)), na.value = "grey60") +
  scale_color_manual(name = NULL, values = c("0 libraries per km2" = "#c0392b")) +
  map_coords +
  labs(title = "Libraries per square kilometre by Chicago community area",
       caption = count_caption) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, size = 6.5))
print(p_density)
ggsave("output/p_density.png", p_density, width = 7, height = 8.5, dpi = 150)
