# Africa Nutrient Map — Pie Charts per Country
# 6 separate PNG files, one per nutrient
# Pie size= % RDA met (from aqua + cap + imp) in 2019 (Namibia: 2020)
# Pie slices   = % contribution of each source to total supply
#                (aquaculture / capture / import as share of total)

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

setwd("/Users/temi/Desktop/Consumption_Rpackage")

#set
year_exceptions <- list(
  "Namibia" = c(late = 2020)   # use 2020 instead of 2019
)

# Nutrient specs: column suffix, display label, fixed_max_rdi for size scaling
nutrient_specs <- list(
  list(col = "protein_g",      label = "Protein",      max_rdi = 30),
  list(col = "calcium_mg",     label = "Calcium",      max_rdi = 30),
  list(col = "iron_mg",        label = "Iron",         max_rdi = 30),
  list(col = "zinc_mg",        label = "Zinc",         max_rdi = 30),
  list(col = "vitamina_mcg",   label = "Vitamin A",    max_rdi = 30),
  list(col = "vitaminb12_mcg", label = "Vitamin B12",  max_rdi = 30)
)

source_colors <- c(
  "Aquaculture" = "#1f78b4",
  "Capture"     = "#33a02c",
  "Import"      = "#e31a1c"
)

min_size <- 0.5
max_size <- 2.4

#load data

df_aqua       <- read.csv("artis_aquaculture.csv",  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_cap        <- read.csv("artis_capture.csv",      header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_imp        <- read.csv("artis_import.csv",       header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
nutrient_data <- read.csv("edible_nutrient.csv",    header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
pop_data      <- read.csv("Total_population.csv",   header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
rda_data_raw  <- read.csv(
  "/Users/temi/Desktop/Consumption Analysis/population_country_rda_all_countries.csv",
  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA"
)

name_to_iso <- pop_data %>% select(iso3, country) %>% distinct()

#country coordinates

country_coords <- tribble(
  ~country,                        ~lon,        ~lat,
  "Senegal",                       -14.452362,  14.497401,
  "Nigeria",                         8.675277,   9.081999,
  "Mauritania",                    -10.940835,  21.007890,
  "Egypt",                          30.802498,  26.820553,
  "Ghana",                          -1.023194,   7.946527,
  "Cote d'Ivoire",                  -5.547080,   7.539989,
  "Gambia",                        -15.310139,  13.443182,
  "Guinea",                         -9.696645,   9.945587,
  "Benin",                           2.315834,   9.307690,
  "Guinea-Bissau",                 -15.180413,  11.803749,
  "Liberia",                        -9.429499,   6.428055,
  "Sierra Leone",                  -11.779889,   8.460555,
  "Togo",                            0.824782,   8.619543,
  "Tunisia",                         9.537499,  33.886917,
  "Uganda",                         32.290275,   1.373333,
  "Tanzania",                       34.888822,  -6.369028,
  "South Africa",                   22.937506, -30.559482,
  "Libya",                          17.228331,  26.335100,
  "Mozambique",                     35.529562, -18.665695,
  "Namibia",                        18.490410, -22.957640,
  "Gabon",                          11.609444,  -0.803689,
  "Cameroon",                       12.354722,   7.369722,
  "Cape Verde",                    -24.013197,  16.002082,
  "Morocco",                        -7.092620,  31.791702,
  "Democratic Republic of Congo",   21.758664,  -4.038333,
  "Congo",                          15.827659,  -0.228021,
  "Angola",                         17.873887, -11.202692,
  "Djibouti",                       42.590275,  11.825138,
  "Algeria",                         1.659626,  28.033886,
  "Eritrea",                        39.782334,  15.179384,
  "Kenya",                          37.906193,  -0.023559,
  "Madagascar",                     46.500000, -20.000000,
  "Mauritius",                      57.552152, -20.348404,
  "Sudan",                          30.217636,  12.862807,
  "Somalia",                        46.199616,   5.152149
)

# nutrient per capita by source for 1996
# Late year = 2019 for all countries, 2020 for Namibia

nutrient_cols <- c("vitamina_mcg", "vitaminb12_mcg", "calcium_mg",
                   "iron_mg", "zinc_mg", "protein_g")

percap_consumer_all <- function(df, src_name) {
  df %>%
    group_by(sciname, year, consumer_iso3c) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(nutrient_data, by = "sciname") %>%
    mutate(
      consumption_edible_t = consumption_live_t * edible,
      vitamina_mcg         = consumption_edible_t * vitamina_mcg   / 0.0001,
      vitaminb12_mcg       = consumption_edible_t * vitaminb12_mcg / 0.0001,
      calcium_mg           = consumption_edible_t * calcium_mg     / 0.0001,
      iron_mg              = consumption_edible_t * iron_mg        / 0.0001,
      zinc_mg              = consumption_edible_t * zinc_mg        / 0.0001,
      protein_g            = consumption_edible_t * protein_g      / 0.0001
    ) %>%
    group_by(year, consumer_iso3c) %>%
    summarize(across(all_of(nutrient_cols), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    left_join(pop_data, by = c("consumer_iso3c" = "iso3", "year" = "Year")) %>%
    mutate(across(all_of(nutrient_cols), ~ .x / population)) %>%
    mutate(iso3c = consumer_iso3c, source = src_name) %>%
    select(iso3c, year, source, all_of(nutrient_cols))
}

aqua_pc <- percap_consumer_all(df_aqua, "Aquaculture")
cap_pc  <- percap_consumer_all(df_cap,  "Capture")
imp_pc  <- percap_consumer_all(df_imp,  "Import")

all_pc <- bind_rows(aqua_pc, cap_pc, imp_pc)

# For each country, determine its late year
namibia_iso <- name_to_iso$iso3[name_to_iso$country == "Namibia"]

all_pc_late <- all_pc %>%
  mutate(late_year = if_else(iso3c == namibia_iso, 2020L, 2019L)) %>%
  filter(year == late_year)

cat("Rows in late-year data:", nrow(all_pc_late), "\n")

# total supply and each source contribution
# Total supply per country per nutrient (sum across sources)
supply_total <- all_pc_late %>%
  group_by(iso3c) %>%
  summarize(across(all_of(nutrient_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  rename_with(~ paste0("total_", .), all_of(nutrient_cols))

# Source contributions as % of total supply — computed explicitly per nutrient
contributions <- all_pc_late %>%
  left_join(supply_total, by = "iso3c") %>%
  mutate(
    pct_protein_g      = if_else(total_protein_g      > 0, protein_g      / total_protein_g      * 100, NA_real_),
    pct_calcium_mg     = if_else(total_calcium_mg     > 0, calcium_mg     / total_calcium_mg     * 100, NA_real_),
    pct_iron_mg        = if_else(total_iron_mg        > 0, iron_mg        / total_iron_mg        * 100, NA_real_),
    pct_zinc_mg        = if_else(total_zinc_mg        > 0, zinc_mg        / total_zinc_mg        * 100, NA_real_),
    pct_vitamina_mcg   = if_else(total_vitamina_mcg   > 0, vitamina_mcg   / total_vitamina_mcg   * 100, NA_real_),
    pct_vitaminb12_mcg = if_else(total_vitaminb12_mcg > 0, vitaminb12_mcg / total_vitaminb12_mcg * 100, NA_real_)
  ) %>%
  select(iso3c, source, starts_with("pct_"))

# %RDA met in 1996

rda_lookup <- rda_data_raw %>%
  left_join(name_to_iso, by = c("Country" = "country")) %>%
  rename(iso3c = iso3) %>%
  # Year=2019 slot holds 2020 data for Namibia — alignment works automatically
  filter(Year == 2019) %>%
  select(
    iso3c,
    rda_protein_g      = PerCapita_Protein_g,
    rda_calcium_mg     = PerCapita_Calcium_mg,
    rda_iron_mg        = PerCapita_Iron_mg,
    rda_vitamina_mcg   = PerCapita_VitA_ug,
    rda_vitaminb12_mcg = PerCapita_VitB12_ug,
    rda_zinc_mg        = PerCapita_Zinc_mg
  )

pct_rda_met <- supply_total %>%
  left_join(rda_lookup, by = "iso3c") %>%
  mutate(
    pct_rda_protein_g      = total_protein_g      / (rda_protein_g      * 365) * 100,
    pct_rda_calcium_mg     = total_calcium_mg     / (rda_calcium_mg     * 365) * 100,
    pct_rda_iron_mg        = total_iron_mg        / (rda_iron_mg        * 365) * 100,
    pct_rda_vitamina_mcg   = total_vitamina_mcg   / (rda_vitamina_mcg   * 365) * 100,
    pct_rda_vitaminb12_mcg = total_vitaminb12_mcg / (rda_vitaminb12_mcg * 365) * 100,
    pct_rda_zinc_mg        = total_zinc_mg        / (rda_zinc_mg        * 365) * 100
  ) %>%
  select(iso3c, starts_with("pct_rda_"))

#combine and join coordinates

plot_data <- contributions %>%
  left_join(pct_rda_met, by = "iso3c") %>%
  left_join(name_to_iso, by = c("iso3c" = "iso3")) %>%
  left_join(country_coords, by = "country") %>%
  filter(!is.na(lon), !is.na(lat))

cat("Countries with coordinates:", n_distinct(plot_data$iso3c), "\n")

# Africa basemap

africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")

#draw one pie per country
# Uses ggplot2 annotation_custom with a grob, or we build wedges manually
# using geom_arc_bar from ggforce (if available) or draw with polygons.
# We use a simple approach: convert pie wedges to polygon coordinates.

wedge_polygon <- function(cx, cy, r, start_deg, end_deg, n = 50) {
  angles <- seq(start_deg, end_deg, length.out = n) * pi / 180
  data.frame(
    x = c(cx, cx + r * cos(angles), cx),
    y = c(cy, cy + r * sin(angles), cy)
  )
}

draw_nutrient_map <- function(spec) {

  nut_col      <- spec$col
  nut_label    <- spec$label
  max_rdi      <- spec$max_rdi
  pct_rda_col  <- paste0("pct_rda_", nut_col)
  pct_src_col  <- paste0("pct_", nut_col)

  # Build wedge polygons for all countries
  wedge_list <- list()

  countries_in_data <- unique(plot_data$iso3c)

  for (iso in countries_in_data) {

    ctry_rows <- plot_data %>% filter(iso3c == iso)
    if (nrow(ctry_rows) == 0) next

    lon_c  <- ctry_rows$lon[1]
    lat_c  <- ctry_rows$lat[1]
    rdi    <- ctry_rows[[pct_rda_col]][1]

    if (!is.finite(rdi) || !is.finite(lon_c) || !is.finite(lat_c)) next

    radius <- min_size + (min(abs(rdi), max_rdi) / max_rdi) * (max_size - min_size)

    start_angle <- 90   # start from top (12 o'clock)

    for (src in c("Aquaculture", "Capture", "Import")) {

      src_row <- ctry_rows %>% filter(source == src)
      if (nrow(src_row) == 0) next

      pct_val <- src_row[[pct_src_col]]
      if (!is.finite(pct_val) || pct_val <= 0) next

      sweep <- pct_val * 3.6   # % -> degrees
      end_angle <- start_angle - sweep   # clockwise

      poly <- wedge_polygon(lon_c, lat_c, radius, end_angle, start_angle)
      poly$source  <- src
      poly$iso3c   <- iso
      poly$country <- ctry_rows$country[1]
      poly$rdi     <- rdi
      poly$group   <- paste(iso, src, sep = "_")

      wedge_list[[length(wedge_list) + 1]] <- poly
      start_angle <- end_angle
    }
  }

  if (length(wedge_list) == 0) {
    cat("No wedge data for", nut_label, "\n")
    return(invisible(NULL))
  }

  wedge_df <- bind_rows(wedge_list) %>%
    mutate(source = factor(source, levels = c("Aquaculture", "Capture", "Import")))

  # Label data (one row per country)
  label_df <- plot_data %>%
    select(iso3c, country, lon, lat, all_of(pct_rda_col)) %>%
    distinct() %>%
    rename(rdi = all_of(pct_rda_col)) %>%
    filter(is.finite(lon), is.finite(lat), is.finite(rdi))

  # Draw
  p <- ggplot() +
    geom_sf(data = africa, fill = "whitesmoke", color = "grey60",
            linewidth = 0.3) +
    geom_polygon(data = wedge_df,
                 aes(x = x, y = y, group = group, fill = source),
                 color = "black", linewidth = 0.2) +
    geom_text(data = label_df,
              aes(x = lon, y = lat - 0.8,
                  label = paste0(round(rdi, 0), "%")),
              size = 3, fontface = "bold", color = "darkblue",
              vjust = 1) +
    scale_fill_manual(values = source_colors, name = "Source") +
    coord_sf(xlim = c(-30, 65), ylim = c(-40, 40), expand = FALSE) +
    labs(title = nut_label,
         caption = paste0("Pie size = % RDA met | Slices = % contribution by source\n",
                          "Year: 2019 (Namibia: 2020)")) +
    theme_void(base_size = 14) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      plot.title       = element_text(size = 20, face = "bold", hjust = 0.5,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(size = 10, hjust = 0.5, color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_text(size = 12, face = "bold"),
      legend.text      = element_text(size = 11)
    )

  out_file <- paste0("Map_", gsub(" ", "_", nut_label), "_RDA_pie.png")
  ggsave(out_file, plot = p, width = 14, height = 14, dpi = 300, bg = "white")
  cat("Saved:", out_file, "\n")
}

#draw all six maps

for (spec in nutrient_specs) {
  draw_nutrient_map(spec)
}

