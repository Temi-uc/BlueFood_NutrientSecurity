
# Panel a: (capture_2019 - capture_1996) + (aqua_2019 - aqua_1996)  [stacked]
# Panel b: (import_2019 - import_1996) - (export_2019 - export_1996) [net trade]
# Panel c: %RDA met (aqua+cap+imp) / (PerCapita_Protein_g * 365) * 100, diff 2019-1996
#
# Namibia exception: 2001 = early year, 2020 = late year
# Export filter: consumption_source == "foreign step 1" only

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

setwd("/Users/temi/Desktop/Consumption_Rpackage")

# country order 

desired_order <- c(
  "Senegal", "Namibia", "Gambia", "Ghana", "Morocco", "Algeria", "South Africa",
  "Kenya", "Eritrea", "Madagascar", "Tanzania", "Somalia", "DR of Congo", "Sudan",
  "Mozambique", "Cape Verde", "Liberia", "Djibouti", "GuineaBissau", "Angola", "Togo",
  "Nigeria", "Cameroon", "Mauritania", "Uganda", "Libya", "Benin", "Tunisia", "Guinea",
  "SierraLeone", "CotedIvoire", "Egypt", "Congo", "Mauritius", "Gabon"
)

# Countries that use alternate years instead of 1996 / 2019
year_exceptions <- list(
  "Namibia" = c(early = 2001, late = 2020)
)

# Load data
df_aqua       <- read.csv("artis_aquaculture.csv",  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_cap        <- read.csv("artis_capture.csv",      header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_imp        <- read.csv("artis_import.csv",       header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_exp        <- read.csv("artis_export.csv",       header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
nutrient_data <- read.csv("edible_nutrient.csv",    header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
pop_data      <- read.csv("Total_population.csv",   header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
rda_data_raw  <- read.csv(
  "/Users/temi/Desktop/Consumption Analysis/population_country_rda_all_countries.csv",
  header = TRUE, stringsAsFactors = FALSE, na.strings = "NA"
)

# iso3c <-> country name lookup (used at the end for plot labels only)
name_to_iso <- pop_data %>% select(iso3, country) %>% distinct()

# Export filtering
# Keep only rows where consumption_source == "foreign step 1"

df_exp_filtered <- df_exp %>% filter(consumption_source == "foreign step 1")
cat("Export rows after filter:", nrow(df_exp_filtered), "\n")

# protein per capita for consumer (Aquaculture, fisheries and import)
#   sum live tonnes by species / year / country
#   join nutrient table - compute edible tonnes - compute protein grams
#   sum all species - one protein total per country per year
#   divide by population - grams per person per year

percap_consumer <- function(df, src_name) {

  df %>%
    group_by(sciname, year, consumer_iso3c) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(nutrient_data, by = "sciname") %>%
    mutate(
      consumption_edible_t = consumption_live_t * edible,
      protein_g            = consumption_edible_t * protein_g / 0.0001
    ) %>%
    group_by(year, consumer_iso3c) %>%
    summarize(protein_g = sum(protein_g, na.rm = TRUE), .groups = "drop") %>%
    left_join(pop_data, by = c("consumer_iso3c" = "iso3", "year" = "Year")) %>%
    mutate(
      protein_g_percap = protein_g / population,
      iso3c            = consumer_iso3c,
      source           = src_name
    ) %>%
    select(iso3c, year, source, protein_g_percap)
}

aqua_pc <- percap_consumer(df_aqua, "Aquaculture")
cap_pc  <- percap_consumer(df_cap,  "Capture")
imp_pc  <- percap_consumer(df_imp,  "Import")

cat("aqua_pc rows:", nrow(aqua_pc), "\n")
cat("cap_pc rows:",  nrow(cap_pc),  "\n")
cat("imp_pc rows:",  nrow(imp_pc),  "\n")

# protein per capita - export
# export uses exporter_iso3c (not consumer_iso3c)
# same conversion chain as consumer side.

exp_summarized <- df_exp_filtered %>%
  group_by(sciname, year, exporter_iso3c) %>%
  summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE),
            .groups = "drop")

exp_total_nutrients <- exp_summarized %>%
  left_join(nutrient_data, by = "sciname") %>%
  mutate(
    consumption_edible_t = consumption_live_t * edible,
    protein_g            = consumption_edible_t * protein_g / 0.0001
  )

exp_pc <- exp_total_nutrients %>%
  group_by(year, exporter_iso3c) %>%
  summarize(protein_g = sum(protein_g, na.rm = TRUE), .groups = "drop") %>%
  left_join(pop_data, by = c("exporter_iso3c" = "iso3", "year" = "Year")) %>%
  mutate(
    protein_g_percap = protein_g / population,
    iso3c            = exporter_iso3c,
    source           = "Export"
  ) %>%
  select(iso3c, year, source, protein_g_percap)

cat("exp_pc rows:", nrow(exp_pc), "\n")

# combine all sources 

all_pc <- bind_rows(aqua_pc, cap_pc, imp_pc, exp_pc)
cat("all_pc rows:", nrow(all_pc), "\n")

# year map: default 1996/2019, exceptions override 

exception_by_iso <- lapply(names(year_exceptions), function(cname) {
  iso <- name_to_iso$iso3[name_to_iso$country == cname]
  if (length(iso) == 0) {
    warning("Year exception country '", cname, "'if not found in pop_data, skipped")
    return(NULL)
  }
  data.frame(
    iso3c      = iso,
    early_year = year_exceptions[[cname]]["early"],
    late_year  = year_exceptions[[cname]]["late"],
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()

year_map <- data.frame(iso3c = unique(na.omit(all_pc$iso3c)),
                       stringsAsFactors = FALSE) %>%
  left_join(exception_by_iso, by = "iso3c") %>%
  mutate(
    early_year = ifelse(is.na(early_year), 1996, early_year),
    late_year  = ifelse(is.na(late_year),  2019, late_year)
  )

# compute differences (late - early) per source
# For each country x source: pull early and late year values, subtract.
# Positive = increase, negative = decline.

diff_by_source <- all_pc %>%
  left_join(year_map, by = "iso3c") %>%
  filter(year == early_year | year == late_year) %>%
  mutate(period = if_else(year == early_year, "early", "late")) %>%
  select(iso3c, source, period, protein_g_percap) %>%
  pivot_wider(names_from = period, values_from = protein_g_percap) %>%
  mutate(change = late - early)

# Panel a: capture + aquaculture changes (stacked)

panel_a <- diff_by_source %>%
  filter(source %in% c("Capture", "Aquaculture")) %>%
  select(iso3c, source, change)

# Panel b: net trade 
# (import_late - import_early) - (export_late - export_early)
# change = late - early already computed above, so subtract export change

panel_b <- diff_by_source %>%
  filter(source %in% c("Import", "Export")) %>%
  mutate(signed_change = if_else(source == "Export", -change, change)) %>%
  group_by(iso3c) %>%
  summarize(net_change = sum(signed_change, na.rm = TRUE), .groups = "drop")

# Panel c: % RDA met difference 
# %RDA_year = (aqua + cap + imp per capita g/yr) / (PerCapita_Protein_g/day * 365) * 100
# Panel C = %RDA_late - %RDA_early

# RDA file uses Year = 1996 or 2019 as alignment key for ALL countries.
# Namibia's 2001 data sits in the Year=1996 slot, 2020 in Year=2019.

supply_total <- all_pc %>%
  filter(source %in% c("Aquaculture", "Capture", "Import")) %>%
  group_by(iso3c, year) %>%
  summarize(total_supply_percap = sum(protein_g_percap, na.rm = TRUE), .groups = "drop")

rda_protein <- rda_data_raw %>%
  left_join(name_to_iso, by = c("Country" = "country")) %>%
  rename(iso3c = iso3) %>%
  filter(Year %in% c(1996, 2019)) %>%
  select(iso3c, Year, PerCapita_Protein_g)

pct_rda <- supply_total %>%
  left_join(year_map, by = "iso3c") %>%
  filter(year == early_year | year == late_year) %>%
  mutate(align_year = if_else(year == early_year, 1996L, 2019L)) %>%
  left_join(rda_protein, by = c("iso3c", "align_year" = "Year")) %>%
  # PerCapita_Protein_g is g/capita/day; total_supply_percap is g/capita/year
  # Multiply RDA by 365 to put both on an annual basis
  mutate(pct_rda = total_supply_percap / (PerCapita_Protein_g * 365) * 100) %>%
  mutate(period = if_else(year == early_year, "early", "late")) %>%
  select(iso3c, period, pct_rda) %>%
  pivot_wider(names_from = period, values_from = pct_rda) %>%
  mutate(rda_diff = late - early) %>%
  select(iso3c, rda_diff)

# merge, filter, order

combined_a <- panel_a %>%
  left_join(name_to_iso, by = c("iso3c" = "iso3")) %>%
  filter(country %in% desired_order)

combined_b <- panel_b %>%
  left_join(name_to_iso, by = c("iso3c" = "iso3")) %>%
  filter(country %in% desired_order)

combined_c <- pct_rda %>%
  left_join(name_to_iso, by = c("iso3c" = "iso3")) %>%
  filter(country %in% desired_order)

valid_countries <- Reduce(intersect, list(
  combined_a$country,
  combined_b$country,
  combined_c$country
))
valid_countries <- intersect(desired_order, valid_countries)

cat("\nCountries in final plot (", length(valid_countries), "):",
    paste(valid_countries, collapse = ", "), "\n")
missing <- setdiff(desired_order, valid_countries)
if (length(missing) > 0)
  cat("Dropped (missing data in at least one panel):", paste(missing, collapse = ", "), "\n\n")

combined_a <- combined_a %>%
  filter(country %in% valid_countries) %>%
  mutate(country = factor(country, levels = valid_countries, ordered = TRUE))

combined_b <- combined_b %>%
  filter(country %in% valid_countries) %>%
  mutate(country = factor(country, levels = valid_countries, ordered = TRUE))

combined_c <- combined_c %>%
  filter(country %in% valid_countries) %>%
  mutate(country = factor(country, levels = valid_countries, ordered = TRUE))

# range check

shared_xlim  <- c(-6000, 4000)
shared_ticks <- c(-6000, -4000, -2000, 0, 2000, 4000)

cat("\n-- Out-of-range values (outside", shared_xlim[1], "to", shared_xlim[2], ") --\n")

oor_a <- combined_a %>% filter(change    < shared_xlim[1] | change    > shared_xlim[2])
oor_b <- combined_b %>% filter(net_change< shared_xlim[1] | net_change> shared_xlim[2])
oor_c <- combined_c %>% filter(rda_diff  < shared_xlim[1] | rda_diff  > shared_xlim[2])

if (nrow(oor_a) > 0) { cat("Panel A:\n"); print(oor_a %>% select(country, source, change)) }
if (nrow(oor_b) > 0) { cat("Panel B:\n"); print(oor_b %>% select(country, net_change)) }
if (nrow(oor_c) > 0) { cat("Panel C:\n"); print(oor_c %>% select(country, rda_diff)) }
if (nrow(oor_a) + nrow(oor_b) + nrow(oor_c) == 0) cat("None -- all values within range.\n")
cat("-- To fix: widen shared_xlim and shared_ticks in section 13 --\n\n")

#plot

countries_rev <- rev(valid_countries)
col_pos       <- "#1a9641"
col_neg       <- "#d7191c"

base_theme <- theme_classic(base_size = 22) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(fill = NA, color = "black", linewidth = 1),
    axis.text.y      = element_text(size = 22, face = "bold", color = "black"),
    axis.text.x      = element_text(size = 20, color = "black"),
    axis.title.x     = element_text(size = 22, face = "bold", color = "black"),
    plot.title       = element_text(size = 25, face = "bold", color = "black"),
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 20),
    legend.title     = element_blank()
  )

no_y_axis <- theme(
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank()
)

# panel a — stacked capture + aquaculture
pA <- ggplot(combined_a, aes(x = change, y = country, fill = source)) +
  geom_col(width = 0.8, position = "stack") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("Capture" = "#1f78b4", "Aquaculture" = "#33a02c")) +
  scale_x_continuous(limits = shared_xlim, breaks = shared_ticks,
                     labels = scales::comma) +
  scale_y_discrete(limits = countries_rev) +
  labs(title = "(a) Fisheries & Aquaculture",
       x = "Protein Change (g/capita/year)", y = NULL) +
  base_theme

# panel b — net trade
pB <- ggplot(combined_b, aes(x = net_change, y = country,
                              fill = net_change >= 0)) +
  geom_col(width = 0.8) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("TRUE" = col_pos, "FALSE" = col_neg),
                    guide = "none") +
  scale_x_continuous(limits = shared_xlim, breaks = shared_ticks,
                     labels = scales::comma) +
  scale_y_discrete(limits = countries_rev) +
  labs(title = "(b) Net Trade",
       x = "Protein Change (g/capita/year)", y = NULL) +
  base_theme + no_y_axis

# panel c — % RDA met
pC <- ggplot(combined_c, aes(x = rda_diff, y = country,
                              fill = rda_diff >= 0)) +
  geom_col(width = 0.8) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("TRUE" = col_pos, "FALSE" = col_neg),
                    guide = "none") +
  scale_y_discrete(limits = countries_rev) +
  labs(title = "(c) % RDA Met",
       x = "% Change in Protein RDA Met", y = NULL) +
  base_theme + no_y_axis

# combine and save

final_plot <- pA + pB + pC +
  plot_layout(widths = c(0.8, 1.0, 0.8))

ggsave(
  filename = "FINAL_clean_no_missing_countries.png",
  plot     = final_plot,
  width    = 39,
  height   = 19,
  dpi      = 150,
  bg       = "white"
)

cat("Saved: FINAL_clean_no_missing_countries.png\n")
cat("Countries plotted:", length(valid_countries), "\n")
cat("Note: Namibia uses 2001/2020 instead of 1996/2019\n")
