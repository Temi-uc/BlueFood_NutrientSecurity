library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

setwd("/Users/temi/Desktop/Consumption_Rpackage")

# Country order 
desired_order <- c(
  "Gabon", "Mauritius", "Congo", "Egypt", "CotedIvoire",
  "SierraLeone", "Guinea", "Tunisia", "Benin", "Libya", "Uganda", "Mauritania",
  "Cameroon", "Nigeria", "Togo", "Angola", "GuineaBissau",
  "Djibouti", "Liberia", "CapeVerde", "Mozambique", "Sudan", "DR of Congo", "Somalia",
  "Tanzania", "Madagascar", "Eritrea", "Kenya", "South Africa", "Algeria",
  "Morocco", "Ghana", "Gambia", "Namibia", "Senegal"
)

year_exceptions <- list(
  "Namibia" = c(early = 2001, late = 2020)
)

# Load data

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

#all nutrients per capita for consumer
#all six nutrients computed in one pass per source.
#export is excluded from % RDA met (same as the protein figure).

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

cat("aqua_pc rows:", nrow(aqua_pc), "\n")
cat("cap_pc rows:",  nrow(cap_pc),  "\n")
cat("imp_pc rows:",  nrow(imp_pc),  "\n")

#total supply per capita (aqua + cap + imp)

supply_total <- bind_rows(aqua_pc, cap_pc, imp_pc) %>%
  group_by(iso3c, year) %>%
  summarize(across(all_of(nutrient_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

#year map

exception_by_iso <- lapply(names(year_exceptions), function(cname) {
  iso <- name_to_iso$iso3[name_to_iso$country == cname]
  if (length(iso) == 0) {
    warning("Year exception '", cname, "' not found in pop_data — skipped.")
    return(NULL)
  }
  data.frame(iso3c      = iso,
             early_year = year_exceptions[[cname]]["early"],
             late_year  = year_exceptions[[cname]]["late"],
             stringsAsFactors = FALSE)
}) %>% bind_rows()

year_map <- data.frame(iso3c = unique(na.omit(supply_total$iso3c)),
                       stringsAsFactors = FALSE) %>%
  left_join(exception_by_iso, by = "iso3c") %>%
  mutate(
    early_year = ifelse(is.na(early_year), 1996, early_year),
    late_year  = ifelse(is.na(late_year),  2019, late_year)
  )

# rda (all nutrients)
# RDA file uses Year = 1996 or 2019 as alignment key for all countries.
# Namibia's 2001 data is in the Year=1996 slot, 2020 in Year=2019.

rda_lookup <- rda_data_raw %>%
  left_join(name_to_iso, by = c("Country" = "country")) %>%
  rename(iso3c = iso3) %>%
  filter(Year %in% c(1996, 2019)) %>%
  select(
    iso3c, Year,
    protein_g      = PerCapita_Protein_g,
    calcium_mg     = PerCapita_Calcium_mg,
    iron_mg        = PerCapita_Iron_mg,
    vitamina_mcg   = PerCapita_VitA_ug,
    vitaminb12_mcg = PerCapita_VitB12_ug,
    zinc_mg        = PerCapita_Zinc_mg
  )

#compute %RDA met differnce per nutrient
#for each country x nutrient:
#%RDA_year = supply_percap_year / (RDA_percap_day * 365) * 100
#diff = %RDA_late - %RDA_early

pct_rda_all <- supply_total %>%
  left_join(year_map, by = "iso3c") %>%
  filter(year == early_year | year == late_year) %>%
  mutate(align_year = if_else(year == early_year, 1996L, 2019L)) %>%
  left_join(rda_lookup, by = c("iso3c", "align_year" = "Year")) %>%
  # Divide each nutrient supply by its RDA * 365 to get % met annually
  mutate(
    pct_protein_g      = protein_g.x      / (protein_g.y      * 365) * 100,
    pct_calcium_mg     = calcium_mg.x     / (calcium_mg.y     * 365) * 100,
    pct_iron_mg        = iron_mg.x        / (iron_mg.y        * 365) * 100,
    pct_vitamina_mcg   = vitamina_mcg.x   / (vitamina_mcg.y   * 365) * 100,
    pct_vitaminb12_mcg = vitaminb12_mcg.x / (vitaminb12_mcg.y * 365) * 100,
    pct_zinc_mg        = zinc_mg.x        / (zinc_mg.y        * 365) * 100,
    period = if_else(year == early_year, "early", "late")
  ) %>%
  select(iso3c, period,
         pct_protein_g, pct_calcium_mg, pct_iron_mg,
         pct_vitamina_mcg, pct_vitaminb12_mcg, pct_zinc_mg) %>%
  pivot_longer(cols = starts_with("pct_"),
               names_to = "nutrient", values_to = "pct_rda") %>%
  pivot_wider(names_from = period, values_from = pct_rda) %>%
  mutate(rda_diff = late - early) %>%
  select(iso3c, nutrient, rda_diff)

# merge and filter to desired countries

pct_rda_named <- pct_rda_all %>%
  left_join(name_to_iso, by = c("iso3c" = "iso3")) %>%
  filter(country %in% desired_order) %>%
  mutate(country = factor(country, levels = desired_order, ordered = TRUE)) %>%
  arrange(country)

valid_countries <- intersect(desired_order, unique(pct_rda_named$country))
missing <- setdiff(desired_order, valid_countries)
if (length(missing) > 0)
  cat("Missing countries (no RDA data):", paste(missing, collapse = ", "), "\n\n")

countries_rev <- rev(valid_countries)

#nutrient panel
# label, column name in pct_rda_named, x limits, x tick marks

nutrient_specs <- list(
  list(col = "pct_protein_g",      label = "Protein",      xlim = c(-40, 40),   ticks = c(-40, -20, 0, 20, 40)),
  list(col = "pct_calcium_mg",     label = "Calcium",      xlim = c(-40, 40),   ticks = c(-40, -20, 0, 20, 40)),
  list(col = "pct_iron_mg",        label = "Iron",         xlim = c(-40, 40),   ticks = c(-40, -20, 0, 20, 40)),
  list(col = "pct_zinc_mg",        label = "Zinc",         xlim = c(-40, 40),   ticks = c(-40, -20, 0, 20, 40)),
  list(col = "pct_vitamina_mcg",   label = "Vitamin A",    xlim = c(-5, 5),     ticks = c(-5, 0, 5)),
  list(col = "pct_vitaminb12_mcg", label = "Vitamin B12",  xlim = c(-200, 100), ticks = c(-200, -100, 0, 100))
)

#build plots

col_pos <- "#1a9641"
col_neg <- "#d7191c"

base_theme <- theme_classic(base_size = 22) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(fill = NA, color = "black", linewidth = 1),
    axis.text.x      = element_text(size = 20, color = "black"),
    axis.title.x     = element_text(size = 20, face = "bold", color = "black"),
    plot.title       = element_text(size = 26, face = "bold", color = "black"),
    legend.position  = "none"
  )

show_y  <- theme(axis.text.y = element_text(size = 22, face = "bold", color = "black"))
hide_y  <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Panel indices that show y-axis labels: 1st and 4th (left column, both rows)
show_y_idx <- c(1, 4)
# Panel indices that show x-axis label: bottom row (4, 5, 6)
show_x_idx <- c(4, 5, 6)

plots <- lapply(seq_along(nutrient_specs), function(i) {

  spec    <- nutrient_specs[[i]]
  df_nut  <- pct_rda_named %>%
    filter(nutrient == spec$col, !is.na(rda_diff)) %>%
    mutate(country = factor(country, levels = valid_countries, ordered = TRUE))

  # Small horizontal tick marks at zero (matching Python hlines)
  x_range    <- spec$xlim[2] - spec$xlim[1]
  tick_hw    <- x_range * 0.08
  tick_df    <- data.frame(country = factor(valid_countries,
                                            levels = valid_countries,
                                            ordered = TRUE))

  p <- ggplot(df_nut, aes(x = rda_diff, y = country, fill = rda_diff >= 0)) +
    # Zero tick marks across all bars
    geom_segment(data = tick_df,
                 aes(x = -tick_hw, xend = tick_hw,
                     y = country, yend = country),
                 inherit.aes = FALSE,
                 color = "black", linewidth = 0.8, alpha = 0.8) +
    geom_col(width = 0.85) +
    geom_vline(xintercept = 0, color = "black", linewidth = 1.8) +
    scale_fill_manual(values = c("TRUE" = col_pos, "FALSE" = col_neg),
                      guide = "none") +
    scale_x_continuous(limits = spec$xlim, breaks = spec$ticks,
                       labels = scales::comma) +
    scale_y_discrete(limits = countries_rev) +
    labs(
      title = spec$label,
      x     = if (i %in% show_x_idx) "% Change in RDA" else "",
      y     = NULL
    ) +
    base_theme +
    if (i %in% show_y_idx) show_y else hide_y
})

#combine and save

final_plot <- wrap_plots(plots, nrow = 2, ncol = 3) +
  plot_layout(guides = "collect")

ggsave(
  filename = "Differences_in_RDA_all_nutrients.png",
  plot     = final_plot,
  width    = 40,
  height   = 30,
  dpi      = 200,
  bg       = "white"
)

cat("Saved: Differences_in_RDA_all_nutrients.png\n")
cat("Countries plotted:", length(valid_countries), "\n")
cat("Note: Namibia uses 2001/2020 instead of 1996/2019\n")
