# load libraries
library(dplyr)
library(arrow)
library(tidyverse)
library(ggplot2)

setwd("/Users/temi/Desktop/Consumption_Rpackage")

# load data
df_exp <- read.csv("artis_export.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_cap <- read.csv("artis_capture.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_aqua <- read.csv("artis_aquaculture.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
df_imp <- read.csv("artis_import.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
nutrient_data <- read.csv("edible_nutrient.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
population <- read.csv("Total_population.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")


# define list of datasets to give to the loop
datasets <- list(
  aqua = df_aqua,
  cap  = df_cap,
  imp  = df_imp,
  exp  = df_exp
)

# create nutrient per capita data tables for aquaculture, capture fisheries, and imports
results <- lapply(datasets, function(df) {
  
  summarized <- df %>%
    group_by(sciname, year, consumer_iso3c) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE), .groups = "drop")
  
  total_nutrients <- summarized %>%
    left_join(nutrient_data, by = "sciname") %>%
    mutate(
      consumption_edible_t = consumption_live_t * edible,
      vitamina_mcg   = consumption_edible_t * vitamina_mcg / 0.0001,
      vitaminb12_mcg = consumption_edible_t * vitaminb12_mcg / 0.0001,
      calcium_mg     = consumption_edible_t * calcium_mg / 0.0001,
      iron_mg        = consumption_edible_t * iron_mg / 0.0001,
      zinc_mg        = consumption_edible_t * zinc_mg / 0.0001,
      protein_g      = consumption_edible_t * protein_g / 0.0001
    )
  
  total_nutrients <- total_nutrients %>%
    group_by(year, consumer_iso3c) %>%
    summarize(
      vitamina_mcg   = sum(vitamina_mcg,   na.rm = TRUE),
      vitaminb12_mcg = sum(vitaminb12_mcg, na.rm = TRUE),
      calcium_mg     = sum(calcium_mg,     na.rm = TRUE),
      iron_mg        = sum(iron_mg,        na.rm = TRUE),
      zinc_mg        = sum(zinc_mg,        na.rm = TRUE),
      protein_g      = sum(protein_g,      na.rm = TRUE), .groups = "drop")
  
  total_nutrients %>%
    left_join(population, by = c("consumer_iso3c" = "iso3", "year" = "Year")) %>%
    mutate(across(c(vitamina_mcg, vitaminb12_mcg, calcium_mg, iron_mg, zinc_mg, protein_g),
                  ~ .x / population),
           iso3c = consumer_iso3c) %>%
    select(-country, -population, -consumer_iso3c)
})

# create nutrient per capita data table for exports, joining on exporter_iso3c
results$exp <- {
  
  summarized <- df_exp %>%
    group_by(sciname, year, exporter_iso3c) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE), .groups = "drop")
  
  total_nutrients <- summarized %>%
    left_join(nutrient_data, by = "sciname") %>%
    mutate(
      consumption_edible_t = consumption_live_t * edible,
      vitamina_mcg   = consumption_edible_t * vitamina_mcg / 0.0001,
      vitaminb12_mcg = consumption_edible_t * vitaminb12_mcg / 0.0001,
      calcium_mg     = consumption_edible_t * calcium_mg / 0.0001,
      iron_mg        = consumption_edible_t * iron_mg / 0.0001,
      zinc_mg        = consumption_edible_t * zinc_mg / 0.0001,
      protein_g      = consumption_edible_t * protein_g / 0.0001
    )
  
  total_nutrients <- total_nutrients %>%
    group_by(year, exporter_iso3c) %>%
    summarize(
      vitamina_mcg   = sum(vitamina_mcg,   na.rm = TRUE),
      vitaminb12_mcg = sum(vitaminb12_mcg, na.rm = TRUE),
      calcium_mg     = sum(calcium_mg,     na.rm = TRUE),
      iron_mg        = sum(iron_mg,        na.rm = TRUE),
      zinc_mg        = sum(zinc_mg,        na.rm = TRUE),
      protein_g      = sum(protein_g,      na.rm = TRUE), .groups = "drop")
  
  total_nutrients %>%
    left_join(population, by = c("exporter_iso3c" = "iso3", "year" = "Year")) %>%
    mutate(across(c(vitamina_mcg, vitaminb12_mcg, calcium_mg, iron_mg, zinc_mg, protein_g),
                  ~ .x / population),
           iso3c = exporter_iso3c) %>%
    select(-country, -population, -exporter_iso3c)
}

aqua_nutrients_percap <- results$aqua
cap_nutrients_percap  <- results$cap
imp_nutrients_percap  <- results$imp
exp_nutrients_percap  <- results$exp

# Graphs
all_nutrients <- bind_rows(
  aqua_nutrients_percap %>% mutate(source = "Aquaculture"),
  cap_nutrients_percap  %>% mutate(source = "Capture"),
  imp_nutrients_percap  %>% mutate(source = "Import"),
  exp_nutrients_percap  %>% mutate(source = "Export")
)

all_nutrients_long <- all_nutrients %>%
  pivot_longer(cols = c(vitamina_mcg, vitaminb12_mcg, calcium_mg, iron_mg, zinc_mg, protein_g),
               names_to = "nutrient",
               values_to = "value")

all_nutrients_relative <- all_nutrients_long %>%
  group_by(iso3c, source, nutrient) %>%
  mutate(
    baseline_year  = if (any(year == 1996)) 1996L else min(year, na.rm = TRUE),
    value_baseline = value[year == baseline_year][1],
    value_relative = value - value_baseline,
    value_relative = ifelse(source == "Export", -value_relative, value_relative)
  ) %>%
  ungroup() %>%
  mutate(source = factor(source, levels = c("Import", "Aquaculture", "Capture", "Export")))

# Compute total nutrient line (sum across sources, minus baseline) 
total_line <- all_nutrients_long %>%
  mutate(signed_value = ifelse(source == "Export", -value, value)) %>%
  group_by(iso3c, year, nutrient) %>%
  summarize(total = sum(signed_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(iso3c, nutrient) %>%
  mutate(
    baseline_year  = if (any(year == 1996)) 1996L else min(year, na.rm = TRUE),
    baseline_total = total[year == baseline_year][1],
    total_relative = total - baseline_total
  ) %>%
  ungroup()

nutrient_list <- c("vitamina_mcg", "vitaminb12_mcg", "calcium_mg",
                   "iron_mg",      "zinc_mg",         "protein_g")
country_list <- unique(all_nutrients_relative$iso3c)
country_list <- country_list[!is.na(country_list)]

# Shared theme 
clean_theme <- theme_classic() +
  theme(
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.5),
    strip.background  = element_rect(fill = "white", color = NA),
    strip.text        = element_text(hjust = 0, face = "plain", size = 10),
    axis.line         = element_line(color = "black"),
    axis.text         = element_text(color = "black", size = 9),
    axis.title        = element_text(color = "black", size = 10),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    legend.position   = "top",
    legend.direction  = "horizontal"
  )

# Create output folder 
output_dir <- "figures_Original_New"
dir.create(output_dir, showWarnings = FALSE)

# Loop over countries 
for (ctry in country_list) {
  
  ctry_data  <- all_nutrients_relative %>% filter(iso3c == ctry)
  ctry_total <- total_line             %>% filter(iso3c == ctry)
  
  if (nrow(ctry_data) == 0 || all(is.na(ctry_data$value_relative))) next
  
  # individual nutrient plots: e.g. MRT_protein_g.png
  for (nut in nutrient_list) {
    
    ctry_nut_data  <- ctry_data  %>% filter(nutrient == nut)
    ctry_nut_total <- ctry_total %>% filter(nutrient == nut)
    
    if (nrow(ctry_nut_data) == 0 || all(is.na(ctry_nut_data$value_relative))) next
    
    baseline_yrs <- unique(ctry_nut_data$baseline_year)
    baseline_lbl <- if (length(baseline_yrs) == 1) as.character(baseline_yrs) else "earliest available"
    
    p <- ggplot() +
      geom_bar(data = ctry_nut_data,
               aes(x = year, y = value_relative, fill = source),
               stat = "identity") +
      geom_line(data = ctry_nut_total,
                aes(x = year, y = total_relative, color = "Total Available Nutrient"),
                linewidth = 1, linetype = "dashed") +
      geom_hline(yintercept = 0, linewidth = 1.5, color = "black") +
      scale_fill_manual(
        name   = "Source",
        values = c(
          "Aquaculture" = "skyblue",
          "Capture"     = "lightgreen",
          "Import"      = "salmon",
          "Export"      = "purple"
        )
      ) +
      scale_color_manual(
        name   = "",
        values = c("Total Available Nutrient" = "red")
      ) +
      labs(
        title    = paste("Change in", nut, "per capita relative to", baseline_lbl),
        subtitle = ctry,
        x        = "Year",
        y        = paste("Change in", nut)
      ) +
      clean_theme
    
    ggsave(
      filename = file.path(output_dir, paste0(ctry, "_", nut, ".png")),
      plot     = p,
      width    = 8,
      height   = 6,
      dpi      = 300
    )
  }
  
  # all nutrients faceted: e.g. MRT_allnutrients.png
  baseline_yrs <- unique(ctry_data$baseline_year)
  subtitle_txt <- if (length(baseline_yrs) == 1 && baseline_yrs == 1996) {
    ctry
  } else {
    paste0(ctry, "  (baseline: 1996 or earliest available year per source)")
  }
  
  p_all <- ggplot() +
    geom_bar(data = ctry_data,
             aes(x = year, y = value_relative, fill = source),
             stat = "identity") +
    geom_line(data = ctry_total,
              aes(x = year, y = total_relative, color = "Total Available Nutrient"),
              linewidth = 1, linetype = "dashed") +
    geom_hline(yintercept = 0, linewidth = 1.5, color = "black") +
    facet_wrap(~ nutrient, scales = "free_y", ncol = 2) +
    scale_fill_manual(
      name   = "Source",
      values = c(
        "Aquaculture" = "skyblue",
        "Capture"     = "lightgreen",
        "Import"      = "salmon",
        "Export"      = "purple"
      )
    ) +
    scale_color_manual(
      name   = "",
      values = c("Total Available Nutrient" = "red")
    ) +
    labs(
      title    = "Contribution of Aquaculture, Fisheries, and Trade to Nutrient Availability Relative to Baseline Year",
      subtitle = subtitle_txt,
      x        = "Year",
      y        = "Change in nutrient per capita relative to baseline"
    ) +
    clean_theme
  
  ggsave(
    filename = file.path(output_dir, paste0(ctry, "_allnutrients.png")),
    plot     = p_all,
    width    = 10,
    height   = 12,
    dpi      = 300
  )
}

