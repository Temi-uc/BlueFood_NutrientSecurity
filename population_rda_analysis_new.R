# Population Demographics + RDA Nutrient Needs Calculator
# 1996 & 2019

library(dplyr)
library(readr)
library(purrr)

#set folder path

data_dir <- "/Users/temi/Desktop/Consumption Analysis"   


# file maps  — year is passed explicitly so files without
#     a year in their name are still tagged correctly

files_1996 <- tribble(
  ~filename,                                                           ~Gender,   ~Age,   ~Year,
  "male-population-of-children-under-the-age-of-5_1996.csv",         "Male",    "0-4",   1996,
  "male-population-aged-5-to-9-years_1996.csv",                      "Male",    "5-9",   1996,
  "male-population-aged-10-to-14-years_1996.csv",                    "Male",    "10-14", 1996,
  "male-population-aged-15-to-19-years_1996.csv",                    "Male",    "15-19", 1996,
  "male-population-aged-20-to-29-years_1996.csv",                    "Male",    "20-29", 1996,
  "male-population-aged-30-to-39-years_1996.csv",                    "Male",    "30-39", 1996,
  "male-population-aged-40-to-49-years_1996.csv",                    "Male",    "40-49", 1996,
  "male-population-aged-50-to-59-years_1996.csv",                    "Male",    "50-59", 1996,
  "male-population-aged-60-to-69-years_1996.csv",                    "Male",    "60-69", 1996,
  "male-population-aged-70-to-79-years_1996.csv",                    "Male",    "70-79", 1996,
  "male-population-aged-80-to-89-years_1996.csv",                    "Male",    "80-89", 1996,
  "male-population-aged-90-to-99-years_1996.csv",                    "Male",    "90-99", 1996,
  "male-population-older-than-100-years_1996.csv",                   "Male",    "100+",  1996,
  "female-population-of-children-under-the-age-of-5_1996.csv",      "Female",  "0-4",   1996,
  "female-population-aged-5-to-9-years_1996.csv",                    "Female",  "5-9",   1996,
  "female-population-aged-10-to-14-years_1996.csv",                  "Female",  "10-14", 1996,
  "female-population-aged-15-to-19-years_1996.csv",                  "Female",  "15-19", 1996,
  "female-population-aged-20-to-29-years_1996.csv",                  "Female",  "20-29", 1996,
  "female-population-aged-30-to-39-years_1996.csv",                  "Female",  "30-39", 1996,
  "female-population-aged-40-to-49-years_1996.csv",                  "Female",  "40-49", 1996,
  "female-population-aged-50-to-59-years_1996.csv",                  "Female",  "50-59", 1996,
  "female-population-aged-60-to-69-years_1996.csv",                  "Female",  "60-69", 1996,
  "female-population-aged-70-to-79-years_1996.csv",                  "Female",  "70-79", 1996,
  "female-population-aged-80-to-89-years_1996.csv",                  "Female",  "80-89", 1996,
  "female-population-aged-90-to-99-years_1996.csv",                  "Female",  "90-99", 1996,
  "female-population-older-than-100-years_1996.csv",                 "Female",  "100+",  1996
)

files_2019 <- tribble(
  ~filename,                                                           ~Gender,   ~Age,   ~Year,
  "male-population-of-children-under-the-age-of-5_1996.csv",         "Male",    "0-4",   2019,
  "male-population-aged-5-to-9-years_1996.csv",                      "Male",    "5-9",   2019,
  "male-population-aged-10-to-14-years_1996.csv",                    "Male",    "10-14", 2019,
  "male-population-aged-15-to-19-years_1996.csv",                    "Male",    "15-19", 2019,
  "male-population-aged-20-to-29-years_1996.csv",                    "Male",    "20-29", 2019,
  "male-population-aged-30-to-39-years_1996.csv",                    "Male",    "30-39", 2019,
  "male-population-aged-40-to-49-years_1996.csv",                    "Male",    "40-49", 2019,
  "male-population-aged-50-to-59-years_1996.csv",                    "Male",    "50-59", 2019,
  "male-population-aged-60-to-69-years_1996.csv",                    "Male",    "60-69", 2019,
  "male-population-aged-70-to-79-years.csv",                         "Male",    "70-79", 2019,
  "male-population-aged-80-to-89-years.csv",                         "Male",    "80-89", 2019,
  "male-population-aged-90-to-99-years.csv",                         "Male",    "90-99", 2019,
  "male-population-older-than-100-years.csv",                        "Male",    "100+",  2019,
  "female-population-of-children-under-the-age-of-5.csv",           "Female",  "0-4",   2019,
  "female-population-aged-5-to-9-years.csv",                         "Female",  "5-9",   2019,
  "female-population-aged-10-to-14-years.csv",                       "Female",  "10-14", 2019,
  "female-population-aged-15-to-19-years.csv",                       "Female",  "15-19", 2019,
  "female-population-aged-20-to-29-years.csv",                       "Female",  "20-29", 2019,
  "female-population-aged-30-to-39-years.csv",                       "Female",  "30-39", 2019,
  "female-population-aged-40-to-49-years.csv",                       "Female",  "40-49", 2019,
  "female-population-aged-50-to-59-years.csv",                       "Female",  "50-59", 2019,
  "female-population-aged-60-to-69-years.csv",                       "Female",  "60-69", 2019,
  "female-population-aged-70-to-79-years.csv",                       "Female",  "70-79", 2019,
  "female-population-aged-80-to-89-years.csv",                       "Female",  "80-89", 2019,
  "female-population-aged-90-to-99-years.csv",                       "Female",  "90-99", 2019,
  "female-population-older-than-100-years.csv",                      "Female",  "100+",  2019
)

Namibia_nutrient_table <- tribble(
  ~Gender,   ~Age,    ~population, Year,
"Male",    "0-4", 131090, 2001,
"Male",    "5-9",  128703, 2001,
"Male",    "10-14", 123674, 2001,
"Male",    "15-19", 97721, 2001,
"Male",    "20-29", 164787, 2001,
"Male",    "30-39", 114379, 2001,
"Male",    "40-49", 64290, 2001,
"Male",    "50-59", 40634, 2001,
"Male",    "60-69", 22861, 2001,
"Male",    "70-79", 9789, 2001,
"Male",    "80-89", 2557, 2001,
"Male",    "90-99", 214, 2001,
"Male",    "100+", 2, 2001,
"Female",  "0-4", 132258, 2001,
"Female",  "5-9", 132430,   2001,
"Female",  "10-14", 123402, 2001,
"Female",  "15-19", 100862, 2001,
"Female",  "20-29", 168815, 2001,
"Female",  "30-39", 123498, 2001,
"Female",  "40-49", 75857, 2001,
"Female",  "50-59", 49782, 2001,
"Female",  "60-69", 31505, 2001,
"Female",  "70-79", 15950, 2001,
"Female",  "80-89", 5994, 2001,
"Female",  "90-99", 781, 2001,
"Female",  "100+", 16, 2001,

#2020
"Male",    "0-4", 192418, 2020,
"Male",    "5-9", 168182, 2020,
"Male",    "10-14", 144328, 2020,
"Male",    "15-19", 129539, 2020,
"Male",    "20-29", 249599, 2020,
"Male",    "30-39", 184945, 2020,
"Male",    "40-49", 126232, 2020,
"Male",    "50-59", 75271, 2020,
"Male",    "60-69", 39194, 2020,
"Male",    "70-79", 16756, 2020,
"Male",    "80-89", 3789, 2020,
"Male",    "90-99", 289, 2020,
"Male",    "100+",  3, 2020,
"Female",  "0-4", 193328,  2020,
"Female",  "5-9", 170278,   2020,
"Female",  "10-14", 144912, 2020,
"Female",  "15-19", 127685, 2020,
"Female",  "20-29", 255568, 2020,
"Female",  "30-39", 193377, 2020,
"Female",  "40-49", 130441, 2020,
"Female",  "50-59", 89970, 2020,
"Female",  "60-69", 54590, 2020,
"Female",  "70-79", 27429, 2020,
"Female",  "80-89", 9174, 2020,
"Female",  "90-99", 1427,  2020,
"Female",  "100+", 51, 2020,
# Combine both year maps into one
demographic_files <- bind_rows(files_1996, files_2019)

# RDA lookup table  (per-person per-day)
#     Source: https://odphp.health.gov/sites/default/files/2019-09/Appendix-E3-1-Table-A4.pdf
#     Units:
#       Protein   → g/day
#       Calcium   → mg/day
#       Iron      → mg/day
#       Vitamin_A → µg RAE/day
#       Vitamin_B12 → µg/day
#       Zinc      → mg/day

rda_table <- tribble(
  ~Gender,   ~Age,    ~RDA_Protein_g, ~RDA_Calcium_mg, ~RDA_Iron_mg,
  ~RDA_VitA_ug,  ~RDA_VitB12_ug, ~RDA_Zinc_mg,

  # MALES 
  "Male", "0-4",    13,  700,   7,   300, 0.9, 3.0,
  "Male", "5-9",    19,  1000,  10,  400, 1.2, 5.0,
  "Male", "10-14",  34,  1300,  8,   600, 1.8, 8.0,
  "Male", "15-19",  52,  1300,  11,  900, 2.4, 11.0,
  "Male", "20-29",  56,  1000,  8,   900, 2.4, 11.0,
  "Male", "30-39",  56,  1000,  8,   900, 2.4, 11.0,
  "Male", "40-49",  56,  1000,  8,   900, 2.4, 11.0,
  "Male", "50-59",  56,  1200,  8,   900, 2.4, 11.0,
  "Male", "60-69",  56,  1200,  8,   900, 2.4, 11.0,
  "Male", "70-79",  56,  1200,  8,   900, 2.4, 11.0,
  "Male", "80-89",  56,  1200,  8,   900, 2.4, 11.0,
  "Male", "90-99",  56,  1200,  8,   900, 2.4, 11.0,
  "Male", "100+",   56,  1200,  8,   900, 2.4, 11.0,

  # FEMALES
  "Female", "0-4",    13,  700,   7,   300, 0.9, 3.0,
  "Female", "5-9",    19,  1000,  10,  400, 1.2, 5.0,
  "Female", "10-14",  34,  1300,  8,   600, 1.8, 8.0,
  "Female", "15-19",  46,  1300,  15,  700, 2.4, 9.0,
  "Female", "20-29",  46,  1000,  18,  700, 2.4, 8.0,
  "Female", "30-39",  46,  1000,  18,  700, 2.4, 8.0,
  "Female", "40-49",  46,  1000,  18,  700, 2.4, 8.0,
  "Female", "50-59",  46,  1200,  8,   700, 2.4, 8.0,
  "Female", "60-69",  46,  1200,  8,   700, 2.4, 8.0,
  "Female", "70-79",  46,  1200,  8,   700, 2.4, 8.0,
  "Female", "80-89",  46,  1200,  8,   700, 2.4, 8.0,
  "Female", "90-99",  46,  1200,  8,   700, 2.4, 8.0,
  "Female", "100+",   46,  1200,  8,   700, 2.4, 8.0
)


# read file
#     Year is passed in explicitly — no reliance on filename

read_pop_file <- function(filename, gender, age, year) {
  full_path <- file.path(data_dir, filename)
  if (!file.exists(full_path)) stop("File not found: ", full_path)
  df <- read_csv(full_path, show_col_types = FALSE)

  # Standardise "Entity" → "Country"
  if ("Entity" %in% names(df)) df <- rename(df, Country = Entity)

  # Use Year column if present (2019 files), otherwise use the passed year argument (1996 files)
  if (!("Year" %in% names(df))) {
    df$Year <- year
  }

  # Find population column: age-band columns like "5-9 years" come first,
  # then anything with "population/pop", then any remaining numeric column
  pop_col <- names(df)[grepl("years", names(df), ignore.case = TRUE)]

  if (length(pop_col) == 0) {
    pop_col <- names(df)[grepl("population|pop", names(df), ignore.case = TRUE) &
                           !grepl("^country$|^entity$", names(df), ignore.case = TRUE)]
  }

  if (length(pop_col) == 0) {
    exclude <- c("Year", "year", "Code", "code")
    pop_col <- names(df)[sapply(df, is.numeric) & !names(df) %in% exclude]
  }

  if (length(pop_col) == 0) stop("Cannot find population column in: ", filename)

  df <- rename(df, Population = all_of(pop_col[1]))

  df |>
    mutate(Gender = gender, Age = age) |>
    select(Country, Gender, Age, Year, Population)
}

# Read all files for both years
combined_df <- pmap_dfr(
  list(demographic_files$filename, demographic_files$Gender, demographic_files$Age, demographic_files$Year),
  read_pop_file
)

cat("Loaded", nrow(combined_df), "rows covering years:",
    paste(sort(unique(combined_df$Year)), collapse = " & "), "\n\n")


# join RDA → demographic-level data

demographic_df <- combined_df |>
  left_join(rda_table, by = c("Gender", "Age")) |>
  mutate(
    Total_Protein_g   = RDA_Protein_g   * Population,
    Total_Calcium_mg  = RDA_Calcium_mg  * Population,
    Total_Iron_mg     = RDA_Iron_mg     * Population,
    Total_VitA_ug     = RDA_VitA_ug     * Population,
    Total_VitB12_ug   = RDA_VitB12_ug   * Population,
    Total_Zinc_mg     = RDA_Zinc_mg     * Population
  ) |>
  select(
    Country, Year, Gender, Age, Population,
    RDA_Protein_g, RDA_Calcium_mg, RDA_Iron_mg,
    RDA_VitA_ug,   RDA_VitB12_ug, RDA_Zinc_mg,
    Total_Protein_g,  Total_Calcium_mg, Total_Iron_mg,
    Total_VitA_ug,    Total_VitB12_ug, Total_Zinc_mg
  )


# country-level summary  (both years kept via group_by Year)

country_df <- demographic_df |>
  group_by(Country, Year) |>
  summarise(
    Total_Population  = sum(Population,       na.rm = TRUE),
    Total_Protein_g   = sum(Total_Protein_g,  na.rm = TRUE),
    Total_Calcium_mg  = sum(Total_Calcium_mg, na.rm = TRUE),
    Total_Iron_mg     = sum(Total_Iron_mg,    na.rm = TRUE),
    Total_VitA_ug     = sum(Total_VitA_ug,    na.rm = TRUE),
    Total_VitB12_ug     = sum(Total_VitB12_ug,    na.rm = TRUE),
    Total_Zinc_mg     = sum(Total_Zinc_mg,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    PerCapita_Protein_g   = Total_Protein_g   / Total_Population,
    PerCapita_Calcium_mg  = Total_Calcium_mg  / Total_Population,
    PerCapita_Iron_mg     = Total_Iron_mg     / Total_Population,
    PerCapita_VitA_ug     = Total_VitA_ug     / Total_Population,
    PerCapita_VitB12_ug   = Total_VitB12_ug     / Total_Population,
    PerCapita_Zinc_mg     = Total_Zinc_mg     / Total_Population
  ) |>
  arrange(Country, Year)


# Save outputs  (combined files covering both years)

write_csv(demographic_df, file.path(data_dir, "population_demographic_rda_1996_2019.csv"))
write_csv(country_df,     file.path(data_dir, "population_country_rda_1996_2019.csv"))

cat("✅ Files written to:", data_dir, "\n")
cat("   • population_demographic_rda_1996_2019.csv — one row per country × gender × age × year\n")
cat("   • population_country_rda_1996_2019.csv     — one row per country × year (aggregated)\n\n")


# preview

cat("Demographic-level preview ─\n")
print(head(demographic_df))

cat("\n─ Country-level preview ─\n")
print(head(country_df))