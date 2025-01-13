## ----------------------------------------------------------------
# install.packages(c("tidyverse",
#                    "here",
#                    "tidylog",
#                    "summarytools"))

## ----------------------------------------------------------------------
tidyverse::tidyverse_packages()


## ---------------------------------------------------------
library(here)
library(tidyverse)
library(tidylog)
library(summarytools)


## ----------------------------------------------------------------------

dt_raw <- read_csv(here("data/individual_seed_production.csv"))


## ----------------------------------------------------------------------
glimpse(dt_raw)


## ----------------------------------------------------------------------
head(dt_raw)


## ----------------------------------------------------------------------
dt_raw |>
  arrange(count)


## ----------------------------------------------------------------------
dt_raw |>
  arrange(desc(count))


## ----------------------------------------------------------------------
dt_raw |>
  arrange(site_name, species_name, desc(count))


## ----------------------------------------------------------------------

dt_raw |>
  rename(site = site_name)


## ----------------------------------------------------------------------

dt_raw |>
  relocate(year, .before = megaplot)


## ----------------------------------------------------------------------

dt_raw |>
  select(site_name, year, species_name, count)


## ----------------------------------------------------------------------

dt_raw |>
  select(-c(megaplot, plot, trap))


## ----------------------------------------------------------------------

dt <- dt_raw |>
  select(site = site_name,
         year,
         species_name,
         plant_ID,
         count,
         method = general_method,
         stem_cm = stem_diameter_cm,
         trap_area_m2)


## ----------------------------------------------------------------------
glimpse(dt)


## ----------------------------------------------------------------------
summary(dt$species_name)


## ----------------------------------------------------------------------
dfSummary(dt$species_name)


## ----------------------------------------------------------------------
summary(dt$count)


## ----------------------------------------------------------------------
dfSummary(dt$count)


## ----------------------------------------------------------------------
dt |>
  distinct(site)


## ----------------------------------------------------------------------
unique(dt$site)


## ----------------------------------------------------------------------
dt |>
  distinct(site, method)


## ----------------------------------------------------------------------
dt |>
  mutate(fruits_per_m2 = count/trap_area_m2)


## ----------------------------------------------------------------------
dt |>
  filter(site == "BNZ")


## ----------------------------------------------------------------------
dt |>
  filter(site %in% c("AEC", "AND", "BNZ")) |>
  filter(count >= 10)


## ----------------------------------------------------------------------
dt |>
  group_by(site) |>
  summarise(fruits = sum(count))


## ----------------------------------------------------------------------
dt |>
  group_by(site) |>
  summarise(fruits = sum(count, na.rm = TRUE))


## ----------------------------------------------------------------------
dt |>
  group_by(site) |>
  summarise(max_fruit = max(count, na.rm = TRUE),
            min_fruit = min(count, na.rm = TRUE))


## ----------------------------------------------------------------------
dt |>
  group_by(site, species_name, year) |>
  summarise(mean_fruits = mean(count, na.rm = TRUE)) |>
  ungroup()


## ----------------------------------------------------------------------
dt |>
  filter(!is.na(count)) |>
  filter(count != 0) |>
  select(count) |>
  summary()


## ----------------------------------------------------------------------
dt |>
  mutate(nivel_frutos = case_when(
    count <= 100 ~ "bajo",
    count > 100 & count <= 1000 ~ "medio",
    count > 1000 ~ "alto"))


## ----------------------------------------------------------------------
dt |>
  mutate(nivel_frutos = case_when(
    count <= 100 ~ "bajo",
    count > 100 & count <= 1000 ~ "medio",
    count > 1000 ~ "alto")) |>
  group_by(nivel_frutos) |>
  summarise(trees = n())


## ----------------------------------------------------------------------
dt_fix <- dt |>
  # quitar un valor equivocado
  mutate(count = if_else(count > 200000, NA, count))


## ----------------------------------------------------------------------
dt_fix <- dt |>
  # quitar un valor equivocado
  mutate(count = if_else(count > 200000, NA, count)) |>
  # calcular número de frutos por m2
  mutate(fruits_per_m2 = count/trap_area_m2) |>
  # crear variable con la cantidad de frutos de count o corregida
  mutate(fruits = if_else(is.na(fruits_per_m2), count, fruits_per_m2))


## ----------------------------------------------------------------------
dt_fix <- dt |>
  # quitar un valor equivocado
  mutate(count = if_else(count > 200000, NA, count)) |>
  # calcular número de frutos por m2
  mutate(fruits_per_m2 = count/trap_area_m2) |>
  # crear variable con la cantidad de frutos de count o corregida
  mutate(fruits = if_else(is.na(fruits_per_m2), count, fruits_per_m2)) |>
  # quitar valores de 0 o NA
  filter(count != 0)


## ----------------------------------------------------------------------
head(dt_fix)


## ----------------------------------------------------------------------
dt_fix |>
  group_by(site, year) |>
  summarise(fruits = mean(fruits, na.rm. = TRUE))


## ----------------------------------------------------------------------
dt_short <- dt_fix |>
  group_by(site, year) |>
  summarise(fruits = mean(fruits, na.rm. = TRUE)) |>
  pivot_wider(names_from = "site",
              values_from = "fruits")

head(dt_short)


## ----------------------------------------------------------------------
dt_short |>
  pivot_longer(cols = c(AEC:SEV),
               names_to = "site",
               values_to = "fruits")


## ----------------------------------------------------------------------
sp_info <- read_csv(here("data/species_attributes.csv"))


## ----------------------------------------------------------------------
glimpse(sp_info)


## ----------------------------------------------------------------------
sp_info |> count(pollinator_code)
sp_info |> count(family)


## ----------------------------------------------------------------------
dt_sp <- dt_fix |>
  left_join(sp_info, by = c("species_name"))


## ----------------------------------------------------------------------
setdiff(sp_info$species_name, dt_fix$species_name)


## ----------------------------------------------------------------------
glimpse(dt_sp)


## ----------------------------------------------------------------------
write_csv(dt_sp, here("data/clean_data.csv"))
#write_csv2(dt_sp, here("data/clean_data.csv"))


## ----------------------------------------------------------------------
## #install.packages("arrow")
## library(arrow)
##
## write_parquet(dt_sp, here("data/clean_data.parquet"))
##
## dt_sp |>
##   group_by(site) |>
##   arrow::write_dataset(path = "data/clean_data", format = "parquet")


## ----------------------------------------------------------------------
dt_sp |>
  filter(!is.na(stem_cm)) |>
  arrange(desc(stem_cm))


## ----------------------------------------------------------------------
dt_sp |>
  filter(!is.na(stem_cm)) |>
  group_by(species_name) |>
  summarise(mean = mean(stem_cm),
            sd = sd(stem_cm))


## ----------------------------------------------------------------------
dt |>
  filter(!is.na(stem_cm)) |>
  mutate(tree_size = case_when(stem_cm >= 40 ~ "big",
                               stem_cm < 40 ~ "small")) |>
  group_by(tree_size) |>
  summarise(n_trees = n(),
            n_species = n_distinct(species_name))


## ---------------------------------------------------------------------
dt_sp |>
  filter(method == "TRAP") |>
  group_by(site) |>
  summarise(max_fruit = max(fruits_per_m2),
            min_fruit = mean(fruits_per_m2))


## ----------------------------------------------------------------------
dt_sp |>
  filter(site %in% c("CWT", "SEV")) |>
  filter(year %in% c(2000:2010)) |>
  group_by(site, year) |>
  summarise(fruits = sum(fruits)) |>
  pivot_wider(names_from = site, values_from = fruits)


## ----------------------------------------------------------------------
dt_sp |>
  filter(year %in% c(2001:2005)) |>
  filter(str_detect(species_name, "Abies")) |>
  group_by(year, species_name) |>
  summarise(fruits = sum(fruits)) |>
  pivot_wider(names_from = year, values_from = fruits)

