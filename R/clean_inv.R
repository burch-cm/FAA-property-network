# clean and mask data

library(tidyverse)
library(readxl)

fn <- "./rawdata/Leased and Owned Inventory by LOB Oct FY22.xlsx"
inv_raw <- readxl::read_excel(path = fn, sheet = "inv")

# randomize tag order 
# to anonymize tags as points
reshuffle <- \(x) sample(x, size = length(x), replace = FALSE)


set.seed(18760222) # go blue jays

fsr_list <-
  inv_raw["FSR Email"] |> 
  drop_na() |> 
  distinct() |> 
  map_df(reshuffle) |> 
  mutate(fsr_node_num = row_number(),
         fsr_node = paste0('FSR-', fsr_node_num)) |> 
  rename(fsr_email = `FSR Email`) |> 
  select(fsr_node, fsr_email)

tag_list <-
  inv_raw["Tag"] |>  
  drop_na() |> 
  distinct() |> 
  map_df(reshuffle) |> 
  mutate(node_num = row_number(),
         tag_node = paste0('TAG-', node_num)) |> 
  rename(tag = Tag) |> 
  select(tag_node, tag)

exp_org_list <-
  inv_raw["Exp Org"] |> 
  drop_na() |> 
  distinct() |> 
  map_df(reshuffle) |> 
  mutate(exp_num = row_number(),
         exp_node = paste0('ORG-', exp_num)) |> 
  rename(exp_org = `Exp Org`) |> 
  select(exp_node, exp_org)

lob_list <- 
  inv_raw["Level 2"] |> 
  drop_na() |> 
  distinct() |> 
  map_df(reshuffle) |> 
  mutate(lob_num = row_number(),
         lob_node = paste0('LOB-', lob_num)) |> 
  rename(line_of_business = `Level 2`) |> 
  select(lob_node, line_of_business)


inv_clean <- 
  inv_raw |> 
  select(tag = Tag, 
         vehicle_type = `Vehicle Type`, 
         fuel_type = `Fuel Type`,
         garage_state = `Garage State`,
         exp_org = `Exp Org`,
         region = Region,
         line_of_business = `Level 2`,
         fsr_email = `FSR Email`) |> 
  mutate(ownership = if_else(str_detect(tag, "DOT"), 
                             "owned",
                             "leased")) |> 
  left_join(fsr_list, by = "fsr_email") |> 
  left_join(tag_list, by = "tag") |>
  left_join(exp_org_list, by = "exp_org") |>
  left_join(lob_list, by = "line_of_business") |> 
  select(-c(fsr_email, tag, exp_org, line_of_business)) |> 
  select(tag_node, everything())

# write clean inv
write_csv(inv_clean, "./data/inventory_clean.csv")
