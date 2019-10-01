library(tidyverse)
library(ggthemes)

bees_raw <- read_csv("pyke_1982_data.csv", 
                 skip = 2,
                 na = 0,
                 col_names = c(
                   "plant_species",
                   "corolla_length",
                   "appositus",
                   "kirbyellus",
                   "flavifrons",
                   "sylvicola",
                   "bifarius",
                   "frigidus",
                   "occidentalis"
                 ))
bees <- 
  bees_raw %>% 
  gather(key = "bombus_species",
         value = visits,
         appositus,
         kirbyellus,
         flavifrons,
         sylvicola,
         bifarius,
         frigidus,
         occidentalis) %>% 
  mutate(bee_group = case_when(
    bombus_species == "appositus" | 
      bombus_species == "kirbyellus" ~ "Group 1",
    bombus_species == "flavifrons" ~ "Group 2",
    bombus_species == "sylvicola" |
      bombus_species == "bifarius" |
      bombus_species == "frigidus" ~ "Group 3",
    TRUE ~ "Group 4")) %>% 
  mutate(bee_group = factor(bee_group,
                            levels = c("Group 1", "Group 2", "Group 3", "Group 4"),
                            ordered = TRUE))

bees <- bees %>% 
  mutate(corolla_group = case_when(
    corolla_length < 4.0 ~ "0",
    corolla_length >= 4.0 &
      corolla_length < 8.0 ~ "4",
    corolla_length > 8.0 &
      corolla_length < 12.0 ~ "8",
    TRUE ~ "12")) %>% 
  mutate(corolla_group = factor(corolla_group,
                                levels = c("0", "4", "8", "12",
                                           ordered = TRUE)))

bees_small <- bees %>% 
  filter(bee_group == "Group 1" |
         bombus_species == "bifarius" |
           bombus_species == "frigidus")


bee_sums <- bees_small %>% 
  group_by(bombus_species, corolla_group) %>% 
  summarize(total_visits = sum(visits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(bombus_species = factor(bombus_species,
                                 levels = c(
                                   "appositus",
                                   "kirbyellus",
                                   "bifarius",
                                   "frigidus"
                                 ),
                                 ordered = TRUE))

bee_sums %>% 
  ggplot() +
  geom_col(aes(x = corolla_group, y = total_visits)) +
  facet_grid(rows = vars(bombus_species)) +
  theme_few() +
  labs(x = "Corolla Size Gorup (mm, minimum)",
       y = "Total Visits")

  

