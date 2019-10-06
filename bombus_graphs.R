library(tidyverse)
library(patchwork)

## Script to create blank and answer key graphs
## of the different data.


# Functions ---------------------------------------------------------------

save_plot <- function(save_name = "plot.png",
                      plot_name = last_plot(), 
                      width = 6,
                      height = 6,
                      units = "in") {
  ggsave(filename = save_name, 
         plot = plot_name, 
         width = width,
         height = height,
         units = units)
}
  


# Shared constants --------------------------------------------------------

# Use these for labeling factors so that the
# axis labels and legends have proper scientific names.

default_font <- "Linux Libertine O" # Plats get same font as text.

bombus_species_alphabetical <- c("B. appositus", 
                                "B. bifarius", 
                                "B. frigidus", 
                                "B. kirbiellus", 
                                "B. sylvicola")

bombus_species_size <- c("B. appositus", 
                        "B. kirbiellus", 
                        "B. bifarius", 
                        "B. frigidus", 
                        "B. sylvicola")


###########################################################################
## Begin plots
##

# Queen proboscis lengths -------------------------------------------------
#
# First plot of exercise to show mean and standard deviation 
# for five species of Bombus. Standard error is too small to
# be practical for graphing.

## Read in data to plot
bombus <- read_csv("proboscis_lengths.csv") %>% 
  gather(key = species, 
         value = length)


bomb <- bombus %>% 
  group_by(species) %>% 
  summarise(mean = mean(length),
            stdev = sd(length)) %>% 
  #  add_row(species = "example", mean = 10.0, stdev = 0.3) %>% 
  mutate(species = factor(species, 
                          levels = c(#"example", 
                            "appositus", 
                            "bifarius", 
                            "frigidus",
                            "kirbiellus",
                            "sylvicola"),
                          ordered = TRUE))

example_df <- tibble(species = "example", 
                     mean = 10.2, 
                     stdev = 0.4)

Alpha = 0 #0 for blank plot, 1 for full plot

bombus_plot <- 
  bomb %>% 
  ggplot() +
  geom_linerange(aes(x = species,
                     ymin = mean - stdev,
                     ymax = mean + stdev),
                 alpha = Alpha) + 
  geom_point(aes(x = species,
                 y = mean),
             alpha = Alpha) +
  labs(y = "Mean (mm)",
       x = NULL) +
  theme_bw() +
  scale_x_discrete(labels = bombus_species_alphabetical) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(text = element_text(family = default_font)) +
  scale_y_continuous(limits = c(6,14),
                     minor_breaks = seq(6, 14, 0.1), 
                     breaks = seq(6,14,1),
                     expand = c(0.01,0.01))

example_plot <- 
  example_df %>% 
  ggplot() +
  geom_linerange(aes(x = species,
                     ymin = mean - stdev,
                     ymax = mean + stdev)) + 
  geom_point(data = example_df,
             aes(x = species,
                 y = mean)) +
  labs(y = NULL,
       x = NULL) +
  theme_bw() +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(text = element_text(family = default_font)) +
  scale_y_continuous(limits = c(6,14),
                     minor_breaks = seq(6, 14, 0.1), 
                     breaks = seq(6,14,1),
                     expand = c(0.01,0.01))

final_plot <- bombus_plot + example_plot + plot_layout(nrow = 1, widths = c(5,1))


save_file <- ifelse(Alpha == 0,
                    "mean_proboscis_plot_blank.png", # Alpha = 0
                    "mean_proboscis_plot_key.png")   # Alpha = 1

save_plot(save_name = save_file, 
          plot_name = final_plot,
          width = 6,
          height = 3)


# Proboscis vs corolla length ---------------------------------------------
#
# Scatterplot of proboscis v corolla length.


pc_length <- read_csv("proboscis_corolla_lengths.csv")

Alpha = 1 #0 for blank plot, 1 for full plot

proboscis_corolla_plot <- pc_length %>% 
  ggplot(aes(x = proboscis,
             y = corolla)) +
  geom_point(alpha = Alpha) +
#  geom_smooth(method = "lm",
#              se = FALSE,
#              col = "gray50") +
  theme_bw() +
  labs(x = "Mean proboscis length (mm)",
       y = "Mean corolla length (mm)") +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(text = element_text(family = default_font)) +
  scale_y_continuous(limits = c(3,15),
                     minor_breaks = seq(3, 15, 0.1), 
                     breaks = seq(3, 15, 1),
                     expand = c(0.02, 0.02)) +
  scale_x_continuous(limits = c(5, 13),
                     minor_breaks = seq(5, 13, 0.1), 
                     breaks = seq(5, 13, 1),
                     expand = c(0.01, 0.01)) 

save_file <- ifelse(Alpha == 0,
                    "proboscis_corolla_blank.png", # Alpha = 0
                    "proboscis_corolla_key.png")   # Alpha = 1

save_plot(save_name = save_file, 
          plot_name = proboscis_corolla_plot,
          width = 6,
          height = 3)



# Bombus flower visits ----------------------------------------------------
# 
# Make column charts of number of visits by each species to 
# each size class of corollas. 

bees_raw <- read_csv("bombus_flower_visits.csv", 
                     skip = 2,
                     col_names = c(
                       "plant_species",
                       "corolla_length",
                       "appositus",
                       "kirbiellus",
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
         kirbiellus,
         bifarius,
         frigidus,
         sylvicola) %>% 
  select(-flavifrons, -occidentalis) %>% 
  mutate(bee_group = case_when(
    bombus_species == "appositus" | 
      bombus_species == "kirbiellus" ~ "Group 1",
    TRUE ~ "Group 2")) %>% 
  mutate(bee_group = factor(bee_group,
                            levels = c("Group 1", "Group 2"),
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

bee_sums <- bees %>% 
  group_by(bombus_species, corolla_group) %>% 
  summarize(total_visits = sum(visits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(bombus_species = factor(bombus_species,
                                 levels = c(
                                   "appositus",
                                   "kirbiellus",
                                   "bifarius",
                                   "frigidus",
                                   "sylvicola"
                                 ),
                                 labels = bombus_species_size,
                                 ordered = TRUE))

Alpha = 1 #0 for blank plot, 1 for full plot

flower_visits <- 
  bee_sums %>% 
  ggplot() +
  geom_col(aes(x = corolla_group, y = total_visits),
           alpha = Alpha) +
  facet_grid(rows = vars(bombus_species)) +
  theme_bw() +
  labs(x = "Corolla size group (mm, minimum)",
       y = "Total visits") +
  theme(strip.text.y = element_text(face = "italic")) +
  theme(text = element_text(family = default_font))

save_file <- ifelse(Alpha == 0,
                    "flower_visits_blank.png", # Alpha = 0
                    "flower_visits_key.png")   # Alpha = 1

save_plot(save_name = save_file, 
          plot_name = flower_visits,
          width = 4,
          height = 7)





# Gothic Transect altitude plots ------------------------------------------
#
# Plot relative abundance of each species within a proboscis length
# class along the altitudinal transect.

gothic <-  read_csv("gothic_transect.csv",
                    skip = 1,
                    col_names = c(
                      "site_number",
                      "appositus",
                      "kirbiellus",
                      "bifarius",
                      "frigidus",
                      "sylvicola"
                    )) %>% 
  gather(key = species, 
         value = relative_abundance, 
         appositus,
         kirbiellus,
         bifarius,
         frigidus,
         sylvicola)

## Long proboscis group
gothic_long <- gothic %>% 
  filter(species == "appositus" |
           species == "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("appositus", "kirbiellus"),
                          labels = c("B. appositus", "B. kirbiellus"),
                          ordered = TRUE))

## Short proboscis group
gothic_short <- gothic %>% 
  filter(species != "appositus" &
           species != "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("bifarius", "frigidus", "sylvicola"),
                          labels = c("B. bifarius", "B. frigidus", "B. sylvicola"),
                          ordered = TRUE))

Alpha = 0 #0 for blank plot, 1 for full plot

## Washington transect, long proboscis
gl_plot <- gothic_long %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = NULL,
       y = "Relative abundance",
       title = "Long proboscis",
       lty = NULL) +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(text = element_text(family = default_font)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8,
                                   face = "italic")) +
  guides(lty = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 17, 1),
                     expand = c(0.01, 0.01))

## Washington transect, short proboscis
gs_plot <- gothic_short %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = "Site number",
       y = NULL,
       title = "Short proboscis",
       lty = NULL) +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(text = element_text(family = default_font)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8, 
                                   face = "italic")) +
  guides(lty = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 17, 1),
                     expand = c(0.01, 0.01))

gothic_plot <- gl_plot + gs_plot + plot_layout(ncol = 1)

save_file <- ifelse(Alpha == 0, 
                    "gothic_relative_abundance_blank.png", # Alpha = 0
                    "gothic_relative_abundance_key.png")   # Alpha = 1

save_plot(save_name = save_file, 
          plot_name = gothic_plot,
          width = 4,
          height = 7)


# Gothic elevation profile ------------------------------------------------
#
# Elevations from Fig. 3. Maximum values used when a range was given.

elevations <- c(2873, # Site 1
                2867, # Site 2
                2885, # Site 3
                2915, # Site 4
                2924, # Site 5
                2930, # Site 6
                2939, # Site 7
                2958, # Site 8
                2961, # Site 9
                2985, # Site 10
                3000, # Site 11
                3076, # Site 12
                3145, # Site 13
                3242, # Site 14
                3333, # Site 15
                3485, # Site 16
                3697) # Site 17

site_numbers <- seq(1, 17, 1)

gothic_elevations <- tibble(site_number = site_numbers, elevation = elevations)

gothic_plot <- gothic_elevations %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = elevations)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 17, 2)) +
  scale_y_continuous(limits = c(2800, 3700),
                     breaks = seq(2800, 3700, 100)) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(family = default_font,
                            size = 8)) +
  labs(x = "Site",
       y = "Elevation (m)")

save_plot("gothic_transect.png", 
          gothic_plot,
          width = 3, height = 2.3)


##
## Not used for now.
##
# Washington / Schofield Transect altitude plots --------------------------
#
# Plot relative abundance of each species within a proboscis length
# class along the altitudinal transects.

wash_tran <- read_csv("washington_transect.csv",
                      skip = 1,
                      col_names = c(
                        "site_number",
                        "appositus",
                        "kirbiellus",
                        "bifarius",
                        "frigidus",
                        "sylvicola"
                      )) %>% 
  gather(key = species, 
         value = relative_abundance, 
         appositus,
         kirbiellus,
         bifarius,
         frigidus,
         sylvicola)

scho_tran <- read_csv("schofield_transect.csv",
                      skip = 1,
                      col_names = c(
                        "site_number",
                        "appositus",
                        "kirbiellus",
                        "bifarius",
                        "frigidus",
                        "sylvicola"
                      )) %>% 
  gather(key = species, 
         value = relative_abundance, 
         appositus,
         kirbiellus,
         bifarius,
         frigidus,
         sylvicola)

## Long proboscis group
wash_long <- wash_tran %>% 
  filter(species == "appositus" |
           species == "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("appositus", "kirbiellus"),
                          labels = c("B. appositus", "B. kirbiellus"),
                          ordered = TRUE))

## Short proboscis group
wash_short <- wash_tran %>% 
  filter(species != "appositus" &
           species != "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("bifarius", "frigidus", "sylvicola"),
                          labels = c("B. bifarius", "B. frigidus", "B. sylvicola"),
                          ordered = TRUE))

## Long proboscis group
scho_long <- scho_tran %>% 
  filter(species == "appositus" |
           species == "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("appositus", "kirbiellus"),
                          labels = c("B. appositus", "B. kirbiellus"),
                          ordered = TRUE))

# Short proboscis group
scho_short <- scho_tran %>% 
  filter(species != "appositus" &
           species != "kirbiellus") %>% 
  mutate(species = factor(species,
                          levels = c("bifarius", "frigidus", "sylvicola"),
                          labels = c("B. bifarius", "B. frigidus", "B. sylvicola"),
                          ordered = TRUE))


Alpha = 0 #0 for blank plot, 1 for full plot

## Washington transect, long proboscis
wl_plot <- wash_long %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = NULL,
       y = "Relative abundance",
       title = "Washington Transect") +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2)) +
  theme(panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 8, 1))
  
## Washington transect, short proboscis
ws_plot <- wash_short %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = "Site number",
       y = "Relative abundance") +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2)) +
  theme(panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 8, 1))


## Schofield transect, long proboscis
sl_plot <- scho_long %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       lty = "Long proboscis",
       title = "Schofield Transect") +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2)) +
  theme(panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  guides(lty = guide_legend(label.theme = element_text(face = "italic",
                                                       size = 9))) +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 6, 1))

## Washington transect, short proboscis
ss_plot <- scho_short %>% 
  ggplot() +
  geom_line(aes(x = site_number,
                y = relative_abundance,
                lty = species),
            alpha = Alpha) +
  theme_bw() +
  labs(x = "Site number",
       y = NULL,
       lty = "Short proboscis") +
  theme(panel.grid.minor.y = element_line(color = "gray90", size = 0.2)) +
  theme(panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  guides(lty = guide_legend(label.theme = element_text(face = "italic",
                                                       size = 9))) +
  scale_y_continuous(limits = c(0, 1),
                     minor_breaks = seq(0, 1, 0.01), 
                     breaks = seq(0, 1, 0.1),
                     expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(1, 6, 1))


transect_plots <- wl_plot + sl_plot + ws_plot + ss_plot + plot_layout(ncol = 2)

save_file <- ifelse(Alpha == 0, 
                    "transect_relative_abundance_blank.png", # Alpha = 0
                    "transect_relative_abundance_key.png")   # Alpha = 1


save_plot(save_name = save_file, 
          plot_name = transect_plots,
          width = 6,
          height = 5)






