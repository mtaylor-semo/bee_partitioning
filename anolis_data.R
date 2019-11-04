# Quick script to make a graph of Table 1 from
# Pacala and Roughgarden 1982 Science 217: 444.

# species, mean, sd, N, island

library(tidyverse)
library(patchwork)

species <- c("A. gingivinis", "A. wattsi pogus", "A. bimaculatus", "A. wattsi schwartzi")
svl <- c(41.31, 38.28, 53.27, 39.60)
svl_sd <- c(7.69, 3.52, 12.58, 5.31)
perch_height <- c(0.88, 0.17, 2.01, 0.16)
perch_sd <- c(0.80, 0.09, 1.71, 0.17)
N <- c(440, 538, 633, 470)
country <- c("St. Maarten", "St. Maarten", "St. Eustatius", "St. Eustatius")

anolis <- tibble(species, N, svl, svl_sd, perch_height, perch_sd, country)
anolis <- anolis %>% 
  mutate(svl_se = svl_sd/sqrt(N),
         perch_se = perch_height/sqrt(N)) %>%
  group_by(country)

#cntry <- "St. Maarten"
cntry <- "St. Eustatius"

cntry_svl <- ggplot(subset(anolis, country == cntry)) +
  geom_point(aes(x = species, y = svl), size = 2) +
#  geom_segment(aes(x = species, xend = species, yend = svl),
#               lty = 3) +
  geom_linerange(aes(x = species, ymin = svl-svl_se, ymax = svl + svl_se )) +
  theme_bw() +
  labs(x = "Species",
       y = "Snout-Vent Length (mm)") +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(panel.grid = element_blank())

cntry_perch <- ggplot(subset(anolis, country == cntry)) +
  geom_point(aes(x = species, y = perch_height), size = 2) +
  #  geom_segment(aes(x = species, xend = species, yend = svl),
  #               lty = 3) +
  geom_linerange(aes(x = species, ymin = perch_height-perch_se, ymax = perch_height + perch_se )) +
  theme_bw() +
  labs(x = "Species",
       y = "Perch Height (m)") +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(panel.grid = element_blank())

cntry_plots <- cntry_svl + cntry_perch

ggsave("st_maarten_plots.png", cntry_plots, width = 6, height = 3, units = "in")
ggsave("st_eustatius_plots.png", cntry_plots, width = 6, height = 3, units = "in")

